# ==============================
# global.R
# ==============================

library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(dplyr)
library(openssl)
library(qrcode)
library(jsonlite)

has_openxlsx  <- requireNamespace("openxlsx", quietly = TRUE)
has_officer   <- requireNamespace("officer", quietly = TRUE)
has_pagedown  <- requireNamespace("pagedown", quietly = TRUE)
has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)

db_path <- "data/citymap.db"

init_db <- function() {
  if (!dir.exists("data")) dir.create("data")
  if (!dir.exists("data/backups")) dir.create("data/backups")
  if (!dir.exists("uploads")) dir.create("uploads")

  conn <- dbConnect(SQLite(), db_path)

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      full_name TEXT,
      email TEXT,
      profile_image TEXT,
      totp_secret TEXT,
      totp_enabled INTEGER DEFAULT 0,
      dark_mode INTEGER DEFAULT 0,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS projects (
      project_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      description TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS districts (
      district_id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      description TEXT,
      status TEXT DEFAULT 'Not Started',
      due_date TEXT,
      pos_x REAL,
      pos_y REAL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS checklist_items (
      item_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_id INTEGER NOT NULL,
      text TEXT NOT NULL,
      is_done INTEGER DEFAULT 0,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS notes (
      note_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_id INTEGER NOT NULL,
      tag TEXT DEFAULT 'General',
      content TEXT NOT NULL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS resources (
      resource_id INTEGER PRIMARY KEY AUTOINCREMENT,
      district_id INTEGER NOT NULL,
      type TEXT NOT NULL,
      title TEXT,
      path_or_url TEXT NOT NULL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS history (
      history_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      project_id INTEGER,
      district_id INTEGER,
      action TEXT,
      details TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbDisconnect(conn)
}

init_db()

backup_db <- function() {
  if (file.exists(db_path)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file.copy(db_path, file.path("data/backups", paste0("citymap_", stamp, ".db")), overwrite = TRUE)
    backups <- list.files("data/backups", full.names = TRUE)
    if (length(backups) > 3) {
      backups <- backups[order(file.info(backups)$mtime)]
      file.remove(backups[seq_len(length(backups) - 3)])
    }
  }
}
backup_db()

# Serve uploaded attachment files
addResourcePath("uploads", "uploads")

hash_password <- function(password) {
  salt <- openssl::rand_bytes(16)
  hash <- openssl::sha256(charToRaw(password), key = salt)
  paste0(openssl::base64_encode(salt), ":", openssl::base64_encode(hash))
}

verify_password <- function(password, stored) {
  parts <- strsplit(stored, ":")[[1]]
  if (length(parts) != 2) return(FALSE)
  salt <- openssl::base64_decode(parts[1])
  stored_hash <- openssl::base64_decode(parts[2])
  computed <- openssl::sha256(charToRaw(password), key = salt)
  identical(as.vector(stored_hash), as.vector(computed))
}

log_action <- function(conn, user_id, project_id = NA, district_id = NA, action, details = "") {
  dbExecute(conn,
    "INSERT INTO history (user_id, project_id, district_id, action, details) VALUES (?, ?, ?, ?, ?)",
    params = list(user_id, project_id, district_id, action, details))
}

# ---- Status badge colors (map-level, qualitative palette) ----
status_colors <- list(
  "Not Started" = "#E4E2DD",
  "In Progress" = "#B8CCE0",
  "Done"        = "#B7D0C2",
  "Overdue"     = "#E8B4A0"
)

# ---- Notes tag colors (Vintage Matcha & Earthy Neutrals - notes only) ----
tag_colors <- list(
  "General"             = "#EDE3D6",
  "Deadline Flag"       = "#E3A89F",
  "Subtopic / Planning" = "#B9C4A6",
  "Action Item"         = "#8A9A74",
  "Archived"            = "#D6D2C4"
)

# ---- Automatic icon badges: scan note content for dates / code ----
note_icons <- function(text) {
  icons <- c()
  if (grepl("\\d{1,2}[/-]\\d{1,2}([/-]\\d{2,4})?", text) ||
      grepl("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\b", text, ignore.case = TRUE)) {
    icons <- c(icons, "\U0001F4C5")  # calendar
  }
  if (grepl("```", text) || grepl("<-|function\\s*\\(|\\bdef\\s|\\{[^}]*\\}", text)) {
    icons <- c(icons, "\U0001F4BB")  # laptop
  }
  paste(icons, collapse = " ")
}

# ---- District status derived from due date, used for map badges ----
compute_display_status <- function(status, due_date) {
  if (!is.null(due_date) && !is.na(due_date) && nzchar(due_date) && status != "Done") {
    d <- tryCatch(as.Date(due_date), error = function(e) NA)
    if (!is.na(d) && d < Sys.Date()) return("Overdue")
  }
  status
}
