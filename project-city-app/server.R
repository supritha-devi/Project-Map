# ==============================
# server.R
# ==============================

server <- function(input, output, session) {

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

  rv <- reactiveValues(
    user = NULL,
    page = "dashboard",
    current_project_id = NULL,
    current_district_id = NULL,
    pending_2fa_user = NULL,
    refresh = 0,
    new_totp_secret = NULL
  )

  bump <- function() rv$refresh <- rv$refresh + 1

  # ---------- DB helpers ----------
  get_conn <- function() dbConnect(SQLite(), db_path)

  get_projects <- function(user_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM projects WHERE user_id = ? ORDER BY created_at", params = list(user_id))
  }

  get_districts <- function(project_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM districts WHERE project_id = ? ORDER BY created_at", params = list(project_id))
  }

  get_district <- function(district_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM districts WHERE district_id = ?", params = list(district_id))
  }

  get_checklist <- function(district_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM checklist_items WHERE district_id = ? ORDER BY created_at", params = list(district_id))
  }

  get_notes <- function(district_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM notes WHERE district_id = ? ORDER BY created_at DESC", params = list(district_id))
  }

  get_resources <- function(district_id) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn, "SELECT * FROM resources WHERE district_id = ? ORDER BY created_at DESC", params = list(district_id))
  }

  get_history <- function(project_id, limit = 100) {
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbGetQuery(conn,
      "SELECT * FROM history WHERE project_id = ? ORDER BY created_at DESC LIMIT ?",
      params = list(project_id, limit))
  }

  # ================= AUTH =================

  observeEvent(input$auth_mode, {
    if (input$auth_mode == "register") {
      shinyjs::show("register_extra")
      shinyjs::hide("totp_extra")
    } else {
      shinyjs::hide("register_extra")
    }
    updateActionButton(session, "auth_btn",
                        label = if (input$auth_mode == "register") "Create Account" else "Login")
  })

  observeEvent(input$auth_btn, {
    username <- trimws(input$username)
    password <- input$password

    if (username == "" || password == "") {
      output$auth_message <- renderText("Please enter username and password.")
      return()
    }

    conn <- get_conn(); on.exit(dbDisconnect(conn))

    if (input$auth_mode == "register") {
      if (password != input$password_confirm) {
        output$auth_message <- renderText("Passwords do not match.")
        return()
      }
      existing <- dbGetQuery(conn, "SELECT user_id FROM users WHERE username = ?", params = list(username))
      if (nrow(existing) > 0) {
        output$auth_message <- renderText("Username already exists.")
        return()
      }
      dbExecute(conn,
        "INSERT INTO users (username, password_hash, full_name, email) VALUES (?, ?, ?, ?)",
        params = list(username, hash_password(password), input$full_name, input$email))
      new_id <- dbGetQuery(conn, "SELECT user_id FROM users WHERE username = ?", params = list(username))$user_id
      log_action(conn, new_id, action = "Account created", details = paste("Welcome", username))
      output$auth_message <- renderText("Account created! Please log in.")
      updateRadioButtons(session, "auth_mode", selected = "login")
      return()
    }

    # ---- Login ----
    user_row <- dbGetQuery(conn, "SELECT * FROM users WHERE username = ?", params = list(username))
    if (nrow(user_row) == 0) {
      output$auth_message <- renderText("User not found.")
      return()
    }
    if (!verify_password(password, user_row$password_hash[1])) {
      output$auth_message <- renderText("Incorrect password.")
      return()
    }

    if (isTRUE(user_row$totp_enabled[1] == 1)) {
      shinyjs::show("totp_extra")
      code <- trimws(input$totp_code %||% "")
      if (identical(code, "")) {
        rv$pending_2fa_user <- user_row
        output$auth_message <- renderText("Enter your 6-digit authenticator code and click Continue again.")
        return()
      }
      if (!verify_totp(user_row$totp_secret[1], code)) {
        output$auth_message <- renderText("Invalid authenticator code.")
        return()
      }
    }

    # ---- Successful login ----
    rv$user <- user_row
    rv$pending_2fa_user <- NULL
    log_action(conn, user_row$user_id[1], action = "Login", details = "User logged in")

    if (isTRUE(user_row$dark_mode[1] == 1)) shinyjs::addClass(selector = "body", class = "dark-mode")

    projects <- get_projects(user_row$user_id[1])
    if (nrow(projects) > 0) {
      choices <- setNames(as.character(projects$project_id), projects$name)
      updateSelectInput(session, "project_switch", choices = choices, selected = choices[1])
      rv$current_project_id <- as.numeric(choices[1])
    } else {
      updateSelectInput(session, "project_switch", choices = c("No projects yet" = ""))
      rv$current_project_id <- NULL
    }

    rv$page <- "dashboard"
    shinyjs::hide("auth_page")
    shinyjs::show("main_app")
  })

  observeEvent(input$logout_btn, {
    rv$user <- NULL
    rv$current_project_id <- NULL
    rv$current_district_id <- NULL
    rv$page <- "dashboard"
    shinyjs::removeClass(selector = "body", class = "dark-mode")
    shinyjs::hide("main_app")
    shinyjs::show("auth_page")
    shinyjs::hide("totp_extra")
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    output$auth_message <- renderText("")
  })

  # ---------------- NAV ----------------
  observeEvent(input$nav_dashboard, { rv$page <- "dashboard" })
  observeEvent(input$nav_map,       { rv$page <- "map" })
  observeEvent(input$nav_activity,  { rv$page <- "activity" })
  observeEvent(input$nav_export,    { rv$page <- "export" })
  observeEvent(input$nav_settings,  { rv$page <- "settings" })

  observeEvent(input$project_switch, {
    req(input$project_switch, nzchar(input$project_switch))
    rv$current_project_id <- as.numeric(input$project_switch)
    rv$page <- "dashboard"
  })

  observeEvent(input$open_district, {
    rv$current_district_id <- as.numeric(input$open_district)
    rv$page <- "district"
  })

  observeEvent(input$back_to_map, { rv$page <- "map" })

  # ================= NEW PROJECT =================
  observeEvent(input$create_project_btn, {
    req(rv$user, input$new_project_name, nzchar(trimws(input$new_project_name)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "INSERT INTO projects (user_id, name, description) VALUES (?, ?, ?)",
              params = list(rv$user$user_id[1], trimws(input$new_project_name), input$new_project_desc %||% ""))
    new_pid <- dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id
    log_action(conn, rv$user$user_id[1], project_id = new_pid, action = "Project created",
               details = input$new_project_name)

    projects <- get_projects(rv$user$user_id[1])
    choices <- setNames(as.character(projects$project_id), projects$name)
    updateSelectInput(session, "project_switch", choices = choices, selected = as.character(new_pid))
    rv$current_project_id <- new_pid
    updateTextInput(session, "new_project_name", value = "")
    updateTextAreaInput(session, "new_project_desc", value = "")
    rv$page <- "map"
    bump()
  })

  # ================= ADD DISTRICT =================
  observeEvent(input$add_district_btn, {
    req(rv$current_project_id, input$new_district_name, nzchar(trimws(input$new_district_name)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    existing_n <- nrow(get_districts(rv$current_project_id))
    pos_x <- 8 + (existing_n %% 4) * 24
    pos_y <- 10 + (existing_n %/% 4) * 30
    dbExecute(conn,
      "INSERT INTO districts (project_id, name, description, status, pos_x, pos_y) VALUES (?, ?, ?, 'Not Started', ?, ?)",
      params = list(rv$current_project_id, trimws(input$new_district_name), input$new_district_desc %||% "", pos_x, pos_y))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               action = "District created", details = input$new_district_name)
    updateTextInput(session, "new_district_name", value = "")
    updateTextAreaInput(session, "new_district_desc", value = "")
    bump()
  })

  # ================= DISTRICT DETAIL ACTIONS =================
  observeEvent(input$save_status_due, {
    req(rv$current_district_id)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn,
      "UPDATE districts SET status = ?, due_date = ?, updated_at = CURRENT_TIMESTAMP WHERE district_id = ?",
      params = list(input$edit_status, as.character(input$edit_due_date), rv$current_district_id))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               district_id = rv$current_district_id, action = "Status/due date updated")
    bump()
  })

  observeEvent(input$add_checklist_item, {
    req(rv$current_district_id, input$new_checklist_text, nzchar(trimws(input$new_checklist_text)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "INSERT INTO checklist_items (district_id, text) VALUES (?, ?)",
              params = list(rv$current_district_id, trimws(input$new_checklist_text)))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               district_id = rv$current_district_id, action = "Checklist item added",
               details = input$new_checklist_text)
    updateTextInput(session, "new_checklist_text", value = "")
    bump()
  })

  observeEvent(input$toggle_item, {
    req(input$toggle_item)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    cur <- dbGetQuery(conn, "SELECT is_done FROM checklist_items WHERE item_id = ?",
                       params = list(as.numeric(input$toggle_item)))
    new_val <- if (nrow(cur) > 0 && cur$is_done[1] == 1) 0 else 1
    dbExecute(conn, "UPDATE checklist_items SET is_done = ? WHERE item_id = ?",
              params = list(new_val, as.numeric(input$toggle_item)))
    bump()
  })

  observeEvent(input$delete_item, {
    req(input$delete_item)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "DELETE FROM checklist_items WHERE item_id = ?", params = list(as.numeric(input$delete_item)))
    bump()
  })

  observeEvent(input$add_note_btn, {
    req(rv$current_district_id, input$new_note_content, nzchar(trimws(input$new_note_content)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "INSERT INTO notes (district_id, tag, content) VALUES (?, ?, ?)",
              params = list(rv$current_district_id, input$new_note_tag, trimws(input$new_note_content)))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               district_id = rv$current_district_id, action = "Note added", details = input$new_note_tag)
    updateTextAreaInput(session, "new_note_content", value = "")
    bump()
  })

  observeEvent(input$note_filter, { bump() })

  observeEvent(input$add_link_btn, {
    req(rv$current_district_id, input$new_link_url, nzchar(trimws(input$new_link_url)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "INSERT INTO resources (district_id, type, title, path_or_url) VALUES (?, 'link', ?, ?)",
              params = list(rv$current_district_id, trimws(input$new_link_url), trimws(input$new_link_url)))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               district_id = rv$current_district_id, action = "Link added")
    updateTextInput(session, "new_link_url", value = "")
    bump()
  })

  observeEvent(input$new_file_upload, {
    req(rv$current_district_id, input$new_file_upload)
    dest_dir <- file.path("uploads", as.character(rv$current_district_id))
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
    dest_path <- file.path(dest_dir, input$new_file_upload$name)
    file.copy(input$new_file_upload$datapath, dest_path, overwrite = TRUE)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    rel_path <- file.path("uploads", as.character(rv$current_district_id), input$new_file_upload$name)
    dbExecute(conn, "INSERT INTO resources (district_id, type, title, path_or_url) VALUES (?, 'file', ?, ?)",
              params = list(rv$current_district_id, input$new_file_upload$name, rel_path))
    log_action(conn, rv$user$user_id[1], project_id = rv$current_project_id,
               district_id = rv$current_district_id, action = "File uploaded",
               details = input$new_file_upload$name)
    bump()
  })

  # ================= SETTINGS =================
  observeEvent(input$save_profile_btn, {
    req(rv$user)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "UPDATE users SET full_name = ?, email = ? WHERE user_id = ?",
              params = list(input$settings_full_name, input$settings_email, rv$user$user_id[1]))
    rv$user <- dbGetQuery(conn, "SELECT * FROM users WHERE user_id = ?", params = list(rv$user$user_id[1]))
    showNotification("Profile saved.", type = "message")
  })

  observeEvent(input$dark_mode_toggle, {
    req(rv$user)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    val <- if (isTRUE(input$dark_mode_toggle)) 1 else 0
    dbExecute(conn, "UPDATE users SET dark_mode = ? WHERE user_id = ?", params = list(val, rv$user$user_id[1]))
    if (val == 1) shinyjs::addClass(selector = "body", class = "dark-mode")
    else shinyjs::removeClass(selector = "body", class = "dark-mode")
  })

  observeEvent(input$enable_2fa_btn, {
    req(rv$user)
    secret <- generate_totp_secret()
    rv$new_totp_secret <- secret
    output$totp_qr <- renderPlot({
      uri <- totp_uri(secret, rv$user$username[1])
      qr <- qrcode::qr_code(uri)
      plot(qr)
    })
    output$totp_secret_text <- renderText(secret)
  })

  observeEvent(input$confirm_2fa_btn, {
    req(rv$user, rv$new_totp_secret, input$confirm_2fa_code)
    if (verify_totp(rv$new_totp_secret, input$confirm_2fa_code)) {
      conn <- get_conn(); on.exit(dbDisconnect(conn))
      dbExecute(conn, "UPDATE users SET totp_secret = ?, totp_enabled = 1 WHERE user_id = ?",
                params = list(rv$new_totp_secret, rv$user$user_id[1]))
      rv$user <- dbGetQuery(conn, "SELECT * FROM users WHERE user_id = ?", params = list(rv$user$user_id[1]))
      showNotification("Two-factor authentication enabled.", type = "message")
    } else {
      showNotification("Code didn't match - try scanning again.", type = "error")
    }
  })

  observeEvent(input$disable_2fa_btn, {
    req(rv$user)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "UPDATE users SET totp_secret = NULL, totp_enabled = 0 WHERE user_id = ?",
              params = list(rv$user$user_id[1]))
    rv$user <- dbGetQuery(conn, "SELECT * FROM users WHERE user_id = ?", params = list(rv$user$user_id[1]))
    showNotification("Two-factor authentication disabled.", type = "message")
  })

  observeEvent(input$rename_project_btn, {
    req(rv$current_project_id, input$rename_project_input, nzchar(trimws(input$rename_project_input)))
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dbExecute(conn, "UPDATE projects SET name = ? WHERE project_id = ?",
              params = list(trimws(input$rename_project_input), rv$current_project_id))
    projects <- get_projects(rv$user$user_id[1])
    choices <- setNames(as.character(projects$project_id), projects$name)
    updateSelectInput(session, "project_switch", choices = choices, selected = as.character(rv$current_project_id))
    showNotification("Project renamed.", type = "message")
  })

  observeEvent(input$delete_project_btn, {
    req(rv$current_project_id)
    conn <- get_conn(); on.exit(dbDisconnect(conn))
    dist_ids <- dbGetQuery(conn, "SELECT district_id FROM districts WHERE project_id = ?",
                            params = list(rv$current_project_id))$district_id
    if (length(dist_ids) > 0) {
      qs <- paste(rep("?", length(dist_ids)), collapse = ",")
      dbExecute(conn, sprintf("DELETE FROM checklist_items WHERE district_id IN (%s)", qs), params = as.list(dist_ids))
      dbExecute(conn, sprintf("DELETE FROM notes WHERE district_id IN (%s)", qs), params = as.list(dist_ids))
      dbExecute(conn, sprintf("DELETE FROM resources WHERE district_id IN (%s)", qs), params = as.list(dist_ids))
    }
    dbExecute(conn, "DELETE FROM districts WHERE project_id = ?", params = list(rv$current_project_id))
    dbExecute(conn, "DELETE FROM history WHERE project_id = ?", params = list(rv$current_project_id))
    dbExecute(conn, "DELETE FROM projects WHERE project_id = ?", params = list(rv$current_project_id))

    projects <- get_projects(rv$user$user_id[1])
    if (nrow(projects) > 0) {
      choices <- setNames(as.character(projects$project_id), projects$name)
      updateSelectInput(session, "project_switch", choices = choices, selected = choices[1])
      rv$current_project_id <- as.numeric(choices[1])
    } else {
      updateSelectInput(session, "project_switch", choices = c("No projects yet" = ""))
      rv$current_project_id <- NULL
    }
    rv$page <- "dashboard"
    showNotification("Project deleted.", type = "warning")
    bump()
  })

  # ================= PAGE RENDERERS =================

  dashboard_ui <- function() {
    if (is.null(rv$current_project_id)) {
      return(div(class = "content",
        div(class = "section",
            h2("Welcome"),
            p("You don't have any projects yet. Create your first one below."),
            textInput("new_project_name", "Project name"),
            textAreaInput("new_project_desc", "Description", rows = 2),
            actionButton("create_project_btn", "+ Create Project", class = "btn-caramel")
        )
      ))
    }

    dists <- get_districts(rv$current_project_id)
    if (nrow(dists) == 0) {
      total <- 0; done <- 0; pct <- 0
    } else {
      total <- nrow(dists)
      done <- sum(dists$status == "Done")
      pct <- round(100 * done / total)
    }

    is_overdue <- vapply(seq_len(nrow(dists)), function(i) {
      dd <- dists$due_date[i]
      if (is.na(dd) || !nzchar(dd) || dists$status[i] == "Done") return(FALSE)
      d <- tryCatch(as.Date(dd), error = function(e) NA)
      !is.na(d) && d < Sys.Date()
    }, logical(1))
    overdue <- dists[is_overdue, , drop = FALSE]

    recent <- if (nrow(dists) > 0) dists[order(dists$updated_at, decreasing = TRUE), , drop = FALSE][seq_len(min(5, nrow(dists))), ] else dists

    div(class = "content",
      div(class = "section-title", "Dashboard"),
      div(class = "section",
          h3(paste0(pct, "% complete")),
          p(paste0(done, " of ", total, " districts done")),
          if (nrow(overdue) > 0) {
            div(class = "callout-coral",
                strong(paste0(nrow(overdue), " district(s) overdue: ")),
                paste(overdue$name, collapse = ", "))
          }
      ),
      div(class = "section",
          h3("Recently updated"),
          if (nrow(recent) == 0) p("Nothing yet.") else
            tagList(lapply(seq_len(nrow(recent)), function(i) {
              div(class = "recent-row",
                  tags$a(href = "#", style = "color:var(--caramel); font-weight:600; text-decoration:none;",
                         onclick = sprintf("Shiny.setInputValue('open_district', %d, {priority: 'event'}); return false;",
                                            recent$district_id[i]),
                         recent$name[i]),
                  span(class = "badge-small", recent$status[i]))
            }))
      ),
      div(class = "section",
          h3("New project"),
          textInput("new_project_name", "Project name"),
          textAreaInput("new_project_desc", "Description", rows = 2),
          actionButton("create_project_btn", "+ Create Project", class = "btn-caramel")
      )
    )
  }

  map_ui <- function() {
    if (is.null(rv$current_project_id)) {
      return(div(class = "content", p("Create a project first from the Dashboard.")))
    }
    dists <- get_districts(rv$current_project_id)

    search_term <- trimws(input$search_box %||% "")
    if (nzchar(search_term)) {
      keep <- grepl(search_term, dists$name, ignore.case = TRUE)
      if (nrow(dists) > 0) {
        for (i in seq_len(nrow(dists))) {
          if (!keep[i]) {
            notes_i <- get_notes(dists$district_id[i])
            if (nrow(notes_i) > 0 && any(grepl(search_term, notes_i$content, ignore.case = TRUE))) keep[i] <- TRUE
          }
        }
      }
      dists <- dists[keep, , drop = FALSE]
    }

    cards <- if (nrow(dists) == 0) {
      div(class = "empty-state",
          div(class = "empty-icon", "\U0001F3D9"),
          h2("Your city is empty"),
          p("Add a district for each part of your project.")
      )
    } else {
      tagList(lapply(seq_len(nrow(dists)), function(i) {
        d <- dists[i, ]
        disp_status <- compute_display_status(d$status, d$due_date)
        col <- status_colors[[disp_status]] %||% "#E4E2DD"
        checklist_i <- get_checklist(d$district_id)
        chk_done <- sum(checklist_i$is_done == 1)
        chk_total <- nrow(checklist_i)
        div(class = "district-card",
            style = sprintf("left:%s%%; top:%s%%;", d$pos_x, d$pos_y),
            onclick = sprintf("Shiny.setInputValue('open_district', %d, {priority: 'event'})", d$district_id),
            div(class = "d-title", d$name),
            div(class = "d-snippet", substr(d$description %||% "", 1, 80)),
            div(class = "d-row",
                span(class = "badge", style = sprintf("background:%s;", col), disp_status),
                if (chk_total > 0) span(class = "checklist-tag", sprintf("%d/%d", chk_done, chk_total))
            )
        )
      }))
    }

    div(class = "content",
      div(class = "page-header",
          h1("City Map"),
          div(style = "display:flex; gap:8px;",
              textInput("search_box", NULL, value = search_term, placeholder = "Search districts...")
          )
      ),
      div(class = "map-wrap", cards),
      div(class = "section",
          h3("+ Add District"),
          textInput("new_district_name", "Name"),
          textAreaInput("new_district_desc", "Short description", rows = 2),
          actionButton("add_district_btn", "Add District", class = "btn-caramel")
      )
    )
  }

  district_detail_ui <- function() {
    req(rv$current_district_id)
    d <- get_district(rv$current_district_id)
    if (nrow(d) == 0) return(div(class = "content", p("District not found.")))

    checklist <- get_checklist(rv$current_district_id)
    notes <- get_notes(rv$current_district_id)
    resources <- get_resources(rv$current_district_id)

    filter_tag <- input$note_filter %||% "All"
    if (!identical(filter_tag, "All") && nrow(notes) > 0) {
      notes <- notes[notes$tag == filter_tag, , drop = FALSE]
    }

    chk_done <- sum(checklist$is_done == 1)
    chk_total <- nrow(checklist)

    checklist_rows <- if (chk_total == 0) tags$p("No tasks yet.") else
      tagList(lapply(seq_len(nrow(checklist)), function(i) {
        it <- checklist[i, ]
        div(class = if (it$is_done == 1) "check-item done" else "check-item",
            tags$input(type = "checkbox", checked = if (it$is_done == 1) "checked" else NULL,
                       onclick = sprintf("Shiny.setInputValue('toggle_item', %d, {priority:'event'})", it$item_id)),
            span(it$text),
            tags$span("\u00D7", class = "delete-x",
                      onclick = sprintf("Shiny.setInputValue('delete_item', %d, {priority:'event'})", it$item_id))
        )
      }))

    note_rows <- if (nrow(notes) == 0) tags$p("No notes yet.") else
      tagList(lapply(seq_len(nrow(notes)), function(i) {
        n <- notes[i, ]
        col <- tag_colors[[n$tag]] %||% "#EDE3D6"
        div(class = "note-entry", style = sprintf("border-left-color:%s;", col),
            span(class = "time", substr(n$created_at, 1, 16)),
            div(class = "tag", style = sprintf("background:%s;", col), n$tag),
            div(paste(note_icons(n$content), n$content))
        )
      }))

    resource_rows <- if (nrow(resources) == 0) tags$p("No linked resources yet.") else
      tagList(lapply(seq_len(nrow(resources)), function(i) {
        r <- resources[i, ]
        if (r$type == "file") {
          div(class = "resource-row",
              tags$a(href = paste0("/", r$path_or_url), target = "_blank", paste0("\U0001F4C4 ", r$title)))
        } else {
          div(class = "resource-row",
              tags$a(href = r$path_or_url, target = "_blank", paste0("\U0001F517 ", r$title)))
        }
      }))

    div(class = "content",
      div(class = "breadcrumb",
          actionLink("back_to_map", "\u2190 City Map"), " / ", d$name[1]),
      div(class = "header-row",
          h1(d$name[1]),
          span(class = "badge", style = sprintf("background:%s;", status_colors[[compute_display_status(d$status[1], d$due_date[1])]] %||% "#E4E2DD"),
               compute_display_status(d$status[1], d$due_date[1]))
      ),
      div(class = "section",
          h3(sprintf("Checklist (%d/%d done)", chk_done, chk_total)),
          checklist_rows,
          div(class = "add-row",
              textInput("new_checklist_text", NULL, placeholder = "+ Add a task..."),
              actionButton("add_checklist_item", "Add"))
      ),
      div(class = "section notes-panel-wrap",
          h3("Notes"),
          div(class = "notes-filter",
              selectInput("note_filter", NULL,
                          choices = c("All", names(tag_colors)), selected = filter_tag)),
          div(class = "notes-panel", note_rows),
          div(class = "add-note-row",
              textAreaInput("new_note_content", NULL, placeholder = "Write a new note..."),
              selectInput("new_note_tag", NULL, choices = names(tag_colors)),
              actionButton("add_note_btn", "Add Note", class = "btn-caramel"))
      ),
      div(class = "section",
          h3("Status & Due Date"),
          selectInput("edit_status", "Status", choices = c("Not Started", "In Progress", "Done"),
                      selected = d$status[1]),
          dateInput("edit_due_date", "Due date",
                    value = { dd_val <- d$due_date[1]; if (!is.na(dd_val) && nzchar(dd_val)) dd_val else NA }),
          actionButton("save_status_due", "Save", class = "btn-caramel")
      ),
      div(class = "section",
          h3("Linked Resources"),
          resource_rows,
          div(class = "resource-actions",
              textInput("new_link_url", NULL, placeholder = "Paste a link..."),
              actionButton("add_link_btn", "+ Add Link"),
              fileInput("new_file_upload", "+ Upload File"))
      ),
      div(class = "section",
          h3("Activity (this district)"),
          {
            conn <- get_conn(); on.exit(dbDisconnect(conn))
            hist <- dbGetQuery(conn,
              "SELECT * FROM history WHERE district_id = ? ORDER BY created_at DESC LIMIT 10",
              params = list(rv$current_district_id))
            if (nrow(hist) == 0) tags$p("No activity yet.") else
              tagList(lapply(seq_len(nrow(hist)), function(i) {
                div(class = "activity-item", hist$action[i],
                    tags$span(class = "when", substr(hist$created_at[i], 1, 16)))
              }))
          }
      )
    )
  }

  activity_ui <- function() {
    if (is.null(rv$current_project_id)) return(div(class = "content", p("No project selected.")))
    hist <- get_history(rv$current_project_id, limit = 200)
    div(class = "content",
        h1("Activity"),
        if (nrow(hist) == 0) p("No activity yet.") else
          tagList(lapply(seq_len(nrow(hist)), function(i) {
            div(class = "activity-item", paste0(hist$action[i],
                if (nzchar(hist$details[i] %||% "")) paste0(" - ", hist$details[i]) else ""),
                tags$span(class = "when", hist$created_at[i]))
          }))
    )
  }

  export_ui <- function() {
    div(class = "content",
      h1("Export"),
      div(class = "section",
          h3("Export a Report"),
          p("Includes districts, checklist progress, and notes for the current project."),
          downloadButton("export_html", "Download HTML"),
          downloadButton("export_md", "Download Markdown"),
          if (has_officer) downloadButton("export_docx", "Download Word") else
            p(class = "disabled-note", "Word export needs the 'officer' package installed."),
          if (has_pagedown) downloadButton("export_pdf", "Download PDF") else
            p(class = "disabled-note", "PDF export needs the 'pagedown' package (and Chrome) installed.")
      ),
      div(class = "section",
          h3("Export Your Data"),
          downloadButton("export_csv", "Download CSV"),
          if (has_openxlsx) downloadButton("export_xlsx", "Download Excel") else
            p(class = "disabled-note", "Excel export needs the 'openxlsx' package installed."),
          downloadButton("export_json", "Download Full JSON Backup")
      )
    )
  }

  settings_ui <- function() {
    u <- rv$user
    div(class = "content",
      h1("Settings"),
      div(class = "section",
          h3("Profile"),
          textInput("settings_full_name", "Full Name", value = u$full_name[1] %||% ""),
          textInput("settings_email", "Email", value = u$email[1] %||% ""),
          actionButton("save_profile_btn", "Save Profile", class = "btn-caramel")
      ),
      div(class = "section",
          h3("Appearance"),
          checkboxInput("dark_mode_toggle", "Dark mode", value = isTRUE(u$dark_mode[1] == 1))
      ),
      div(class = "section",
          h3("Two-Factor Authentication"),
          if (isTRUE(u$totp_enabled[1] == 1)) {
            tagList(p("2FA is currently enabled."),
                    actionButton("disable_2fa_btn", "Disable 2FA", class = "btn-outline-caramel"))
          } else {
            tagList(
              actionButton("enable_2fa_btn", "Set Up 2FA", class = "btn-caramel"),
              plotOutput("totp_qr", height = "180px", width = "180px"),
              verbatimTextOutput("totp_secret_text"),
              textInput("confirm_2fa_code", "Enter code from your authenticator app"),
              actionButton("confirm_2fa_btn", "Confirm & Enable", class = "btn-caramel")
            )
          }
      ),
      if (!is.null(rv$current_project_id)) div(class = "section",
          h3("Current Project"),
          textInput("rename_project_input", "Rename project"),
          actionButton("rename_project_btn", "Rename", class = "btn-caramel"),
          br(), br(),
          actionButton("delete_project_btn", "Delete This Project", class = "btn-outline-caramel")
      )
    )
  }

  output$page_body <- renderUI({
    rv$refresh
    req(rv$user)
    switch(rv$page,
      "dashboard" = dashboard_ui(),
      "map"       = map_ui(),
      "district"  = district_detail_ui(),
      "activity"  = activity_ui(),
      "export"    = export_ui(),
      "settings"  = settings_ui(),
      dashboard_ui()
    )
  })

  # ================= EXPORT HANDLERS =================

  export_data_bundle <- function() {
    dists <- get_districts(rv$current_project_id)
    list(
      districts = dists,
      checklist = do.call(rbind, lapply(dists$district_id, get_checklist)),
      notes     = do.call(rbind, lapply(dists$district_id, get_notes)),
      resources = do.call(rbind, lapply(dists$district_id, get_resources))
    )
  }

  output$export_csv <- downloadHandler(
    filename = function() "project_city_districts.csv",
    content = function(file) {
      dists <- get_districts(rv$current_project_id)
      write.csv(dists, file, row.names = FALSE)
    }
  )

  output$export_json <- downloadHandler(
    filename = function() "project_city_backup.json",
    content = function(file) {
      bundle <- export_data_bundle()
      writeLines(jsonlite::toJSON(bundle, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )

  if (has_openxlsx) {
    output$export_xlsx <- downloadHandler(
      filename = function() "project_city_data.xlsx",
      content = function(file) {
        bundle <- export_data_bundle()
        openxlsx::write.xlsx(list(Districts = bundle$districts,
                                   Checklist = bundle$checklist,
                                   Notes = bundle$notes,
                                   Resources = bundle$resources), file)
      }
    )
  }

  build_report_html <- function() {
    dists <- get_districts(rv$current_project_id)
    parts <- c("<html><head><meta charset='utf-8'><style>",
               "body{font-family:sans-serif;padding:30px;} h1{color:#A0522D;}",
               ".card{border-left:4px solid #C68642;padding:12px;margin-bottom:12px;background:#FBF8F2;}",
               "</style></head><body>",
               paste0("<h1>Project City Report</h1>"))
    if (nrow(dists) > 0) {
      for (i in seq_len(nrow(dists))) {
        d <- dists[i, ]
        checklist_i <- get_checklist(d$district_id)
        notes_i <- get_notes(d$district_id)
        parts <- c(parts, sprintf("<div class='card'><h2>%s</h2><p>Status: %s</p><p>%s</p>",
                                   d$name, d$status, d$description %||% ""))
        if (nrow(checklist_i) > 0) {
          parts <- c(parts, "<ul>")
          for (j in seq_len(nrow(checklist_i))) {
            mark <- if (checklist_i$is_done[j] == 1) "&#9745;" else "&#9744;"
            parts <- c(parts, sprintf("<li>%s %s</li>", mark, checklist_i$text[j]))
          }
          parts <- c(parts, "</ul>")
        }
        if (nrow(notes_i) > 0) {
          for (k in seq_len(nrow(notes_i))) {
            parts <- c(parts, sprintf("<p><em>[%s]</em> %s</p>", notes_i$tag[k], notes_i$content[k]))
          }
        }
        parts <- c(parts, "</div>")
      }
    }
    parts <- c(parts, "</body></html>")
    paste(parts, collapse = "\n")
  }

  output$export_html <- downloadHandler(
    filename = function() "project_city_report.html",
    content = function(file) writeLines(build_report_html(), file)
  )

  output$export_md <- downloadHandler(
    filename = function() "project_city_report.md",
    content = function(file) {
      dists <- get_districts(rv$current_project_id)
      lines <- c("# Project City Report", "")
      if (nrow(dists) > 0) {
        for (i in seq_len(nrow(dists))) {
          d <- dists[i, ]
          lines <- c(lines, sprintf("## %s (%s)", d$name, d$status), "", d$description %||% "", "")
          checklist_i <- get_checklist(d$district_id)
          if (nrow(checklist_i) > 0) {
            for (j in seq_len(nrow(checklist_i))) {
              mark <- if (checklist_i$is_done[j] == 1) "x" else " "
              lines <- c(lines, sprintf("- [%s] %s", mark, checklist_i$text[j]))
            }
            lines <- c(lines, "")
          }
          notes_i <- get_notes(d$district_id)
          if (nrow(notes_i) > 0) {
            for (k in seq_len(nrow(notes_i))) {
              lines <- c(lines, sprintf("> **%s:** %s", notes_i$tag[k], notes_i$content[k]), "")
            }
          }
        }
      }
      writeLines(lines, file)
    }
  )

  if (has_officer) {
    output$export_docx <- downloadHandler(
      filename = function() "project_city_report.docx",
      content = function(file) {
        dists <- get_districts(rv$current_project_id)
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Project City Report", style = "heading 1")
        if (nrow(dists) > 0) {
          for (i in seq_len(nrow(dists))) {
            d <- dists[i, ]
            doc <- officer::body_add_par(doc, sprintf("%s (%s)", d$name, d$status), style = "heading 2")
            doc <- officer::body_add_par(doc, d$description %||% "")
            checklist_i <- get_checklist(d$district_id)
            if (nrow(checklist_i) > 0) {
              for (j in seq_len(nrow(checklist_i))) {
                mark <- if (checklist_i$is_done[j] == 1) "[Done] " else "[ ] "
                doc <- officer::body_add_par(doc, paste0(mark, checklist_i$text[j]))
              }
            }
          }
        }
        print(doc, target = file)
      }
    )
  }

  if (has_pagedown) {
    output$export_pdf <- downloadHandler(
      filename = function() "project_city_report.pdf",
      content = function(file) {
        tmp_html <- tempfile(fileext = ".html")
        writeLines(build_report_html(), tmp_html)
        tryCatch({
          pagedown::chrome_print(tmp_html, output = file)
        }, error = function(e) {
          showNotification("PDF export failed - Chrome/Chromium may not be installed on this machine.",
                            type = "error", duration = 8)
        })
      }
    )
  }
}
