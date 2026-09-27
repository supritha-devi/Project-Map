# ==============================
# app.R - Main entry point
# ==============================

required_packages <- c(
  "shiny", "shinyjs", "DBI", "RSQLite",
  "dplyr", "openssl", "qrcode", "jsonlite"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Optional packages - only needed if you want Word/Excel/PDF export.
# The app runs fine without them; those specific export buttons will
# show a friendly message instead of failing.
optional_packages <- c("openxlsx", "officer", "pagedown", "rmarkdown")
for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Optional package '", pkg, "' not installed - related export option will be disabled.")
  }
}

source("global.R")
source("totp.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
