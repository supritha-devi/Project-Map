# ==============================
# ui.R
# ==============================

ui <- fluidPage(
  useShinyjs(),
  title = "Project City",

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css")
  ),

  # ---------------- AUTH PAGE ----------------
  div(id = "auth_page", class = "auth-wrapper",
      div(class = "auth-card",
          h2("Project City", class = "auth-title"),
          p("Your project as a living city map", class = "auth-subtitle"),

          radioButtons("auth_mode", NULL,
                       choices = c("Login" = "login", "Create Account" = "register"),
                       inline = TRUE, selected = "login"),

          textInput("username", "Username", placeholder = "Enter username"),
          passwordInput("password", "Password", placeholder = "Enter password"),

          div(id = "register_extra", style = "display:none;",
              textInput("full_name", "Full Name"),
              textInput("email", "Email"),
              passwordInput("password_confirm", "Confirm Password")
          ),

          # 2FA code step, shown only if the account has 2FA enabled
          div(id = "totp_extra", style = "display:none;",
              textInput("totp_code", "6-digit authenticator code")
          ),

          br(),
          actionButton("auth_btn", "Continue", class = "btn-caramel btn-lg w-100"),
          br(), br(),
          textOutput("auth_message")
      )
  ),

  # ---------------- MAIN APP ----------------
  shinyjs::hidden(
    div(id = "main_app",
        div(class = "topbar",
            div(class = "brand", span(class = "dot"), "Project City"),
            div(class = "navlinks",
                actionLink("nav_dashboard", "Dashboard", class = "navlink"),
                actionLink("nav_map",       "Map",       class = "navlink"),
                actionLink("nav_activity",  "Activity",  class = "navlink"),
                actionLink("nav_export",    "Export",    class = "navlink"),
                actionLink("nav_settings",  "Settings",  class = "navlink")
            ),
            div(class = "project-switch-wrap",
                selectInput("project_switch", NULL, choices = NULL, width = "220px"),
                actionButton("logout_btn", "Logout", class = "btn-outline-caramel btn-sm")
            )
        ),
        div(class = "page-container",
            uiOutput("page_body")
        )
    )
  )
)
