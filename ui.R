ui <- navbarPage(
 title = "New York City Flights 2013",
 theme = bs_theme(version = 5, preset = "bootstrap"),
 selected = NULL,
 fluid = TRUE,
 id = "nav",
 windowTitle = tags$head(
  tags$link(
   rel = "icon",
   type = "image/png",
   href = "logo.png"
  ),
  tags$title("NYC Flights")
 ),
 nav_panel_hidden(
  useShinyjs(),
  includeCSS(
   path = "www/styles.css"
  ),
  includeScript(
   path = "www/script.js"
  ),
  value = "wait",
  waiting_pageUI("waiting_user")
 ),
 nav_spacer(),
 nav_item( input_dark_mode(id = "mode", mode = "light")),
 user_profile_ui("user_profile")
)

secure_ui(ui)

# Secure your UI behind your custom sign in page
secure_ui(
 ui,
 sign_in_page_ui = my_custom_sign_in_page
)
