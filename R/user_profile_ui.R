# Define UI for the user profile module with icon
user_profile_ui <- function(id, other_lis = NULL) {
 ns <- NS(id)
 # Add other list items if needed
 if (!is.null(other_lis)) other_lis
 nav_menu(
  title = icon("user"),
  align = "right",
  nav_item(
   textOutput(ns("user_info")),
   hr()
   ),
  nav_item(
   actionButton(
    ns("edit_profile"),
    "Edit profile", 
    icon = icon("user-pen"),
    width = "236px"
   )
  ),
  nav_item(
   actionButton(
    ns("user_settings"),
    "Settings", 
    icon = icon("gear"),
    width = "236px"
   )
  ),
  nav_item(
   actionButton(
    ns("sign_out"), 
    "Sign out", 
    icon = icon("power-off"),
    width = "236px"
   )
  )
 )
}

user_profile_server <- function(id) {
 moduleServer(
  id,
  function(input, output, session) {
   # Show the logged-in user email
   output$user_info <- renderText({
    session$userData$user()$email
   })
   
   # Actions for logout button
   observeEvent(input$sign_out, {
    # Get user id
    get_user_uid <- session$userData$user()$user_uid
    status <- "OFF"
    
    # Write the query
    update_query <- sprintf("UPDATE user_details SET logged_status = '%s' WHERE user_uid = '%s'", status, get_user_uid)
    DBI::dbSendQuery(con, update_query)
    
    # Reload data
    user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
    
    # Sign user out
    sign_out_from_shiny()
    session$reload()
   })
  }
 )
}
