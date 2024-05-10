 disabled_modalUI <- function(id) {
  ns <- NS(id)
  tagList(
   # Create a modal dialog if user is disabled
   modalDialog(
    title = tags$strong("YOU ARE DISABLED"),
    p("Kindly contact the Administrator!"),
    "Status: Disabled", 
    br(),
    "Role: None",
    easyClose = TRUE,
    footer = tagList(
    actionButton(
     inputId = ns("refresh"),
     label = "Refresh",
     icon = icon("rotate-right")
    )
    )
   )
  )
}

disabled_modalServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
     # Import Mysql data
     user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
     
     # Get the logged-in user's UID
     logged_in_uid <- session$userData$user()$user_uid
     
     # Check if logged user exists
     user_exists <- user$data[user_uid == logged_in_uid, .N > 0]
     
     # Extract the user details
     user_uid <- session$userData$user()$user_uid
     email <- session$userData$user()$email
     last_login <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
     
     # Check status to determine if user is enabled
     user_status <- user$data[user_uid == logged_in_uid, user_status]
     admin_check <- session$userData$user()$is_admin
     
     # Pass user_status and admin_check to JavaScript
     output$user_status <- renderText({
      paste(user_status)
     })
     
     # calling an handler
     golem::invoke_js(
      "userStatus",
      list(
       status = user_status
      )  
     )
     # reload app to check if status has changed
     observeEvent(input$refresh, {
      session$reload()
      # show a success notification
      Sys.sleep(3)
      show_toast(
       type = "success",
       position = "top-end",
       title = "Status check success",
       text = "Status refreshed now!",
       session = session
      )
     })
    }
  )
}

