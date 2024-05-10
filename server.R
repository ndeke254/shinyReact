server <- function(input, output, session) {

  # toggle modes between light and dark
 toggle_dark_mode(mode = "light", session = session)
 
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
 
# Logic to switch between the two pages
 switch_page <- function(user_status, admin_check) {
 if (user_status == "Disabled") {
 showModal(
  disabled_modalUI("disabled_user")
  )
 } else {
  if(admin_check == "FALSE") {
   insertTab(
    inputId = "nav",
    nav_panel(
     title = "Content",
     value = "standard",
     icon = icon("chart-simple"),
     app_contentUI("standard_user")
    ),
    target = "wait",
    select = TRUE,
    session = session
   )
  } else {
   insertTab(
    inputId = "nav",
    nav_panel(
     title = "Content",
     value = "standard",
     icon = icon("chart-simple"),
     app_contentUI("standard_user")
    ),
    target = "wait",
    select = TRUE,
    session = session
   )
   insertTab(
    inputId = "nav",
    nav_panel(
     title = "Administrator",
     value = "admin",
     icon = icon("user-tie"),
     login_enabledUI("logged_enabled")
    ),
    target = "standard",
    session = session
   )
  }
 }
 }
 
 waiting_pageServer("waiting_user")
 login_enabledServer("logged_enabled")
 user_profile_server("user_profile")
 app_contentServer("standard_user")
 disabled_modalServer("disabled_user")
 #call switch page function
 switch_page(user_status = user_status, admin_check = admin_check)
 
 # Record UID if new user 
 if (user_exists) {
  status <- "ON"
  
  # Write the query
  update_query <- sprintf("UPDATE user_details SET logged_status = '%s' WHERE user_uid = '%s'",
                          status, user_uid)
  DBI::dbSendQuery(con, update_query)
 } else {
  user_status <- "Disabled"
  logged_status <- "ON"
  
  # Write the query
  insert_query <- paste0("INSERT INTO user_details VALUES('", user_uid, "','", email, "','", user_status, "', '", logged_status, "','", last_login, "')")
  
  DBI::dbSendQuery(con, insert_query)
  
  # Reload data
  user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
 }
 
 # Initialize reactive values to track session start time and user ID
 rv <- reactiveValues(
  startTime = Sys.time(),
  userId = NULL
 )
 observeEvent(session$userData$user(), {
  rv$userId <- session$userData$user()$user_uid
 })
 
 # Logout when tab is closed
 observe({
  if (!is.null(rv$userId)) {
   print(rv$startTime)
   
   sessionTimeout <- 120 # 2 minutes
   
   elapsedTime <- difftime(Sys.time(), rv$startTime, units = "secs")
   print(elapsedTime)
   if (elapsedTime >= sessionTimeout) {
    
    sign_out_from_shiny()
    session$reload()
    
   # update DB on user status
   status <- "OFF"
   update_query <- sprintf("UPDATE user_details
                           SET logged_status = '%s' 
                           WHERE user_uid = '%s'", 
                           status, session$userData$user()$user_uid)
   DBI::dbSendQuery(con, update_query)
   }
  }
 })
 
 }

secure_server(server)
