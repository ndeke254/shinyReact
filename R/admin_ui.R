
value_theme <- "bg-gradient-teal-purple"

# create a UI for user logged in and enabled by admin
login_enabledUI <- function(id) {
  ns <- NS(id)
  actions <- c("", "Enable user", "Disable user", "Update administrator",
               "Delete from app", "Add role to user", "Add new role to app"
               )
  tagList(
   useShinyjs(),
   includeCSS(
    path = "www/styles.css"
   ),
   card(
    card_header("General Users Statistics"),
    height = "300px",
    layout_columns(
     value_box(
      title = "Total users",
      value = textOutput(ns("users")),
      theme = value_theme,
      showcase = bs_icon("people-fill")
     ),
   value_box(
    title = "Online users",
    value = textOutput(ns("online")),
    theme = value_theme,
    showcase = bs_icon("person-fill-check")
   ),
   value_box(
    title = "Disabled users",
    value = textOutput(ns("disabled")),   
    theme = value_theme,
    showcase = bs_icon("person-fill-slash")
   ),
   value_box(
    title = "Active users",
    value = textOutput(ns("active")),
    theme = value_theme,
    showcase = bs_icon("person-workspace")
   ),
   value_box(
    title = "Current administrators",
    value = textOutput(ns("admin")),
    theme = value_theme,
    showcase = bs_icon("database-fill-lock")
   )
   )
   ),
    card(
    card_header("Action on a User"),
    height = "300px",
    layout_columns(
     selectizeInput(
   inputId = ns("users_email"),
   label = "Select an email", 
   multiple = FALSE,
   choices = NULL,
   options = list(maxOptions = 3)
   ),
  selectizeInput(
   inputId = ns("select_action"),
   label = "Select an action", 
   multiple = FALSE,
   choices = actions,
   options = list(maxOptions = 3)
  ),
  disabled(
  selectizeInput(
   inputId = ns("users_role"),
   label = "Select a role", 
   multiple = FALSE,
   choices = NULL,
   options = list(maxOptions = 3)
  )
  ),
  disabled(
  textInput(
   inputId = ns("add_new_role"),
   label = "Add new App role",
   placeholder = "Enter your new role"
  )
  ),
  disabled(
  selectInput(
   inputId = ns("isadmin"),
   label = "Set as Admin",
   choices = c("Yes", "No")
   )
  )
  ),
  card_footer(
  actionButton(
     inputId = ns("actions_button"),
     label = "Action",
     icon = icon("user-tie")
    )
  )
  ),
  card(
   card_header("Selected user info"),
   height = "200px",
   fill = TRUE,
   dataTableOutput(
    outputId =  ns("user_details")
   )
  )
  )
}

login_enabledServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session ) {
     # Query data from MySQL table into a data.table
     user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
     app_users <- get_app_users(api_key = get_api_key())$content
     app_users <- as.data.table(app_users)
     
     # populate value tables
     output$users <- renderText({
      n <- as.character(app_users[ ,.N])
      return(n)
      })
 
     output$online <- renderText({
      n <- as.character(user$data[logged_status == "ON" ,.N])
      return(n)
     })
     
     output$admin <- renderText({
      table <- as.character(app_users[is_admin == "TRUE", email])
      return(table)
     })
    
     output$disabled <- renderText({
      n <- as.character(user$data[user_status == "Disabled" ,.N])
      return(n)
     })
     output$active <- renderText({
      table <- user$data[logged_status == "ON", email]
      return(table)
     })
     
     
      # render selected user details in a table
     observeEvent(input$users_email, {
      user_data <- get_app_users(email = input$users_email)$content
      user_data_table <- as.data.table(user_data)[, .(user_uid, is_admin, email, created_at)]
      user_db_data <- user$data
      
      #merge the two tables
      full_data <- merge.data.table(user_data_table, user_db_data)
      # render user table
      output$user_details <- renderDataTable({
       datatable(
        full_data[1,],
        escape = FALSE,
        selection = "none",
        options = list(
         searching = FALSE,
         paging = FALSE,
         ordering = FALSE,          # Disable ordering in all columns
         lengthMenu = list(FALSE),  # Hide entries selection
         language = list(
          info = ""  # Hide the information about entries
         )
        ) 
     )
      })
     })
     
     # update roles and users choices
     roles_tibble <- get_roles(api_key = get_api_key())
     users_tibble <- get_app_users(api_key = get_api_key())
     
     # convert to data.table
     roles_tibble <- as.data.table(roles_tibble[["content"]])
     users_tibble <- as.data.table(users_tibble[["content"]])
     
     # update fields
     updateSelectizeInput(
      session = session,
      inputId = "users_role",
      choices = roles_tibble[, role_name],
      selected = "",
      server = TRUE,
      options = list(maxOptions = 3)
     )
     
     # update users email choices
     updateSelectizeInput(
      session = session,
      inputId = "users_email",
      choices =  users_tibble[, email], 
      selected = "",
      server = TRUE,
      options = list(maxOptions = 3)
     )
     # observe event on action select
     observeEvent(input$select_action, {
      # update the action button accordingly
      if(input$select_action == "Add role to user"){
       enable("users_role")
       disable("add_new_role")
       disable("isadmin")
       
         updateActionButton(
          inputId = "actions_button",
          label = "Add role",
          icon = icon("person-circle-plus"),
          session = session
         )
        }else if(input$select_action == "Enable user"){
         disable("users_role")
         disable("add_new_role")
         disable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label =  "Enable",
          icon =  icon("user-check"),
          session = session
         )
        }else if(input$select_action == "Disable user"){
         
         disable("users_role")
         disable("add_new_role")
         disable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label =  "Disable",
          icon =  icon("user-slash"),
          session = session
         )
        }else if(input$select_action == "Add new role to app"){
         
         disable("users_role")
         enable("add_new_role")
         disable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label =  "Add Role",
          icon = icon("user-shield"),
          session = session
         )
        }else if(input$select_action == "Delete from app"){
         disable("users_role")
         disable("add_new_role")
         disable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label = "Delete",
          icon = icon("user-xmark"),
          session = session
         )
        }else if(input$select_action == "Update administrator"){
         disable("users_role")
         disable("add_new_role")
         enable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label =  "Set admin",
          icon = icon("user-shield"),
          session = session
         )
        }else{
         disable("users_role")
         disable("add_new_role")
         disable("isadmin")
         
         updateActionButton(
          inputId = "actions_button",
          label = "Action",
          icon = icon("user-tie"),
          session = session
         )
        }
       })
     
     # observe event on button click
     observeEvent(input$actions_button, {
      
      email_input <- input$users_email
      select_input <- input$select_action

       if(email_input == "" | select_input == "") {
       # show a error notification
       show_toast(
        type = "error",
        position = "top-end",
        title = "Email or action required",
        text = "Select email and action!",
        session = session
       )
       }else{
      #get the user id
      users <- get_app_users(api_key = get_api_key())
      users_details <- as.data.table(users[["content"]])
      user_uid <- users_details[email == email_input, user_uid]

      # check status determine if user is enabled
      # Query data from MySQL table into a data.table
      user_data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
      status <- user_data[email == email_input, user_status]      
      # enable a user by assigning a role
      if(input$select_action == "Enable user"){
       req(email_input != "")
       req(select_input != "")
       # if status is Disabled then enable 
      if(status == "Disabled"){
      
       # update database for refresh querying
      user_status <- "Enabled"
      
      # Write the query
      update_query <-  sprintf("UPDATE user_details SET user_status = '%s'
                               WHERE user_uid = '%s'",
                               user_status, user_uid)
      
      DBI::dbSendQuery(con, update_query)
      # Reload data
      user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
      
      # show a success notification
      show_toast(
       type = "success",
       position = "top-end",
       title = "Enable Success",
       text = "User successfully enabled!",
       session = session
      )
      
      } else {
       # show a fail notification
       show_toast(
        type = "error",
        position = "top-end",
        title = "Enable Fail",
        text = "User already enabled!",
        session = session
       )
       }
      }else if(input$select_action == "Disable user"){
       req(email_input != "")
       req(select_input != "")
       if(status == "Enabled"){
        
       # update database for refresh querying
       user_status <- "Disabled"
       
       # Write the query
       update_query <-  sprintf("UPDATE user_details SET user_status = '%s'
                               WHERE user_uid = '%s'",
                                user_status, user_uid)
       
       DBI::dbSendQuery(con, update_query)
       # Reload data
       user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
       
       # show a success notification
       show_toast(
        type = "success",
        position = "top-end",
        title = "Disable Success",
        text = "User successfully disabled!",
        session = session
       )
       
       }else{
        # show a fail notification
        show_toast(
         type = "error",
         position = "top-end",
         title = "Disable Fail",
         text = "User already disabled!",
         session = session
        )
       }
       }else if(input$select_action == "Delete from app"){
        req(email_input != "")
        req(select_input != "")
        available_uids <- users_details[, user_uid]
        if(user_uid %in% available_uids){
        # delete a app user
        delete_user(
         user_uid = user_uid,
         api_key = get_api_key()
        )
         
         users_tibble <- get_app_users(api_key = get_api_key())
         users_tibble <- as.data.table(users_tibble[["content"]])
         # update users email choices
         updateSelectizeInput(
          session = session,
          inputId = "users_email",
          choices =  users_tibble[, email], 
          selected = "",
          server = TRUE,
          options = list(maxOptions = 3)
         )
         
        # remove from database too
        # Write the query
        delete_query <- sprintf("DELETE FROM user_details WHERE
                               user_uid = '%s'", user_uid) 
        
        DBI::dbSendQuery(con, delete_query)
        # Reload data
        user$data <- as.data.table(dbGetQuery(con, "SELECT * FROM user_details"))
        
        # show a success notification
        show_toast(
         type = "success",
         position = "top-end",
         title = "Delete Success",
         text = "User successfully deleted!",
         session = session
        )
        }else{
         # show a error notification
         show_toast(
          type = "error",
          position = "top-end",
          title = "Delete Failed",
          text = "User not found!",
          session = session
         )
        }
       }else if(input$select_action == "Add role to user"){
        req(email_input != "")
        req(select_input != "")
        #get the available user roles first
        user_roles <- get_user_roles(
         user_uid = user_uid, 
         api_key = get_api_key()
         )
        roles <- user_roles$content$role_name
        # conditions
        if(input$users_role %in% roles){
         # show a error notification
         show_toast(
          type = "error",
          position = "top-end",
          title = "Role set Failed",
          text = "Role already exists!",
          session = session
         )
        }else{
         add_user_role(
          user_uid = user_uid,
          role_name = input$users_role,
          api_key = get_api_key()
         )
         # show a success notification
         show_toast(
          type = "success",
          position = "top-end",
          title = "Role set Success",
          text = "Role set successfully!",
          session = session
         )
        }
        }else if(input$select_action == "Add new role to app"){
         app_current_roles <- get_roles(api_key = get_api_key())
         app_current_roles <- app_current_roles$content$role_name

         # condition for update
         if(input$add_new_role %in% app_current_roles){
          # show a error notification
          show_toast(
           type = "error",
           position = "top-end",
           title = "Role add Failed",
           text = "Role already exists!",
           session = session
          )
         }else{
          add_role(
           role_name = input$add_new_role , 
           api_key = get_api_key()
           )
         # show a success notification
         show_toast(
          type = "success",
          position = "top-end",
          title = "Role addition Success",
          text = "Role added successfully!",
          session = session
         )
}
         }else if(input$select_action == "Update administrator") {
          email_input <- input$users_email

      #get the user id
      users <- get_app_users(api_key = get_api_key())
      users_details <- as.data.table(users[["content"]])
      users_details <- users_details[email == email_input, .(user_uid, is_admin)]
      
      admin_check <- users_details[, is_admin]
      user_uid <- users_details[, user_uid]
      
      input_is_admin <- input$isadmin
      is_admin_out <- if (input_is_admin == "Yes") TRUE else FALSE
      get_app_info <- get_apps(app_name = "Technical-test-ACTSERV")
      
      # set condition
      if(admin_check == "TRUE" & is_admin_out == TRUE ||
       admin_check == "FALSE" & is_admin_out == FALSE) {
       # show a error notification
       show_toast(
        type = "error",
        position = "top-end",
        title = "Administrator set failed",
        text = "User status mantained in selected changes!",
        session = session
       )
      }else{
       # editing an existing user
      tryCatch({
       # update the app user
       res <- httr::PUT(
        url = paste0(.polished$api_url, "/app-users"),
        body = list(
         user_uid = user_uid,
         app_uid = .polished$app_uid,
         is_admin = input_is_admin,
         req_user_uid = session$userData$user()$user_uid
        ),
        httr::authenticate(
         user = get_api_key(),
         password = ""
        ),
        encode = "json"
       )
       
       if (!identical(httr::status_code(res), 200L)) {
        
        err <- jsonlite::fromJSON(
         httr::content(res, "text", encoding = "UTF-8")
        )
        stop(err, call. = FALSE)
       }
      })
      # show a success notification
      show_toast(
       type = "success",
       position = "top-end",
       title = "Administrator priviledges",
       text = "Previledges of Admin set!",
       session = session
      )
      }
         }else{
         return()
         }
       }
     }
     )
}
  )
}
 
 



