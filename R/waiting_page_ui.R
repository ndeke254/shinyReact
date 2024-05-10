
# create a UI for logged in but not enabled
waiting_pageUI <- function(id) {
  ns <- NS(id)
    tagList(
     useShinyjs(),
     includeCSS(
      path = "www/styles.css"
     ),
     card(
      fill = FALSE,
     class = "card",
     card_header(
      tags$strong("LOGGED IN SUCCESSFULLY!")
      ),
     card_image(
      file = "www/logo.png",
      width = "14%"
      ),
     card_body(
      div(
       id = "status",
       "Welcome to New York City Flights 2013"
     )
     )
    )
  )
  }

waiting_pageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session ) {
     
     })
    }

     


