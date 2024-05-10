# UI function for the standard user content
app_contentUI <- function(id) {
 ns <- NS(id)
 value_theme <- "bg-gradient-teal-purple"
 tagList(
  useShinyjs(),
  includeCSS(
   path = "www/styles.css"
  ),
  card(
   card_header("Flights Delay among origin"),
   layout_sidebar(
    sidebar = sidebar(
     dateInput(
      inputId = ns("calender"),
      label = "Flights Date",
      min = "2013-01-01",
      max = "2013-12-31",
      value = "2013-01-01"
     ),
     pickerInput(
      inputId = ns("origin"),
      label = "Pick flight origin", 
      choices = unique(flights[, origin]), 
      options = pickerOptions(iconBase = "fas"),
      choicesOpt = list(
       icon = c("fa-plane", "fa-helicopter", "fa-plane-departure")
       )
      )
    ),
    layout_columns(
     height = "200px",
     value_box(
      title = "Total flights",
      value = textOutput(ns("total_flights")),
      theme = value_theme,
      showcase = bs_icon("airplane")
     ),
     value_box(
      title = "Depature delay",
      value = textOutput(ns("depature_delay")),
      theme = value_theme,
      showcase = bs_icon("calendar2-x-fill")
     ),
     value_box(
      title = "Arrival delay",
      value = textOutput(ns("arrival_delay")),
      theme = value_theme,
      showcase = bs_icon("calendar2-x-fill")
     ),
     value_box(
      title = "% of flights delayed",
      value = textOutput(ns("per_delay")),
      theme = value_theme,
      showcase = bs_icon("percent")
     )
    )
   )
  ),
  card(
   card_header("Flights Delay along months and minutes"),
   height = 400,
   layout_sidebar(
    sidebar = sidebar(
     sliderTextInput(
      inputId = ns("month_select"),
      label = "Month choice:",
      grid = TRUE, 
      force_edges = TRUE,
      choices = month.abb
     ),
     sliderInput(
      inputId = ns("minutes"),
      max = 1301,
      min = 1,
      value = 1,
      label = "Select delay minutes"
     ),
     actionButton(
      inputId = ns("plot"),
      label = "Plot",
      icon = icon("chart-simple")
     )
    ),
    layout_columns(
     echarts4rOutput(ns("total_output")),
     echarts4rOutput(ns("depature_output")),
     echarts4rOutput(ns("arrival_output"))
    )
   )
  )
 )
}

# Server function for the standard user content
app_contentServer <- function(id) {
 moduleServer(
  id,
  function(input, output, session) {
   # Observe event on selecting origin
   observeEvent(input$origin, {
    # Process date for filtering
    input_date <- as.character(input$calender)
    date_parts <- strsplit(input_date, "-")[[1]]
    month <- date_parts[2]
    day <- date_parts[3]
    input_day <- gsub("^0+", "", day)
    input_month <- gsub("^0+", "", month)

    # process data
    n_total <- flights[origin == input$origin &
                  month == input_month &
                  day == input_day, .N]
    n_dep_delay <- flights[origin == input$origin &
                  month == input_month &
                  day == input_day &
                  dep_delay >0, .N]
    n_arr_delay <- flights[origin == input$origin &
                  month == input_month &
                  day == input_day &
                  arr_delay >0, .N]
    
    output$total_flights <- renderText({
     return(n_total)
    })
    
    output$depature_delay <- renderText({
    return(n_dep_delay)
    })
    
    output$arrival_delay <- renderText({
     return(n_arr_delay)
    })
    output$per_delay <- renderText({
     per <- ((n_dep_delay+n_arr_delay) / (2*n_total))*100
     return(round(per, 2))
    })
    
   })
    
   # Observe event on slider input
 observeEvent(input$plot, {
  
  # convert abbreviated months to numbers
  month_mapping <- data.frame(
   abbreviation = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
   number = 1:12
  )
 # selected month
 selected_month <- month_mapping$number[month_mapping$abbreviation == input$month_select]
 input_minutes <- input$minutes
 # create gradient bar color
 linear_gradient <- htmlwidgets::JS(
  "{
  type: 'linear',
  x: 0,
  y: 1,
  x2: 1,
  y2: 1,
  colorStops: [{
      offset: 0.6, color: '#20c997' // color at 0%
  }, {
      offset: 1, color: '#41779f' // color at 100%
  }],
  global: false // default is false
}")
 
  # Output a graph for total delays
 total_data <- flights[month == selected_month, .(.N), by = .(origin)]
 output$total_output <- renderEcharts4r({
  total_data[order(N),] |> 
   e_charts(origin) |> 
   e_bar(N, itemStyle = list(
    color = linear_gradient 
   )) |> 
   e_flip_coords() |> 
   e_tooltip() |> 
   e_legend(show = FALSE) |> 
   e_title(text = "Total flights")
 })
 # Output a graph for arrival delays
 arrival_data <- flights[month == selected_month &
                          arr_delay > 0 &
                          arr_delay <= input_minutes,
                         .(.N), by = .(origin)]
 output$arrival_output <- renderEcharts4r({
  arrival_data[order(N),] |> 
   e_charts(origin) |> 
   e_bar(N, itemStyle = list(
    color = linear_gradient 
   )) |> 
   e_flip_coords() |> 
   e_tooltip() |> 
   e_legend(show = FALSE) |> 
   e_title(text = "Arrival Delays")
 })
 
 # Output a graph for departure delays
 departure_data <- flights[month == selected_month &
                            dep_delay > 0 &
                            dep_delay <= input_minutes,
                           .(.N), by = .(origin)]
 output$depature_output <- renderEcharts4r({
  departure_data[order(N),] |> 
   e_charts(origin) |> 
   e_bar(N, itemStyle = list(
    color = linear_gradient 
   )) |> 
   e_flip_coords() |> 
   e_tooltip() |> 
   e_legend(show = FALSE) |> 
   e_title(text = "Departure Delays")
 })
 
 
 
 
 })
  }
)
}
