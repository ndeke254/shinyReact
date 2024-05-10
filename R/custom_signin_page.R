# Customize your sign in page UI with logos, text, and colors.
my_custom_sign_in_page <- sign_in_ui_default(
 color = "#012169",
 company_name = tags$head(
  tags$link(
   rel = "icon",
   type = "image/png",
   href = "logo.png"
  ),
  tags$title("NYC Flights")
 ),
 logo_top = tags$img(
  src = "logo.png",
  style = "width: 80px;"
 ),
 icon_href = "https://github.com/tidyverse/nycflights13",
 background_image = "signin_background.png",
 align = "right"
)
