# import packages and libraries
library(shiny)
library(bslib)
library(polished)
library(nycflights13)
library(data.table)
library(shinyjqui)
library(shinyjs)
library(RMySQL)
library(DT)
library(shinyWidgets)
library(dplyr)
library(bsicons)
library(echarts4r)
library(shiny.fluent)

# Retrieve database credentials
dotenv::load_dot_env()
app_name <- Sys.getenv("APP_NAME")
api_key <- Sys.getenv("API_KEY")

# configure polished auth when the app initially starts up
polished_config(
 app_name = app_name,
 api_key = api_key,
 is_invite_required = FALSE
)

# Retrieve database credentials
host <- Sys.getenv("DB_HOST")
port <- as.numeric(Sys.getenv("DB_PORT"))
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")

# make a connection to MySQL
con <- dbConnect(
 RMySQL::MySQL(),
 dbname = dbname,
 host = host,
 port = port,
 user = user,
 password = password
)

# Load data from MySQL into a reactiveValues object
user <- reactiveValues(data = NULL)

# Set App api_key
set_api_key(api_key = api_key)

