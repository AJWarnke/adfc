library(shiny)
library(shinydashboard)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)  # Add this line

sites <- read_delim("data/site.csv", delim = ";", show_col_types = FALSE)
sites_unique <- sites %>%
  select(name, latitude, longitude) %>%
  distinct()

bike_counter <- readRDS("data/bike_counter_data.rds")
bike_counter$date <- as.POSIXct(bike_counter$date, tz = "UTC")
