# app.R

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)

# Source the UI and server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)