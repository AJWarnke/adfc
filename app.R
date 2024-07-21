# app.R

library(shiny)

# Source the UI and server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)