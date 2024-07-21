# ui.R

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #accidentMap, #heatMap {
        height: calc(100vh - 120px) !important;
      }
      .leaflet-container {
        height: 100%;
      }
    "))
  ),
  titlePanel("Unfälle mit beteiligten Radfahrern in Mannheim"),
  sidebarLayout(
    sidebarPanel(
      selectInput("UJAHR", "Jahr", choices = NULL, selected = "Alle"),
      selectInput("UKATEGORIE", "Unfallschwere", choices = NULL, selected = "Alle")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Punktkarte", 
                 leafletOutput("accidentMap", height = "80vh"),
                 textOutput("sourceInfo")),
        tabPanel("Heatmap", 
                 leafletOutput("heatMap", height = "80vh"),
                 textOutput("heatmapSourceInfo"))
      )
    )
  )
)