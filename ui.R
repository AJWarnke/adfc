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
                 textOutput("heatmapSourceInfo")),
        tabPanel(
          "Kontakt",
          fluidPage(
            titlePanel("Kontakt"),
            fluidRow(
              column(
                width = 6,
                h3("Robert Hofmann"),
                p("Sprecher ADFC Mannheim"),
                p("Email: Robert.Hofmann [at] adfc-bw.de")
              ),
              column(
                width = 6,
                h3("Arne Warnke"),
                p("ADFC Mannheim"),
                p("Email: arne.warnke [at] gmail.com")
              )
            )
          )
        )
      )
    )
  )
)
