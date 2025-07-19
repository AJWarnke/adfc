library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)

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
  
  # ADFC-Logo oben rechts
  tags$div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/a/a4/ADFC-Logo_2009_1.svg",
      height = "50px",
      alt = "ADFC Logo"
    )
  ),
  
  titlePanel("Unfälle mit beteiligten Radfahrern in Mannheim (jetzt inkl. 2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("UJAHR", "Jahr", choices = NULL, selected = "Alle"),
      selectInput("UKATEGORIE", "Unfallschwere", choices = NULL, selected = "Alle")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Punktkarte", 
          leafletOutput("accidentMap", height = "80vh"),
          textOutput("sourceInfo")
        ),
        
        tabPanel(
          "Heatmap", 
          leafletOutput("heatMap", height = "80vh"),
          textOutput("heatmapSourceInfo")
        ),
        
        tabPanel(
          "Kontakt",
          fluidPage(
            titlePanel("Kontakt und Informationen"),
            
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
                p("Email: arne.warnke [at] adfc-bw.de")
              )
            ),
            
            hr(),
            
            h3("Über dieses Projekt"),
            p("Diese interaktive Karte zeigt Unfalldaten mit Radfahrerbeteiligung in Mannheim."),
            p("Ziel ist es, mit Hilfe von offenen Daten die Transparenz zu fördern, städtische Planungen zu unterstützen und zur Verkehrssicherheit im Sinne von Vision Zero beizutragen."),
            p("Die Anwendung wurde vom ADFC Mannheim entwickelt – auf ehrenamtlicher Basis."),
			p("Quelle ist die Unfallstatistik des Statistischen Bundesamtes."),
			
			p("Weitere Informationen zur Unfallverhütung durch den ADFC Mannheim finden Sie hier:"),
			tags$a(
              href = "https://mannheim.adfc.de/artikel/unfallverhuetung-als-zentrale-aufgabe-des-adfc-mannheim",
              "Unfallverhütung als zentrale Aufgabe des ADFC Mannheim",
              target = "_blank"
            ),
            
            br(), br(),
			
            p("Der Quellcode ist öffentlich zugänglich auf GitHub:"),
            tags$a(
              href = "https://github.com/AJWarnke/adfc",
              "https://github.com/AJWarnke/adfc",
              target = "_blank"
            )
          )
        ),
			tabPanel("Liste tödlicher Radunfälle",
			         fluidPage(
			           h3("Tödliche Unfälle - Übersicht"),
			           DTOutput("deadlyAccidentsTable")  # <- CORRECT
			         )
			)
      )
    )
  )
)
