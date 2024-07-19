# Load the necessary libraries
library(shiny)
library(dplyr)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Unfälle Mannheim"),
  sidebarLayout(
    sidebarPanel(
      selectInput("UJAHR", "Jahr", choices = NULL, selected = "All"),
      selectInput("UKATEGORIE", "Unfallschwere", choices = NULL, selected = "All")
    ),
    mainPanel(
      leafletOutput("accidentMap"),
      textOutput("sourceInfo") # Add this line
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load the data
  accidentData <- read.csv("Unfaelle.csv")
  
  # Rename and convert columns
  accidentData <- accidentData %>%
    rename(Longitude = XGCSWGS84, Latitude = YGCSWGS84) %>%
    mutate(Longitude = as.numeric(gsub(",", ".", Longitude)),
           Latitude = as.numeric(gsub(",", ".", Latitude)))
  
  # Update selectInput choices dynamically
  observe({
    updateSelectInput(session, "UJAHR", choices = c("All", sort(unique(accidentData$UJAHR), decreasing = TRUE)))
    updateSelectInput(session, "UKATEGORIE", choices = c("All", unique(accidentData$UKATEGORIE)))
  })
  
  # Reactive expression to filter data based on input
  getMapData <- reactive({
    filteredData <- accidentData
    if (input$UJAHR != "All") {
      filteredData <- filteredData %>% filter(UJAHR == input$UJAHR)
    }
    if (input$UKATEGORIE != "All") {
      filteredData <- filteredData %>% filter(UKATEGORIE == input$UKATEGORIE)
    }
    if (nrow(filteredData) == 0) {
      print("No data found for the selected filters.") # Debugging information
    }
    filteredData
  })
  
  # Generate color palette for UKATEGORIE
  category_colors <- colorFactor(rainbow(length(unique(accidentData$UKATEGORIE))), unique(accidentData$UKATEGORIE))
  
  # Render the Leaflet map
  output$accidentMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = getMapData(), 
                       ~Longitude, ~Latitude, 
                       color = ~category_colors(UKATEGORIE),
                       radius = 3, # Smaller dots
                       popup = ~paste("Category:", UKATEGORIE))
  })
  
  # Add this output for the source information
  output$sourceInfo <- renderText({
    "Quelle: Unfallstatistik Statistisches Bundesamt"
  })
}

# Run the application
shinyApp(ui = ui, server = server)