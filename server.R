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
    updateSelectInput(session, "UJAHR", choices = c("Alle", sort(unique(accidentData$UJAHR), decreasing = TRUE)))
    updateSelectInput(session, "UKATEGORIE", choices = c("Alle", unique(accidentData$UKATEGORIE)))
  })
  
  observe({
    updateSelectInput(
      session, "UJAHR",
      choices = c("Alle", sort(unique(accidentData$UJAHR[accidentData$UJAHR != 2025]), decreasing = TRUE))
    )
    updateSelectInput(
      session, "UKATEGORIE",
      choices = c("Alle", unique(accidentData$UKATEGORIE))
    )
  })
  
  # Reactive expression to filter data based on input
  getMapData <- reactive({
    filteredData <- accidentData
    if (input$UJAHR != "Alle") {
      filteredData <- filteredData %>% filter(UJAHR == input$UJAHR)
    }
    if (input$UKATEGORIE != "Alle") {
      filteredData <- filteredData %>% filter(UKATEGORIE == input$UKATEGORIE)
    }
    if (nrow(filteredData) == 0) {
      print("No data found for the selected filters.") # Debugging information
    }
    filteredData
  })
  
  # Generate color palette for UKATEGORIE
  category_colors <- colorFactor(rainbow(length(unique(accidentData$UKATEGORIE))), unique(accidentData$UKATEGORIE))
  
  # Render the Leaflet point map
  output$accidentMap <- renderLeaflet({
    data <- getMapData()
    if(nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 8.4660, lat = 49.4875, zoom = 12))
    }
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, 
        color = ~category_colors(UKATEGORIE),
        radius = 3, # Smaller dots
        popup = ~paste("Category:", UKATEGORIE),
        label = ~lapply(seq_len(nrow(data)), function(i) {
          HTML(sprintf(
            "<div style='width: 200px;'>Datum (Monat/Jahr): %s.%s um %s:00 Uhr<br>Category: %s<br>%s</div>",
            data$UMONAT[i], data$UJAHR[i], data$USTUNDE[i], data$UKATEGORIE[i], data$Kommentar[i]
          ))
        }),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", 
            padding = "3px 8px",
            "white-space" = "normal",  # Allow text to wrap
            "word-wrap" = "break-word"  # Break long words if necessary
          ),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
  
  # Render the Leaflet heatmap
  output$heatMap <- renderLeaflet({
    data <- getMapData()
    leaflet() %>%
      addTiles() %>%
      addHeatmap(data = data, 
                 lng = ~Longitude, lat = ~Latitude,
                 blur = 20, max = 0.05, radius = 15)
  })
  
  # Add this output for the source information
  output$sourceInfo <- renderText({
    "Quelle: Unfallstatistik Statistisches Bundesamt"
  })
  
  # Add this output for the heatmap source information
  output$heatmapSourceInfo <- renderText({
    "Quelle: Unfallstatistik Statistisches Bundesamt"
  })
  
  output$deadlyAccidentsTable <- DT::renderDT({
    deadlyAccidents()
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Table deadly accidents
  deadlyAccidents <- reactive({
    accidentData %>%
      filter(trimws(UKATEGORIE) == "Unfall mit Getöteten") %>%
      mutate(
        Datum = paste0(sprintf("%02d", UMONAT), ".", UJAHR),
        Beschreibung = Kommentar,
        Ort = paste0("<a href='https://www.openstreetmap.org/?mlat=", 
                     Latitude, "&mlon=", Longitude, "#map=18/", Latitude, "/", Longitude,
                     "' target='_blank'>OpenStreetMap</a>")
      ) %>%
      select(Jahr = UJAHR, Datum, Beschreibung, Ort)
  })
}