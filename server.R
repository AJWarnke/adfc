server <- function(input, output, session) {
  
  # Load the data
  accidentData <- read.csv("Unfaelle.csv")
  
  # Codierung für UTYP1 gemäß PDF - nur Text ohne Nummern
  utyp1_labels <- c(
    "Fahrunfall (F)",
    "Abbiege-Unfall (AB)",
    "Einbiegen/Kreuzen-Unfall (EK)",
    "Überschreiten-Unfall (ÜS)",
    "Unfall durch ruhenden Verkehr (RV)",
    "Unfall im Längsverkehr (LV)",
    "Sonstiger Unfall (SO)"
  )
  names(utyp1_labels) <- 1:7
  
  # Update selectInput choices dynamically
  observe({
    updateSelectInput(
      session, "UJAHR",
      choices = c("Alle", sort(unique(accidentData$UJAHR[accidentData$UJAHR != 2025]), decreasing = TRUE))
    )
    updateSelectInput(
      session, "UKATEGORIE",
      choices = c("Alle", unique(accidentData$UKATEGORIE))
    )
    
    # UTYP1 mit beschreibenden Labels (nur Text, keine Nummern)
    utyp1_values <- sort(unique(accidentData$UTYP1))
    # Create named vector: display text, value = number
    utyp1_choices <- c("Alle" = "Alle", setNames(as.character(utyp1_values), utyp1_labels[as.character(utyp1_values)]))
    
    updateSelectInput(
      session,
      "UTYP1",
      choices = utyp1_choices
    )
    
    # involved Filter - Multiple Choice (kein "Alle" Option nötig bei multiple=TRUE)
    updateSelectInput(
      session,
      "involved",
      choices = sort(unique(accidentData$involved))
    )
    
    # art Filter
    updateSelectInput(
      session,
      "art",
      choices = c("Alle", sort(unique(accidentData$art)))
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
    
    # UTYP1 Filter
    if (input$UTYP1 != "Alle") {
      # input$UTYP1 enthält jetzt direkt die Nummer als String
      selected_utyp1 <- as.integer(input$UTYP1)
      filteredData <- filteredData %>% filter(UTYP1 == selected_utyp1)
    }
    
    # involved Filter - Multiple Choice
    if (!is.null(input$involved) && length(input$involved) > 0) {
      filteredData <- filteredData %>% filter(involved %in% input$involved)
    }
    
    # art Filter
    if (input$art != "Alle") {
      filteredData <- filteredData %>% filter(art == input$art)
    }
    
    if (nrow(filteredData) == 0) {
      print("No data found for the selected filters.") # Debugging information
    }
    
    filteredData
  })
  
  # 
  category_colors <- colorFactor(rainbow(length(unique(accidentData$UKATEGORIE))), unique(accidentData$UKATEGORIE))
  
  output$accidentMap <- renderLeaflet({
    data <- getMapData()
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 8.4660, lat = 49.4875, zoom = 12))
    }
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, 
        color = ~category_colors(UKATEGORIE),
        radius = 3,
        popup = ~paste("Category:", UKATEGORIE),
        label = ~lapply(seq_len(nrow(data)), function(i) {
          HTML(sprintf(
            "<div style='width: 500px;'>Datum (Monat/Jahr): %s.%s um %s:00 Uhr<br>Category: %s<br>Beteiligte: %s<br>Typ: %s<br>Art: %s<br>%s</div>",
            data$UMONAT[i], data$UJAHR[i], data$USTUNDE[i], data$UKATEGORIE[i], data$involved[i], data$typ[i], data$art[i], data$Kommentar[i]
          ))
        }),
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", 
            padding = "3px 8px",
            "white-space" = "normal",
            "word-wrap" = "break-word"
          ),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomright", 
        pal = category_colors, 
        values = data$UKATEGORIE,
        title = "Unfallkategorie",
        opacity = 1
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
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })
  
  # Add this output for the heatmap source information
  output$heatmapSourceInfo <- renderText({
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
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
                     Latitude, "&mlon=", Longitude, "#map=18/", Latitude, 
                     "/", Longitude,
                     "' target='_blank'>OpenStreetMap</a>")
      ) %>%
      select(Jahr = UJAHR, Datum, Beschreibung, Ort)
  })
  
  # Render the Grid Density Map
  output$gridMap <- renderLeaflet({
    data <- getMapData()
    
    if (nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 8.4660, lat = 49.4875, zoom = 12))
    }
    
    # Define grid size (approx 0.001 degrees ≈ ~100m)
    GRID_SIZE <- 0.001
    
    # Compute grid bins
    data <- data %>%
      mutate(
        lat_bin = floor(Latitude / GRID_SIZE) * GRID_SIZE,
        lon_bin = floor(Longitude / GRID_SIZE) * GRID_SIZE
      )
    
    # Count accidents per grid cell
    grid_counts <- data %>%
      group_by(lat_bin, lon_bin) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Color palette
    pal <- colorNumeric(palette = "YlOrRd", domain = grid_counts$count)
    
    # Create base map
    m <- leaflet() %>%
      addTiles()
    
    # Add rectangles for each grid cell
    for (i in 1:nrow(grid_counts)) {
      lat <- grid_counts$lat_bin[i]
      lon <- grid_counts$lon_bin[i]
      count <- grid_counts$count[i]
      
      m <- m %>%
        addRectangles(
          lng1 = lon, lat1 = lat,
          lng2 = lon + GRID_SIZE, lat2 = lat + GRID_SIZE,
          fillColor = pal(count),
          fillOpacity = 0.7,
          color = "#777",
          weight = 0.5,
          label = paste(count, "Unfälle")
        )
    }
    
    m %>%
      addLegend("bottomright", pal = pal, values = grid_counts$count, title = "Unfälle pro Rasterzelle")
  })
  
  
  output$gridMapSourceInfo <- renderText({
    "Quelle: ADFC Mannheim / Unfallstatistik Statistisches Bundesamt"
  })
  
  output$downloadDeadlyAccidents <- downloadHandler(
    filename = function() {
      paste0("Tödliche_Unfälle_Mannheim_Fahrrad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(deadlyAccidents(), file, row.names = FALSE)
    }
  )
  
}