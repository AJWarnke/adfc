server <- function(input, output, session) {
  
  # Karte
  output$site_map <- renderLeaflet({
    leaflet(data = sites_unique) %>%
      addTiles() %>%
      addMarkers(
        ~longitude, ~latitude,
        label = ~name,
        clusterOptions = markerClusterOptions()
      )
  })
  
  plot_yearly_bars <- function(df, title) {
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    
    df_sub <- subset(df, month <= 11)
    
    yearly_sums <- aggregate(counter ~ year, data = df_sub, sum, na.rm = TRUE)
    
    par(mar = c(5, 6, 4, 2) + 0.1) 
    
    barplot(
      height    = yearly_sums$counter,
      names.arg = yearly_sums$year,
      xlab      = "Jahr",
      ylab      = "",
      main      = title,
      col       = "steelblue",
      yaxt      = "n"
    )
    
    yticks <- axTicks(2)
    
    axis(2, 
         at     = yticks, 
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","), 
         las    = 1)
  }
  
  plot_quarter_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    df$year    <- lubridate::year(df$date)
    df$quarter <- lubridate::quarter(df$date)
    
    q_avg <- aggregate(
      counter ~ year + quarter,
      data = df,
      FUN = sum,
      na.rm = TRUE
    )
    
    q_avg$quarter_start <- as.Date(paste0(q_avg$year, "-", (q_avg$quarter - 1) * 3 + 1, "-01"))
    q_avg$date_mid <- q_avg$quarter_start + 45
    q_avg <- q_avg[order(q_avg$date_mid),]
    
    plot(
      q_avg$date_mid,
      q_avg$counter,
      type = "b",
      xlab = "Quartal",
      ylab = "Radfahrer pro Quartal",
      main = title,
      col  = "darkred",
      pch  = 19
    )
  }
  
  # ---- Standort Analysis (consolidated) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "analysis_standort", choices = standort_choices)
  })
  
  output$analysis_barplot <- renderPlot({
    req(input$analysis_standort)
    
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_yearly_bars(
      df_selected,
      paste("Anzahl Radfahrende in den ersten 11 Monaten -", input$analysis_standort)
    )
  })
  
  output$analysis_quarter_ts <- renderPlot({
    req(input$analysis_standort)
    
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_quarter_ts(
      df_selected,
      paste("Quartalsverlauf -", input$analysis_standort)
    )
  })
  
  plot_month_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    
    m_sum <- aggregate(counter ~ year + month, data = df, FUN = sum, na.rm = TRUE)
    
    # Datum für Mitte des Monats (besser lesbar als Monatsanfang)
    m_sum$month_start <- as.Date(sprintf("%d-%02d-01", m_sum$year, m_sum$month))
    m_sum$date_mid <- m_sum$month_start + 14
    m_sum <- m_sum[order(m_sum$date_mid), ]
    
    par(mar = c(5, 6, 4, 2) + 0.1)
    
    plot(
      m_sum$date_mid,
      m_sum$counter,
      type = "l",
      xlab = "Monat",
      ylab = "",
      main = title,
      col  = "darkgreen",
      lwd  = 2,
      yaxt = "n"
    )
    
    yticks <- axTicks(2)
    axis(2,
         at     = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las    = 1)
  }
  
  
  # ---- Cumulative Year Comparison ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_standort", choices = standort_choices)
  })
  
  output$cumulative_plot <- renderPlot({
    req(input$cumulative_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_standort)
    df_standort$date <- as.POSIXct(df_standort$date)
    
    df_standort$year <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) {
      plot.new()
      text(0.5, 0.5, "Keine Daten verfügbar", cex = 1.5)
      return()
    }
    
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    daily_sums <- aggregate(
      counter ~ year + day_of_year,
      data = df_standort,
      FUN = sum,
      na.rm = TRUE
    )
    
    years <- sort(unique(daily_sums$year))
    
    if (length(years) == 0) {
      plot.new()
      text(0.5, 0.5, "Keine Daten verfügbar", cex = 1.5)
      return()
    }
    
    year_totals <- numeric(length(years))
    
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    year_order <- order(-year_totals)
    ordered_years <- years[year_order]
    
    n_years <- length(ordered_years)
    colors <- rainbow(n_years, s = 0.7, v = 0.8)
    
    plot(
      NULL,
      xlim = c(1, 366),
      ylim = c(0, max(daily_sums$cumulative, na.rm = TRUE) * 1.05),
      xlab = "Tag des Jahres",
      ylab = "Kumulierte Summe der Radfahrer",
      main = paste("Jahresvergleich kumulativ (gesamtes Jahr) -", input$cumulative_standort),
      xaxt = "n"
    )
    
    month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", 
                     "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    axis(1, at = month_starts, labels = month_names)
    
    for (i in seq_along(ordered_years)) {
      year <- ordered_years[i]
      year_data <- daily_sums[daily_sums$year == year, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      
      if (nrow(year_data) > 0) {
        lines(
          year_data$day_of_year,
          year_data$cumulative,
          col = colors[i],
          lwd = 2,
          type = "l"
        )
      }
    }
    
    legend(
      "topleft",
      legend = as.character(ordered_years),
      col = colors,
      lwd = 2,
      title = "Jahr",
      ncol = 2,
      bg = "white"
    )
    
    grid(nx = NA, ny = NULL, lty = 2, col = "gray80")
  })
  
  # ---- Raw Data Explorer ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "raw_standort", choices = standort_choices)
  })
  
  observe({
    if (is.null(input$raw_standort)) return()
    
    df_standort <- subset(bike_counter, Standort == input$raw_standort)
    if (nrow(df_standort) > 0) {
      df_standort$date <- as.POSIXct(df_standort$date)
      min_date <- as.Date(min(df_standort$date, na.rm = TRUE))
      max_date <- as.Date(max(df_standort$date, na.rm = TRUE))
      
      updateDateRangeInput(
        session, 
        "raw_daterange",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date
      )
    }
  })
  
  output$analysis_month_ts <- renderPlot({
    req(input$analysis_standort)
    
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    
    plot_month_ts(
      df_selected,
      paste("Monatsverlauf -", input$analysis_standort)
    )
  })
  
  
  output$raw_timeseries <- renderPlot({
    req(input$raw_standort, input$raw_daterange)
    
    df_filtered <- subset(
      bike_counter,
      Standort == input$raw_standort
    )
    
    df_filtered$date <- as.POSIXct(df_filtered$date)
    
    df_filtered <- subset(
      df_filtered,
      as.Date(date) >= input$raw_daterange[1] & 
        as.Date(date) <= input$raw_daterange[2]
    )
    
    if (nrow(df_filtered) == 0) {
      plot.new()
      text(0.5, 0.5, "Keine Daten für den ausgewählten Zeitraum", cex = 1.5)
      return()
    }
    
    df_filtered <- df_filtered[order(df_filtered$date), ]
    
    plot(
      df_filtered$date,
      df_filtered$counter,
      type = "l",
      xlab = "Jahr",
      ylab = "Anzahl Radfahrer (pro Stunde)",
      main = paste("Rohdaten Zeitreihe -", input$raw_standort),
      col = "steelblue",
      lwd = 1.5,
      xaxt = "n"
    )
    
    grid(nx = NA, ny = NULL)
    
    years <- seq(
      from = lubridate::year(min(df_filtered$date)),
      to = lubridate::year(max(df_filtered$date)),
      by = 1
    )
    
    for (year in years) {
      year_start <- as.POSIXct(paste0(year, "-01-01"))
      abline(v = year_start, col = "gray40", lwd = 1, lty = 2)
    }
    
    year_positions <- as.POSIXct(paste0(years, "-01-01"))
    axis(1, at = year_positions, labels = years)
  })
  
}
