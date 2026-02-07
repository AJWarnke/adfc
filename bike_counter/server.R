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
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    df_sub <- df  # Removed the subset filter - now uses all months
    yearly_sums <- aggregate(counter ~ year, data = df_sub, sum, na.rm = TRUE)
    par(mar = c(5, 6, 4, 2) + 0.1)
    barplot(
      height = yearly_sums$counter,
      names.arg = yearly_sums$year,
      xlab = "Jahr",
      ylab = "",
      main = title,
      col = "steelblue",
      yaxt = "n"
    )
    yticks <- axTicks(2)
    axis(2,
         at = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las = 1)
  }
  
  plot_quarter_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    df$year <- lubridate::year(df$date)
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
      col = "darkred",
      pch = 19
    )
  }
  
  plot_month_ts <- function(df, title) {
    df$date <- as.POSIXct(df$date)
    df$year <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    
    # Exclude current month
    current_year <- lubridate::year(Sys.Date())
    current_month <- lubridate::month(Sys.Date())
    df <- df[!(df$year == current_year & df$month == current_month), ]
    
    m_sum <- aggregate(counter ~ year + month, data = df, FUN = sum, na.rm = TRUE)
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
      col = "darkgreen",
      lwd = 2,
      yaxt = "n"
    )
    
    # Add horizontal dashed light gray grid lines
    grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")
    
    yticks <- axTicks(2)
    axis(2,
         at = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las = 1)
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
      paste("Anzahl Radfahrende -", input$analysis_standort)
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
  
  output$analysis_month_ts <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_month_ts(
      df_selected,
      paste("Monatsverlauf -", input$analysis_standort)
    )
  })
  
  # ---- Cumulative Year Comparison (Interactive with Plotly) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_standort", choices = standort_choices)
  })
  
  output$cumulative_plot <- renderPlotly({
    req(input$cumulative_standort)
    df_standort <- subset(bike_counter, Standort == input$cumulative_standort)
    df_standort$date <- as.POSIXct(df_standort$date)
    df_standort$year <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) {
      return(NULL)
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
      return(NULL)
    }
    
    # Calculate cumulative sums
    year_totals <- numeric(length(years))
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    # Order years by total (highest first)
    year_order <- order(-year_totals)
    ordered_years <- years[year_order]
    
    # Create plotly figure
    fig <- plot_ly()
    
    # Add a line for each year
    for (i in seq_along(ordered_years)) {
      year <- ordered_years[i]
      year_data <- daily_sums[daily_sums$year == year, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      
      if (nrow(year_data) > 0) {
        fig <- fig %>%
          add_trace(
            data = year_data,
            x = ~day_of_year,
            y = ~cumulative,
            type = 'scatter',
            mode = 'lines',
            name = as.character(year),
            line = list(width = 2),
            hovertemplate = paste0(
              "<b>Jahr: ", year, "</b><br>",
              "Tag: %{x}<br>",
              "Kumuliert: %{y:,.0f}<br>",
              "<extra></extra>"
            )
          )
      }
    }
    
    # Month labels for x-axis
    month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
                     "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    
    # Layout
    fig <- fig %>%
      layout(
        title = paste("Jahresvergleich kumulativ (gesamtes Jahr) -", input$cumulative_standort),
        xaxis = list(
          title = "Tag des Jahres",
          tickmode = "array",
          tickvals = month_starts,
          ticktext = month_names,
          range = c(1, 366)
        ),
        yaxis = list(
          title = "Kumulierte Summe der Radfahrer",
          separatethousands = TRUE
        ),
        hovermode = "closest",
        legend = list(
          title = list(text = "<b>Jahr</b>"),
          orientation = "v",
          x = 0.02,
          y = 0.98,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "gray",
          borderwidth = 1
        ),
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "white"
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
        displaylogo = FALSE
      )
    
    return(fig)
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
  
  # ---- Monthly Station Summary Table (24 months) ----
  output$monthlyStationTable <- renderDT({
    cutoff_date <- Sys.Date() %m-% months(24)
    
    df_recent <- bike_counter %>%
      filter(date >= cutoff_date) %>%
      mutate(
        year = year(date),
        month = month(date),
        year_month = sprintf("%04d-%02d", year, month)
      )
    
    monthly_summary <- df_recent %>%
      group_by(Standort, year_month) %>%
      summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
    
    wide_table <- monthly_summary %>%
      pivot_wider(
        names_from = year_month,
        values_from = total,
        values_fill = 0
      )
    
    month_cols <- setdiff(names(wide_table), "Standort")
    month_cols_sorted <- sort(month_cols, decreasing = TRUE)
    
    wide_table <- wide_table %>%
      select(Standort, all_of(month_cols_sorted))
    
    datatable(
      wide_table,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        fixedColumns = list(leftColumns = 1),
        dom = 'Bfrtip',
        language = list(
          search = "Suchen:",
          lengthMenu = "Zeige _MENU_ Einträge",
          info = "Zeige _START_ bis _END_ von _TOTAL_ Standorten",
          paginate = list(
            first = "Erste",
            last = "Letzte",
            `next` = "Nächste",
            previous = "Vorherige"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatStyle(
        columns = names(wide_table),
        fontSize = '12px'
      ) %>%
      formatStyle(
        'Standort',
        fontWeight = 'bold',
        backgroundColor = '#f5f5f5'
      )
  })
  
  output$downloadMonthlyTable <- downloadHandler(
    filename = function() {
      paste0("monatsübersicht_radfahrer_", Sys.Date(), ".csv")
    },
    content = function(file) {
      cutoff_date <- Sys.Date() %m-% months(24)
      
      df_recent <- bike_counter %>%
        filter(date >= cutoff_date) %>%
        mutate(
          year = year(date),
          month = month(date),
          year_month = sprintf("%04d-%02d", year, month)
        )
      
      monthly_summary <- df_recent %>%
        group_by(Standort, year_month) %>%
        summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
      
      wide_table <- monthly_summary %>%
        pivot_wider(
          names_from = year_month,
          values_from = total,
          values_fill = 0
        )
      
      month_cols <- setdiff(names(wide_table), "Standort")
      month_cols_sorted <- sort(month_cols, decreasing = TRUE)
      
      wide_table <- wide_table %>%
        select(Standort, all_of(month_cols_sorted))
      
      write.csv(wide_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
}
