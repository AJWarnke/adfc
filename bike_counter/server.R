server <- function(input, output, session) {
  
  # ---- Karte ----
  output$site_map <- renderLeaflet({
    leaflet(data = sites_unique) %>%
      addTiles() %>%
      addMarkers(
        ~longitude, ~latitude,
        label = ~name,
        clusterOptions = markerClusterOptions()
      )
  })
  
  # ---- Helper plot functions (base R) ----
  plot_yearly_bars <- function(df, title) {
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    df_sub   <- df
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
    df$date    <- as.POSIXct(df$date)
    df$year    <- lubridate::year(df$date)
    df$quarter <- lubridate::quarter(df$date)
    q_avg <- aggregate(counter ~ year + quarter, data = df, FUN = sum, na.rm = TRUE)
    q_avg$quarter_start <- as.Date(paste0(q_avg$year, "-", (q_avg$quarter - 1) * 3 + 1, "-01"))
    q_avg$date_mid      <- q_avg$quarter_start + 45
    q_avg <- q_avg[order(q_avg$date_mid), ]
    plot(
      q_avg$date_mid, q_avg$counter,
      type  = "b",
      xlab  = "Quartal",
      ylab  = "Radfahrer pro Quartal",
      main  = title,
      col   = "darkred",
      pch   = 19,
      xaxt  = "n"
    )
    quarter_labels <- paste0(q_avg$year, " Q", q_avg$quarter)
    axis(1, at = q_avg$date_mid, labels = quarter_labels, las = 2, cex.axis = 0.8)
  }
  
  plot_month_ts <- function(df, title) {
    df$date          <- as.POSIXct(df$date)
    df$year          <- lubridate::year(df$date)
    df$month         <- lubridate::month(df$date)
    current_year     <- lubridate::year(Sys.Date())
    current_month    <- lubridate::month(Sys.Date())
    df <- df[!(df$year == current_year & df$month == current_month), ]
    m_sum <- aggregate(counter ~ year + month, data = df, FUN = sum, na.rm = TRUE)
    m_sum$month_start <- as.Date(sprintf("%d-%02d-01", m_sum$year, m_sum$month))
    m_sum$date_mid    <- m_sum$month_start + 14
    m_sum <- m_sum[order(m_sum$date_mid), ]
    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(
      m_sum$date_mid, m_sum$counter,
      type  = "l",
      xlab  = "Monat",
      ylab  = "",
      main  = title,
      col   = "darkgreen",
      lwd   = 2,
      yaxt  = "n"
    )
    grid(nx = NA, ny = NULL, lty = 2, col = "lightgray")
    yticks <- axTicks(2)
    axis(2,
         at     = yticks,
         labels = format(yticks, scientific = FALSE, big.mark = ".", decimal.mark = ","),
         las    = 1)
  }
  
  # ---- Standort Analysis ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "analysis_standort", choices = standort_choices)
  })
  
  output$analysis_barplot <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_yearly_bars(df_selected, paste("Anzahl Radfahrende -", input$analysis_standort))
  })
  
  output$analysis_quarter_ts <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_quarter_ts(df_selected, paste("Quartalsverlauf -", input$analysis_standort))
  })
  
  output$analysis_month_ts <- renderPlot({
    req(input$analysis_standort)
    df_selected <- subset(bike_counter, Standort == input$analysis_standort)
    plot_month_ts(df_selected, paste("Monatsverlauf -", input$analysis_standort))
  })
  
  # ---- Cumulative Year Comparison (full year, interactive Plotly) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_standort", choices = standort_choices)
  })
  
  observe({
    standortchoices <- unique(bike_counter$Standort)
    updateSelectInput(session, "last14_station", choices = standortchoices)
  })
  
  output$cumulative_plot <- renderPlotly({
    req(input$cumulative_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_standort)
    df_standort$date        <- as.POSIXct(df_standort$date)
    df_standort$year        <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    daily_sums <- aggregate(counter ~ year + day_of_year, data = df_standort,
                            FUN = sum, na.rm = TRUE)
    years <- sort(unique(daily_sums$year))
    if (length(years) == 0) return(NULL)
    
    # Cumulative sums per year
    year_totals <- numeric(length(years))
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    ordered_years <- years[order(-year_totals)]
    
    fig <- plot_ly()
    for (yr in ordered_years) {
      year_data <- daily_sums[daily_sums$year == yr, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      if (nrow(year_data) > 0) {
        fig <- add_trace(
          fig,
          data          = year_data,
          x             = ~day_of_year,
          y             = ~cumulative,
          type          = "scatter",
          mode          = "lines",
          name          = as.character(yr),
          line          = list(width = 2),
          hovertemplate = paste0("<b>Jahr: ", yr, "</b><br>",
                                 "Tag: %{x}<br>",
                                 "Kumuliert: %{y:,.0f}<br>",
                                 "<extra></extra>")
        )
      }
    }
    
    month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names  <- c("Jan","Feb","Mär","Apr","Mai","Jun",
                      "Jul","Aug","Sep","Okt","Nov","Dez")
    
    fig <- plotly::layout(
      fig,
      title = paste("Jahresvergleich kumulativ (gesamtes Jahr) -", input$cumulative_standort),
      xaxis = list(title    = "Tag des Jahres",
                   tickmode = "array",
                   tickvals = month_starts,
                   ticktext = month_names,
                   range    = c(1, 366)),
      yaxis = list(title             = "Kumulierte Summe der Radfahrer",
                   separatethousands = TRUE),
      hovermode    = "closest",
      legend       = list(title       = list(text = "<b>Jahr</b>"),
                          orientation = "v", x = 0.02, y = 0.98,
                          bgcolor     = "rgba(255,255,255,0.8)",
                          bordercolor = "gray", borderwidth = 1),
      plot_bgcolor  = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo            = FALSE
    )
    
    return(fig)
  })
  
  # ---- Cumulative Year Comparison (Partial – up to most recent day - 1) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_partial_standort", choices = standort_choices)
  })
  
  output$cumulative_partial_plot <- renderPlotly({
    req(input$cumulative_partial_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_partial_standort)
    df_standort$date        <- as.POSIXct(df_standort$date)
    df_standort$year        <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    max_date <- max(df_standort$date, na.rm = TRUE)
    max_doy  <- lubridate::yday(max_date) - 1L
    if (max_doy < 1) max_doy <- 1
    
    df_filtered <- df_standort[df_standort$day_of_year <= max_doy, ]
    daily_sums  <- aggregate(counter ~ year + day_of_year, data = df_filtered,
                             FUN = sum, na.rm = TRUE)
    years <- sort(unique(daily_sums$year))
    if (length(years) == 0) return(NULL)
    
    year_totals <- numeric(length(years))
    for (i in seq_along(years)) {
      year_data <- daily_sums[daily_sums$year == years[i], ]
      year_data <- year_data[order(year_data$day_of_year), ]
      daily_sums$cumulative[daily_sums$year == years[i]] <- cumsum(year_data$counter)
      year_totals[i] <- max(cumsum(year_data$counter))
    }
    
    ordered_years <- years[order(-year_totals)]
    
    fig <- plot_ly()
    for (yr in ordered_years) {
      year_data <- daily_sums[daily_sums$year == yr, ]
      year_data <- year_data[order(year_data$day_of_year), ]
      if (nrow(year_data) > 0) {
        fig <- add_trace(
          fig,
          data          = year_data,
          x             = ~day_of_year,
          y             = ~cumulative,
          type          = "scatter",
          mode          = "lines",
          name          = as.character(yr),
          line          = list(width = 2),
          hovertemplate = paste0("Jahr: ", yr, "<br>",
                                 "Tag: %{x}<br>",
                                 "Kumuliert: %{y:,.0f}",
                                 "<extra></extra>")
        )
      }
    }
    
    month_starts    <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month_names     <- c("Jan","Feb","Mär","Apr","Mai","Jun",
                         "Jul","Aug","Sep","Okt","Nov","Dez")
    visible_months  <- month_starts[month_starts <= max_doy]
    visible_names   <- month_names[month_starts  <= max_doy]
    
    fig <- plotly::layout(
      fig,
      title = paste0("Jahresvergleich kumulativ (bis Tag ", max_doy, ") - ",
                     input$cumulative_partial_standort),
      xaxis = list(title    = "Tag des Jahres",
                   tickmode = "array",
                   tickvals = visible_months,
                   ticktext = visible_names,
                   range    = c(1, max_doy)),
      yaxis = list(title             = "Kumulierte Summe der Radfahrer",
                   separatethousands = TRUE),
      hovermode    = "closest",
      legend       = list(title       = list(text = "Jahr"),
                          orientation = "v", x = 0.02, y = 0.98,
                          bgcolor     = "rgba(255,255,255,0.8)",
                          bordercolor = "gray", borderwidth = 1),
      plot_bgcolor  = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo            = FALSE
    )
    
    return(fig)
  })
  
  # ---- Cumulative Year Comparison (Partial – Bar Chart) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "cumulative_bar_standort", choices = standort_choices)
  })
  
  output$cumulative_bar_plot <- renderPlotly({
    req(input$cumulative_bar_standort)
    
    df_standort <- subset(bike_counter, Standort == input$cumulative_bar_standort)
    df_standort$date        <- as.POSIXct(df_standort$date)
    df_standort$year        <- lubridate::year(df_standort$date)
    df_standort$day_of_year <- lubridate::yday(df_standort$date)
    
    if (nrow(df_standort) == 0) return(NULL)
    df_standort <- df_standort[!is.na(df_standort$counter), ]
    
    max_date <- max(df_standort$date, na.rm = TRUE)
    max_doy  <- lubridate::yday(max_date) - 1L
    if (max_doy < 1) max_doy <- 1
    
    df_filtered <- df_standort[df_standort$day_of_year <= max_doy, ]
    year_sums   <- aggregate(counter ~ year, data = df_filtered, FUN = sum, na.rm = TRUE)
    year_sums   <- year_sums[order(year_sums$year), ]
    year_sums$year <- as.character(year_sums$year)
    
    current_year <- as.character(lubridate::year(max_date))
    bar_colors   <- ifelse(year_sums$year == current_year, "#e07b00", "steelblue")
    
    fig <- plot_ly(
      data          = year_sums,
      x             = ~year,
      y             = ~counter,
      type          = "bar",
      marker        = list(color = bar_colors),
      hovertemplate = paste0("Jahr: %{x}<br>",
                             "Kumuliert (bis Tag ", max_doy, "): %{y:,.0f}",
                             "<extra></extra>")
    )
    
    fig <- plotly::layout(
      fig,
      title        = paste0("Jahresvergleich kumulativ (bis Tag ", max_doy, ") - ",
                            input$cumulative_bar_standort),
      xaxis        = list(title = "Jahr", type = "category"),
      yaxis        = list(title = "Kumulierte Summe der Radfahrer",
                          separatethousands = TRUE),
      hovermode    = "closest",
      plot_bgcolor  = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo            = FALSE
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
      updateDateRangeInput(session, "raw_daterange",
                           start = min_date, end   = max_date,
                           min   = min_date, max   = max_date)
    }
  })
  
  output$raw_timeseries <- renderPlot({
    req(input$raw_standort, input$raw_daterange)
    df_filtered <- subset(bike_counter, Standort == input$raw_standort)
    df_filtered$date <- as.POSIXct(df_filtered$date)
    df_filtered <- subset(df_filtered,
                          as.Date(date) >= input$raw_daterange[1] &
                            as.Date(date) <= input$raw_daterange[2])
    
    if (nrow(df_filtered) == 0) {
      plot.new()
      text(0.5, 0.5, "Keine Daten für den ausgewählten Zeitraum", cex = 1.5)
      return()
    }
    
    df_filtered <- df_filtered[order(df_filtered$date), ]
    plot(
      df_filtered$date, df_filtered$counter,
      type  = "l",
      xlab  = "Jahr",
      ylab  = "Anzahl Radfahrer (pro Stunde)",
      main  = paste("Rohdaten Zeitreihe -", input$raw_standort),
      col   = "steelblue",
      lwd   = 1.5,
      xaxt  = "n"
    )
    grid(nx = NA, ny = NULL)
    
    years <- seq(from = lubridate::year(min(df_filtered$date)),
                 to   = lubridate::year(max(df_filtered$date)), by = 1)
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
      mutate(year       = year(date),
             month      = month(date),
             year_month = sprintf("%04d-%02d", year, month))
    
    monthly_summary <- df_recent %>%
      group_by(Standort, year_month) %>%
      summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
    
    wide_table <- monthly_summary %>%
      pivot_wider(names_from  = year_month,
                  values_from = total,
                  values_fill = 0)
    
    month_cols        <- setdiff(names(wide_table), "Standort")
    month_cols_sorted <- sort(month_cols, decreasing = TRUE)
    wide_table        <- wide_table %>% select(Standort, all_of(month_cols_sorted))
    
    datatable(
      wide_table,
      options = list(
        pageLength = 25,
        scrollX    = TRUE,
        scrollY    = "600px",
        fixedColumns = list(leftColumns = 1),
        dom        = "Bfrtip",
        language   = list(
          search     = "Suchen:",
          lengthMenu = "Zeige _MENU_ Einträge",
          info       = "Zeige _START_ bis _END_ von _TOTAL_ Standorten",
          paginate   = list(first    = "Erste", last     = "Letzte",
                            `next`   = "Nächste", previous = "Vorherige")
        )
      ),
      rownames = FALSE,
      class    = "cell-border stripe compact"
    ) %>%
      formatStyle(columns = names(wide_table), fontSize = "12px") %>%
      formatStyle("Standort", fontWeight = "bold", backgroundColor = "#f5f5f5")
  })
  
  output$downloadMonthlyTable <- downloadHandler(
    filename = function() {
      paste0("monatsübersicht_radfahrer_", Sys.Date(), ".csv")
    },
    content = function(file) {
      cutoff_date <- Sys.Date() %m-% months(24)
      
      df_recent <- bike_counter %>%
        filter(date >= cutoff_date) %>%
        mutate(year       = year(date),
               month      = month(date),
               year_month = sprintf("%04d-%02d", year, month))
      
      monthly_summary <- df_recent %>%
        group_by(Standort, year_month) %>%
        summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")
      
      wide_table <- monthly_summary %>%
        pivot_wider(names_from  = year_month,
                    values_from = total,
                    values_fill = 0)
      
      month_cols        <- setdiff(names(wide_table), "Standort")
      month_cols_sorted <- sort(month_cols, decreasing = TRUE)
      wide_table        <- wide_table %>% select(Standort, all_of(month_cols_sorted))
      
      write.csv(wide_table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # ---- Monthly Barchart (Monatsbalken) ----
  observe({
    standort_choices <- unique(bike_counter$Standort)
    updateSelectInput(session, "monthly_bar_standort", choices = standort_choices)
  })
  
  output$monthly_bar_plot <- renderPlotly({
    req(input$monthly_bar_standort)
    
    df      <- subset(bike_counter, Standort == input$monthly_bar_standort)
    df$date <- as.Date(df$date)
    
    max_date     <- max(df$date, na.rm = TRUE)
    cutoff_day   <- as.integer(format(max_date, "%d")) - 1L
    cutoff_month <- as.integer(format(max_date, "%m"))
    
    df$year  <- lubridate::year(df$date)
    df$month <- lubridate::month(df$date)
    df$dom   <- as.integer(format(df$date, "%d"))
    
    df_filtered <- df[!(df$month == cutoff_month & df$dom > cutoff_day), ]
    
    m_sum <- aggregate(counter ~ year + month, data = df_filtered, FUN = sum, na.rm = TRUE)
    m_sum$year        <- as.character(m_sum$year)
    m_sum$month_label <- factor(month.abb[m_sum$month], levels = month.abb)
    
    fig <- plot_ly(
      data          = m_sum,
      x             = ~month_label,
      y             = ~counter,
      color         = ~year,
      type          = "bar",
      hovertemplate = paste0("Jahr: %{legendgroup}<br>",
                             "Monat: %{x}<br>",
                             "Summe: %{y:,.0f}<extra></extra>")
    )
    
    fig <- plotly::layout(
      fig,
      barmode = "group",
      title   = paste(
        "Monatssummen –", input$monthly_bar_standort,
        "<br><sup>", month.abb[cutoff_month],
        "nur bis zum", cutoff_day, ". des Monats (alle Jahre)</sup>"
      ),
      xaxis        = list(title = "Monat"),
      yaxis        = list(title = "Summe Radfahrende", separatethousands = TRUE),
      legend       = list(title = list(text = "Jahr")),
      hovermode    = "closest",
      plot_bgcolor  = "#f5f5f5",
      paper_bgcolor = "white"
    )
    
    fig <- plotly::config(
      fig,
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo            = FALSE
    )
    
    return(fig)
  })
  
  # ---- Last 14 Days (base R plot) ----
  output$last14_plot <- renderPlot({
    req(input$last14_station)
    
    df_station      <- subset(bike_counter, Standort == input$last14_station)
    if (nrow(df_station) == 0) return(NULL)
    
    df_station$date        <- as.Date(df_station$date)
    df_station$year        <- lubridate::year(df_station$date)
    df_station$day_of_year <- lubridate::yday(df_station$date)
    
    daily_all <- aggregate(counter ~ year + date + day_of_year,
                           data = df_station, FUN = sum, na.rm = TRUE)
    
    years <- sort(unique(daily_all$year), decreasing = TRUE)
    if (length(years) == 0) return(NULL)
    
    plot_list <- list()
    for (y in years) {
      yr_data <- daily_all[daily_all$year == y, ]
      if (nrow(yr_data) == 0) next
      last_date_y     <- max(yr_data$date, na.rm = TRUE)
      last_complete_y <- last_date_y - 1L
      start_y         <- last_complete_y - 13L
      win_y           <- yr_data[yr_data$date >= start_y & yr_data$date <= last_complete_y, ]
      if (nrow(win_y) == 0) next
      win_y$day_index              <- as.integer(win_y$date - start_y) + 1L
      plot_list[[as.character(y)]] <- win_y
    }
    
    if (length(plot_list) == 0) return(NULL)
    
    year_stats <- lapply(names(plot_list), function(y) {
      d        <- plot_list[[y]]
      d        <- d[order(d$day_index), ]
      last_row <- d[d$day_index == max(d$day_index), ]
      data.frame(year = y, last_value = max(last_row$counter, na.rm = TRUE))
    })
    year_stats    <- do.call(rbind, year_stats)
    year_stats    <- year_stats[order(-year_stats$last_value), ]
    ordered_years <- year_stats$year
    
    max_y <- max(vapply(plot_list, function(d) max(d$counter, na.rm = TRUE), numeric(1)))
    
    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(NA, NA,
         xlim = c(1, 14), ylim = c(0, max_y),
         xlab = "Tag im 14-Tage-Fenster",
         ylab = "Summe Radfahrende pro Tag",
         main = paste("Letzte verfügbaren 14 Tage (minus 1 Tag) –", input$last14_station),
         xaxt = "n")
    axis(1, at = 1:14, labels = 1:14)
    
    cols <- rainbow(length(ordered_years))
    for (i in seq_along(ordered_years)) {
      y <- ordered_years[i]
      d <- plot_list[[y]][order(plot_list[[y]]$day_index), ]
      lines(d$day_index, d$counter, type = "b", col = cols[i], pch = 19)
    }
    
    legend("topleft",
           legend = ordered_years, col = cols, lty = 1, pch = 19,
           title = "Jahr", bg = "white")
  })
  
}