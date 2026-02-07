ui <- dashboardPage(
  dashboardHeader(title = "BikeCounter Mannheim"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Radzählstellen", tabName = "map", icon = icon("bicycle")),
      menuItem("Standort-Analyse", tabName = "standort_analysis", icon = icon("chart-column")),
      menuItem("Jahresvergleich Kumulativ", tabName = "cumulative", icon = icon("line-chart")),
      menuItem("Rohdaten Explorer", tabName = "raw_data", icon = icon("line-chart")),
      menuItem("Monatsübersicht", tabName = "monthly_table", icon = icon("table"))  # NEW
    )
  ),
  dashboardBody(
    tabItems(
      # Karte
      tabItem(
        tabName = "map",
        leafletOutput("site_map", height = 700)
      ),
      
      # Standort Analysis
      tabItem(
        tabName = "standort_analysis",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "analysis_standort",
              "Standort auswählen:",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel(
                "Jahreswerte",
                plotOutput("analysis_barplot", height = 500)
              ),
              tabPanel(
                "Quartalsverlauf",
                plotOutput("analysis_quarter_ts", height = 500)
              ),
              tabPanel(
                "Monatsverlauf",
                plotOutput("analysis_month_ts", height = 500)
              )
            )
          )
        )
      ),
      
      # Cumulative Comparison
      tabItem(
        tabName = "cumulative",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "cumulative_standort",
              "Standort auswählen:",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotlyOutput("cumulative_plot", height = 700)  # Changed from plotOutput
          )
        )
      ),
      
      
      # Raw Data Explorer
      tabItem(
        tabName = "raw_data",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "raw_standort",
              "Standort auswählen:",
              choices = NULL,
              selected = NULL
            )
          ),
          column(
            width = 4,
            dateRangeInput(
              "raw_daterange",
              "Zeitraum auswählen:",
              start = NULL,
              end = NULL,
              language = "de",
              separator = "bis"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("raw_timeseries", height = 600)
          )
        )
      ),
      
      # NEW: Monthly Overview Table
      tabItem(
        tabName = "monthly_table",
        fluidRow(
          column(
            width = 12,
            h3("Summe der Radfahrenden nach Standort und Monat (letzte 24 Monate)"),
            downloadButton("downloadMonthlyTable", "Download als CSV"),
            br(), br(),
            DTOutput("monthlyStationTable")
          )
        )
      )
    )
  )
)
