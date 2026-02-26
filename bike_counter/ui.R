ui <- dashboardPage(
  dashboardHeader(title = "BikeCounter Mannheim"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Radzählstellen", tabName = "map", icon = icon("bicycle")),
      menuItem("Standort-Analyse", tabName = "standort_analysis", icon = icon("chart-column")),
      menuItem("Letzte 14 Tage", tabName = "last14days", icon = icon("calendar-day")),
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
      
      tabItem(
        tabName = "last14days",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "last14_station",
              "Standort auswählen",
              choices = NULL,
              selected = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("last14_plot", height = 600)
          )
        )
      ),
      
      # Cumulative Comparison
      tabItem(
        tabName = "cumulative",
        tabsetPanel(
          tabPanel(
            "Gesamtes Jahr",
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
                plotlyOutput("cumulative_plot", height = 700)
              )
            )
          ),
          tabPanel(
            "Bis aktueller Tag",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "cumulative_partial_standort",
                  "Standort auswählen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("cumulative_partial_plot", height = 700)
              )
            )
          ),
          tabPanel(
            "Jahresvergleich Balken",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "cumulative_bar_standort",
                  "Standort auswählen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("cumulative_bar_plot", height = 700)
              )
            )
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
      
      # NEW: Monthly Overview Table + Barchart
      tabItem(
        tabName = "monthly_table",
        tabsetPanel(
          tabPanel(
            "Tabelle",
            fluidRow(
              column(
                width = 12,
                h3("Summe der Radfahrenden nach Standort und Monat (letzte 24 Monate)"),
                downloadButton("downloadMonthlyTable", "Download als CSV"),
                br(), br(),
                DTOutput("monthlyStationTable")
              )
            )
          ),
          tabPanel(
            "Monatsbalken",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  "monthly_bar_standort",
                  "Standort auswählen:",
                  choices = NULL,
                  selected = NULL
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                plotlyOutput("monthly_bar_plot", height = 600)
              )
            )
          )
        )
      )
    )
  )
)
