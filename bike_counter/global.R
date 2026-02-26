library(shiny)
library(shinydashboard)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)
library(httr)
library(jsonlite)

# ── Load .Renviron ────────────────────────────────────────────
if (file.exists(".Renviron")) {
  message("✓ .Renviron found at: ", normalizePath(".Renviron"))
  readRenviron(".Renviron")
} else {
  message("✗ .Renviron NOT found — working dir is: ", getwd())
}

# ── Load credentials ──────────────────────────────────────────
host         <- Sys.getenv("DATABRICKS_HOST")
token        <- Sys.getenv("DATABRICKS_TOKEN")
http_path    <- Sys.getenv("DATABRICKS_HTTP_PATH")
warehouse_id <- basename(http_path)

message("HOST: '", host, "'")
message("WAREHOUSE_ID: '", warehouse_id, "'")

if (any(nchar(c(host, token, http_path)) == 0)) {
  stop("Missing Databricks credentials. Check .Renviron for DATABRICKS_HOST, DATABRICKS_TOKEN, DATABRICKS_HTTP_PATH.")
}

# ── Load bike counter data (live Databricks with RDS fallback) ─
bike_counter <- tryCatch({
  
  res <- POST(
    url    = paste0("https://", host, "/api/2.0/sql/statements"),
    add_headers(Authorization = paste("Bearer", token)),
    content_type_json(),
    body = toJSON(list(
      warehouse_id = warehouse_id,
      statement    = "SELECT date, counter, Standort FROM counter.mannheim.bike_counter",
      wait_timeout = "50s",
      format       = "JSON_ARRAY"
    ), auto_unbox = TRUE),
    httr::timeout(60)
  )
  
  message("HTTP status: ", status_code(res))
  
  result <- content(res, as = "parsed")
  message("Statement state: ", result$status$state)
  
  if (result$status$state != "SUCCEEDED") {
    stop("Query did not succeed: ", result$status$state)
  }
  
  statement_id <- result$statement_id
  total_chunks <- result$manifest$total_chunk_count
  cols         <- sapply(result$manifest$schema$columns, `[[`, "name")
  
  message("Total chunks: ", total_chunks)
  message("Total rows expected: ", result$manifest$total_row_count)
  
  # ── Collect all chunks ───────────────────────────────────────
  all_rows <- list()
  
  # Chunk 0 is embedded in the initial response
  all_rows[[1]] <- result$result$data_array
  
  # Fetch remaining chunks (1, 2, 3, ...)
  if (total_chunks > 1) {
    for (chunk_index in 1:(total_chunks - 1)) {
      message("Fetching chunk ", chunk_index, " of ", total_chunks - 1, " ...")
      chunk_res <- GET(
        url = paste0("https://", host, "/api/2.0/sql/statements/", statement_id,
                     "/result/chunks/", chunk_index),
        add_headers(Authorization = paste("Bearer", token)),
        httr::timeout(60)
      )
      chunk_data <- content(chunk_res, as = "parsed")
      all_rows[[chunk_index + 1]] <- chunk_data$data_array
    }
  }
  
  # Flatten all chunks into one list of rows
  data <- do.call(c, all_rows)
  message("Total rows collected: ", length(data))
  
  # ── Build dataframe ──────────────────────────────────────────
  rows <- lapply(data, function(row) {
    vapply(row, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
  })
  
  df           <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- cols
  df$date      <- lubridate::ymd_hms(df$date, tz = "UTC")
  df$counter   <- as.numeric(df$counter)
  
  message("✓ Live data loaded. Standorte: ", length(unique(df$Standort)))
  message(">>> Standorte: ", paste(unique(df$Standort), collapse = ", "))
  df
  
}, error = function(e) {
  message("======================")
  message("FULL ERROR: ", conditionMessage(e))
  message("ERROR CLASS: ", paste(class(e), collapse = ", "))
  message("======================")
  message("Falling back to cached RDS")
  rds <- readRDS("data/bike_counter_data.rds")
  rds$date <- lubridate::ymd_hms(as.character(rds$date), tz = "UTC")
  rds
})

message(">>> bike_counter rows: ", nrow(bike_counter))
message(">>> bike_counter Standorte: ", paste(unique(bike_counter$Standort), collapse = ", "))

# ── Static site metadata ──────────────────────────────────────
sites <- read_delim("data/site.csv", delim = ";", show_col_types = FALSE)

sites_unique <- sites %>%
  select(name, latitude, longitude) %>%
  distinct()
