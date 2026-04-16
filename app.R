library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(mesonet)
library(tidyverse)
# Use mesonet package if available, otherwise fall back to IEM API
has_mesonet_pkg <- requireNamespace("mesonet", quietly = TRUE)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Forage Mass Tracker & GDD Calibration"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Manual Entry"),
      textInput("Date", "Date (YYYY-MM-DD or MM/DD/YYYY)"),
      numericInput("AvgPlateMeterReading", "Avg Plate Meter Reading", value = NA),
      actionButton("add_row",    "Add Row",            class = "btn-primary"),
      actionButton("delete_rows","Delete Selected Rows", class = "btn-danger"),
      tags$hr(),
      
      h4("Upload CSV"),
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept   = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep",   "Separator",
                   choices  = c(Comma=",", Semicolon=";", Tab="\t"), selected = ","),
      radioButtons("quote", "Quote",
                   choices  = c(None="", "Double Quote"='"', "Single Quote"="'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp","Display",
                   choices  = c(Head="head", All="all"), selected = "head"),
      tags$hr(),
      verbatimTextOutput("gdd_status")
    ),
    
    mainPanel(
      DTOutput("contents"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")   # NEW: forecast plot
    )
  )
)

get_nws_forecast <- function(lat = 36.1156, lon = -97.0584) {
  
  # Step 1: get forecast URL from gridpoint API
  point_url <- paste0(
    "https://api.weather.gov/points/",
    lat, ",", lon
  )
  
  res <- httr::GET(point_url, user_agent("shiny-app"))
  stop_for_status(res)
  
  point_data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  
  forecast_url <- point_data$properties$forecast
  
  # Step 2: pull forecast
  forecast_res <- httr::GET(forecast_url, user_agent("shiny-app"))
  stop_for_status(forecast_res)
  
  forecast_json <- jsonlite::fromJSON(
    httr::content(forecast_res, "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
  
  periods <- forecast_json$properties$periods
  
  # Step 3: extract relevant fields
  df <- data.frame(
    name = periods$name,
    startTime = as.POSIXct(periods$startTime, tz = "UTC"),
    temp = periods$temperature,
    wind = periods$windSpeed,
    forecast = periods$detailedForecast,
    stringsAsFactors = FALSE
  )
  
  # Step 4: split into daily max/min approximation
  df$date <- as.Date(df$startTime)
  
  # NWS alternates day/night periods → approximate daily max/min
  daily <- aggregate(temp ~ date, df, function(x) c(max = max(x), min = min(x)))
  
  daily <- do.call(data.frame, daily)
  names(daily) <- c("Date", "TMAX", "TMIN")
  
  return(daily)
}

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  parse_date <- function(x) {
    as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
  }
  
  # ── Reactive data store ──────────────────────────────────────────────────
  data <- reactiveVal(data.frame(
    Date                 = character(),
    AvgPlateMeterReading = numeric()
  ))
  
  observeEvent(input$delete_rows, {
    req(input$contents_rows_selected)
    df <- data()
    df <- df[-input$contents_rows_selected, ]
    data(df)
  })
  
  observeEvent(input$add_row, {
    req(input$Date, input$AvgPlateMeterReading)
    df  <- data()
    new <- data.frame(Date = input$Date,
                      AvgPlateMeterReading = input$AvgPlateMeterReading)
    data(rbind(df, new))
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    df_list <- lapply(input$file1$datapath, function(p) {
      read.csv(p, header = input$header, sep = input$sep, quote = input$quote)
    })
    data(do.call(rbind, df_list))
  })
  
  observeEvent(input$contents_cell_edit, {
    info <- input$contents_cell_edit
    df   <- data()
    df[info$row, info$col] <- info$value
    data(df)
  })
  
  # ── GDD fetch ────────────────────────────────────────────────────────────
  gdd_status <- reactiveVal("No data loaded yet.")
  
  fetch_iem <- function(start_date, end_date) {
    url <- paste0(
      "https://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?",
      "network=OK_MESONET",
      "&stations=STIL",
      "&var=max_tmpf,min_tmpf",
      "&sts=", format(start_date, "%Y-%m-%d"),
      "&ets=", format(end_date,   "%Y-%m-%d"),
      "&format=csv"
    )
    tryCatch({
      raw <- dplyr::tibble(read.csv(url, na.strings = c("", "M", "T", "None")),
      if (nrow(raw) == 0) return(NULL))
      # IEM daily.py returns columns: station, day, max_tmpf, min_tmpf
      raw$Date <- as.Date(raw$day)
      raw$TMAX  <- as.numeric(raw$max_tmpf)
      raw$TMIN  <- as.numeric(raw$min_tmpf)
      raw[, c("Date", "TMAX", "TMIN")]
    }, error = function(e) {
      message("IEM fetch error: ", e$message)
      NULL
    })
  }
  
  fetch_mesonet_pkg <- function(start_date, end_date) {
    tryCatch({
      
      raw <- mesonet::mnet_retrieve(
        stid = "STIL",
        start_date = format(start_date, "%Y-%m-%d"),
        end_date = format(end_date, "%Y-%m-%d")
      )
      
      daily <- mesonet::mnet_summarize(
        sub_daily = raw,
        tz = "Etc/GMT+6",
        interval = "1 day",
        include_qc_variables = FALSE
      )
      
      # 🔥 DEBUG (keep this once while testing)
      print(names(daily))
      
      # ---- FIX 1: detect date column safely ----
      date_col <- grep("date", names(daily), ignore.case = TRUE, value = TRUE)[1]
      if (is.na(date_col)) stop("No date column found in mesonet output")
      
      daily$Date <- as.Date(daily[[date_col]])
      
      # ---- FIX 2: detect temp columns safely ----
      tmax_col <- grep("max", names(daily), ignore.case = TRUE, value = TRUE)[1]
      tmin_col <- grep("min", names(daily), ignore.case = TRUE, value = TRUE)[1]
      
      if (is.na(tmax_col) || is.na(tmin_col)) {
        stop("Temperature columns not found in mesonet output")
      }
      
      # ---- FIX 3: convert safely ----
      daily$TMAX <- as.numeric(daily[[tmax_col]]) * 9/5 + 32
      daily$TMIN <- as.numeric(daily[[tmin_col]]) * 9/5 + 32
      
      # ---- FIX 4: prevent silent row loss ----
      daily <- daily[!is.na(daily$TMAX) & !is.na(daily$TMIN), ]
      
      if (nrow(daily) == 0) {
        stop("All rows removed after NA filtering — check column mapping")
      }
      
      daily[, c("Date", "TMAX", "TMIN")]
      
    }, error = function(e) {
      message("mesonet pkg error: ", e$message)
      NULL
    })
  }
  
  gdd_data <- reactive({
    req(data())
    df         <- data()
    df$Date    <- parse_date(df$Date)
    df         <- df[!is.na(df$Date), ]
    req(nrow(df) > 0)
    
    start_date <- min(df$Date)
    # Extend 10 days past last entry for forecast window
    end_date   <- max(df$Date) + 10
    
    gdd_status("Fetching weather data from Mesonet…")
    
    weather <- if (has_mesonet_pkg) {
      fetch_mesonet_pkg(start_date, end_date)
    } else {
      fetch_iem(start_date, end_date)
    }
    
    if (is.null(weather) || nrow(weather) == 0) {
      gdd_status("⚠ Could not retrieve weather data. Check internet connection.")
      return(NULL)
    }
    
    weather <- weather[weather$Date >= start_date & weather$Date <= end_date, ]
    weather <- weather[!is.na(weather$TMAX) & !is.na(weather$TMIN), ]
    
    if (nrow(weather) == 0) {
      gdd_status("⚠ Weather data returned but no rows matched date range.")
      return(NULL)
    }
    
    weather <- weather[order(weather$Date), ]
    weather$GDD     <- pmax(((weather$TMAX + weather$TMIN) / 2) - 32, 0)
    weather$GDD_cum <- cumsum(weather$GDD)
    
    source_label <- if (has_mesonet_pkg) "mesonet pkg" else "IEM API"
    gdd_status(paste0("✓ ", nrow(weather), " days of weather loaded via ", source_label,
                      "\n  Period: ", format(min(weather$Date)), " – ",
                      format(max(weather$Date))))
    weather[, c("Date","GDD","GDD_cum")]
  })
  
  # ── Table ────────────────────────────────────────────────────────────────
  output$contents <- renderDT({
    req(data())
    df         <- data()
    df$Date    <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    gdd <- gdd_data()
    if (!is.null(gdd)) {
      df <- merge(df, gdd[, c("Date","GDD","GDD_cum")], by = "Date", all.x = TRUE)
    }
    if (input$disp == "head") df <- head(df)
    datatable(df, editable = TRUE, selection = "multiple")
  })
  
  # ── Plot 1: Forage mass over time ────────────────────────────────────────
  output$plot1 <- renderPlot({
    req(data())
    df                  <- data()
    df$Date             <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    df                  <- df[order(df$Date), ]
    plot(df$Date, df$ForageMass_kg_ha,
         type = "b", pch = 16, col = "darkgreen", lwd = 2,
         xlab = "Date", ylab = "Forage Mass (kg DM/ha)",
         main = "Forage Mass Over Time")
    grid()
  })
  
  # ── Plot 2: Forage mass vs cumulative GDD ───────────────────────────────
  output$plot2 <- renderPlot({
    req(data())
    df                  <- data()
    df$Date             <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    gdd <- gdd_data()
    req(gdd)
    df <- merge(df, gdd[, c("Date","GDD_cum")], by = "Date", all.x = TRUE)
    df <- df[order(df$GDD_cum), ]
    plot(df$GDD_cum, df$ForageMass_kg_ha,
         type = "b", pch = 16, col = "blue",
         xlab = "Cumulative GDD (base 32°F)",
         ylab = "Forage Mass (kg DM/ha)",
         main = "Forage Mass vs Growing Degree Days")
    grid()
  })
  
  # ── Plot 3: 10-day forecast ──────────────────────────────────────────────
  output$plot3 <- renderPlot({
    
    req(data())
    
    df <- data()
    df$Date <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    
    gdd <- gdd_data()
    req(gdd)
    
    obs <- merge(df, gdd[, c("Date","GDD_cum")], by = "Date", all.x = TRUE)
    obs <- obs[complete.cases(obs[, c("Date","GDD_cum","ForageMass_kg_ha")]), ]
    
    req(nrow(obs) >= 2)
    
    # ---- Fit model
    fit <- lm(ForageMass_kg_ha ~ GDD_cum, data = obs)
    a <- coef(fit)[1]
    b <- coef(fit)[2]
    
    # ---- NOAA forecast
    forecast_weather <- tryCatch({
      get_nws_forecast()
    }, error = function(e) NULL)
    
    if (is.null(forecast_weather) || nrow(forecast_weather) == 0) {
      plot(obs$Date, obs$ForageMass_kg_ha,
           pch = 16, col = "blue",
           main = "NOAA Forecast Failed (Observed Only)",
           xlab = "Date", ylab = "Forage Mass")
      return()
    }
    
    # ---- Remove NA temps (critical fix)
    forecast_weather <- forecast_weather[
      complete.cases(forecast_weather[, c("TMAX","TMIN")]),
    ]
    
    if (nrow(forecast_weather) == 0) return()
    
    # ---- Compute forecast GDD correctly
    forecast_weather$GDD <- pmax(
      ((forecast_weather$TMAX + forecast_weather$TMIN)/2) - 32,
      0
    )
    
    # ---- FIX: safe cumulative baseline
    last_gdd <- max(obs$GDD_cum, na.rm = TRUE)
    if (is.infinite(last_gdd)) last_gdd <- 0
    
    forecast_weather$GDD_cum <- last_gdd + cumsum(forecast_weather$GDD)
    
    # ---- Predict forage
    forecast_weather$Forage_pred <- predict(fit,
                                            newdata = data.frame(
                                              GDD_cum = forecast_weather$GDD_cum
                                            ))
    
    # ---- Plot
    plot(obs$Date, obs$ForageMass_kg_ha,
         pch = 16, col = "blue",
         xlab = "Date",
         ylab = "Forage Mass (kg DM/ha)",
         main = "10-Day Forage Forecast (NOAA NWS)")
    
    lines(forecast_weather$Date, forecast_weather$Forage_pred,
          col = "red", lwd = 2)
    
    points(forecast_weather$Date, forecast_weather$Forage_pred,
           col = "red", pch = 17)
    
    legend("topleft",
           legend = c("Observed", "Forecast"),
           col = c("blue", "red"),
           pch = c(16, 17),
           lty = c(NA, 1),
           bty = "n")
    
    grid()
  })
  
  output$gdd_status <- renderText({ gdd_status() })
}

shinyApp(ui, server)