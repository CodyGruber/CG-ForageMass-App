library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(mesonet)
library(tidyverse)

has_mesonet_pkg <- requireNamespace("mesonet", quietly = TRUE)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Forage Mass Tracker & GDD Calibration"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Manual Entry"),
      textInput("Date", "Date (YYYY-MM-DD or MM/DD/YYYY)"),
      numericInput("AvgPlateMeterReading", "Avg Plate Meter Reading", value = NA),
      actionButton("add_row",     "Add Row",             class = "btn-primary"),
      actionButton("delete_rows", "Delete Selected Rows", class = "btn-danger"),
      tags$hr(),
      
      h4("Upload CSV"),
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept   = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep",   "Separator",
                   choices = c(Comma=",", Semicolon=";", Tab="\t"), selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None="", "Double Quote"='"', "Single Quote"="'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head="head", All="all"), selected = "head"),
      tags$hr(),
      verbatimTextOutput("gdd_status")
    ),
    
    mainPanel(
      h4("Observed Data"),
      DTOutput("contents"),
      
      h4("10-Day GDD & Forage Forecast"),       # NEW
      DTOutput("forecast_table"),               # NEW
      
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

# ── NWS forecast helper ───────────────────────────────────────────────────────
get_nws_forecast <- function(lat = 36.1156, lon = -97.0584) {
  point_url  <- paste0("https://api.weather.gov/points/", lat, ",", lon)
  res        <- httr::GET(point_url, user_agent("shiny-app"))
  stop_for_status(res)
  point_data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  
  forecast_url <- point_data$properties$forecast
  forecast_res <- httr::GET(forecast_url, user_agent("shiny-app"))
  stop_for_status(forecast_res)
  
  forecast_json <- jsonlite::fromJSON(
    httr::content(forecast_res, "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
  
  periods     <- forecast_json$properties$periods
  df          <- data.frame(
    startTime = as.POSIXct(periods$startTime, tz = "UTC"),
    temp      = periods$temperature,
    stringsAsFactors = FALSE
  )
  df$date     <- as.Date(df$startTime)
  
  daily       <- aggregate(temp ~ date, df, function(x) c(max(x), min(x)))
  daily       <- do.call(data.frame, daily)
  names(daily) <- c("Date", "TMAX", "TMIN")
  return(daily)
}

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  parse_date <- function(x) {
    as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
  }
  
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
      "network=OK_MESONET&stations=STIL&var=max_tmpf,min_tmpf",
      "&sts=", format(start_date, "%Y-%m-%d"),
      "&ets=", format(end_date,   "%Y-%m-%d"),
      "&format=csv"
    )
    tryCatch({
      raw       <- read.csv(url, na.strings = c("", "M", "T", "None"))
      if (nrow(raw) == 0) return(NULL)
      raw$Date  <- as.Date(raw$day)
      raw$TMAX  <- as.numeric(raw$max_tmpf)
      raw$TMIN  <- as.numeric(raw$min_tmpf)
      raw[, c("Date", "TMAX", "TMIN")]
    }, error = function(e) { message("IEM fetch error: ", e$message); NULL })
  }
  
  fetch_mesonet_pkg <- function(start_date, end_date) {
    tryCatch({
      raw   <- mesonet::mnet_retrieve(
        stid = "STIL",
        start_date = format(start_date, "%Y-%m-%d"),
        end_date   = format(end_date,   "%Y-%m-%d")
      )
      daily <- mesonet::mnet_summarize(raw, tz = "Etc/GMT+6",
                                       interval = "1 day",
                                       include_qc_variables = FALSE)
      date_col <- grep("date", names(daily), ignore.case = TRUE, value = TRUE)[1]
      tmax_col <- grep("max",  names(daily), ignore.case = TRUE, value = TRUE)[1]
      tmin_col <- grep("min",  names(daily), ignore.case = TRUE, value = TRUE)[1]
      if (any(is.na(c(date_col, tmax_col, tmin_col)))) stop("Column detection failed")
      daily$Date <- as.Date(daily[[date_col]])
      daily$TMAX <- as.numeric(daily[[tmax_col]]) * 9/5 + 32
      daily$TMIN <- as.numeric(daily[[tmin_col]]) * 9/5 + 32
      daily <- daily[!is.na(daily$TMAX) & !is.na(daily$TMIN), ]
      if (nrow(daily) == 0) stop("No rows after NA filter")
      daily[, c("Date", "TMAX", "TMIN")]
    }, error = function(e) { message("mesonet pkg error: ", e$message); NULL })
  }
  
  gdd_data <- reactive({
    req(data())
    df      <- data()
    df$Date <- parse_date(df$Date)
    df      <- df[!is.na(df$Date), ]
    req(nrow(df) > 0)
    
    start_date <- min(df$Date)
    end_date   <- max(df$Date) + 10
    gdd_status("Fetching weather data from Mesonet…")
    
    weather <- if (has_mesonet_pkg) fetch_mesonet_pkg(start_date, end_date) else
      fetch_iem(start_date, end_date)
    
    if (is.null(weather) || nrow(weather) == 0) {
      gdd_status("⚠ Could not retrieve weather data."); return(NULL)
    }
    weather <- weather[weather$Date >= start_date & weather$Date <= end_date, ]
    weather <- weather[!is.na(weather$TMAX) & !is.na(weather$TMIN), ]
    if (nrow(weather) == 0) {
      gdd_status("⚠ No rows matched date range."); return(NULL)
    }
    weather         <- weather[order(weather$Date), ]
    weather$GDD     <- pmax(((weather$TMAX + weather$TMIN) / 2) - 32, 0)
    weather$GDD_cum <- cumsum(weather$GDD)
    
    src <- if (has_mesonet_pkg) "mesonet pkg" else "IEM API"
    gdd_status(paste0("✓ ", nrow(weather), " days loaded via ", src,
                      "\n  Period: ", min(weather$Date), " – ", max(weather$Date)))
    weather[, c("Date", "GDD", "GDD_cum")]
  })
  
  # ── Forecast data (reactive so both table and plot share it) ─────────────
  forecast_data <- reactive({
    req(data())
    df      <- data()
    df$Date <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    
    gdd <- gdd_data()
    req(gdd)
    
    obs <- merge(df, gdd[, c("Date","GDD_cum")], by = "Date", all.x = TRUE)
    obs <- obs[complete.cases(obs[, c("Date","GDD_cum","ForageMass_kg_ha")]), ]
    req(nrow(obs) >= 2)
    
    fit <- lm(ForageMass_kg_ha ~ GDD_cum, data = obs)
    
    fw <- tryCatch(get_nws_forecast(), error = function(e) NULL)
    req(!is.null(fw) && nrow(fw) > 0)
    
    fw <- fw[complete.cases(fw[, c("TMAX","TMIN")]), ]
    req(nrow(fw) > 0)
    
    last_gdd <- max(obs$GDD_cum, na.rm = TRUE)
    if (is.infinite(last_gdd) || is.na(last_gdd)) last_gdd <- 0
    
    fw$GDD_daily  <- pmax(((fw$TMAX + fw$TMIN) / 2) - 32, 0)
    fw$GDD_cum    <- last_gdd + cumsum(fw$GDD_daily)
    fw$Forage_pred <- predict(fit, newdata = data.frame(GDD_cum = fw$GDD_cum))
    
    fw
  })
  
  # ── Forecast table ────────────────────────────────────────────────────────
  output$forecast_table <- renderDT({             # NEW
    fd <- forecast_data()
    req(fd)
    
    display <- data.frame(
      Date              = format(fd$Date, "%Y-%m-%d"),
      TMAX_F            = round(fd$TMAX, 1),
      TMIN_F            = round(fd$TMIN, 1),
      GDD_Daily         = round(fd$GDD_daily, 1),
      GDD_Cumulative    = round(fd$GDD_cum,   1),
      Forage_Pred_kg_ha = round(fd$Forage_pred, 0)
    )
    
    datatable(
      display,
      rownames  = FALSE,
      options   = list(pageLength = 10, dom = "t"),  # "t" = table only, no search box
      colnames  = c("Date", "Max Temp (°F)", "Min Temp (°F)",
                    "Daily GDD", "Cumulative GDD", "Predicted Forage (kg/ha)")
    )
  })
  
  # ── Table ─────────────────────────────────────────────────────────────────
  output$contents <- renderDT({
    req(data())
    df      <- data()
    df$Date <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    gdd <- gdd_data()
    if (!is.null(gdd))
      df <- merge(df, gdd[, c("Date","GDD","GDD_cum")], by = "Date", all.x = TRUE)
    if (input$disp == "head") df <- head(df)
    datatable(df, editable = TRUE, selection = "multiple")
  })
  
  # ── Plot 1 ────────────────────────────────────────────────────────────────
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
  
  # ── Plot 2 ────────────────────────────────────────────────────────────────
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
         xlab = "Cumulative GDD (base 32°F)", ylab = "Forage Mass (kg DM/ha)",
         main = "Forage Mass vs Growing Degree Days")
    grid()
  })
  
  # ── Plot 3 ────────────────────────────────────────────────────────────────
  output$plot3 <- renderPlot({
    req(data())
    df      <- data()
    df$Date <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    
    gdd <- gdd_data()
    req(gdd)
    
    obs <- merge(df, gdd[, c("Date","GDD_cum")], by = "Date", all.x = TRUE)
    obs <- obs[complete.cases(obs[, c("Date","GDD_cum","ForageMass_kg_ha")]), ]
    req(nrow(obs) >= 2)
    
    fit               <- lm(ForageMass_kg_ha ~ GDD_cum, data = obs)
    obs$Forage_fitted <- predict(fit, newdata = data.frame(GDD_cum = obs$GDD_cum))
    
    fd <- forecast_data()        # reuse the shared reactive — no duplicated fetch
    req(fd)
    
    trend_dates  <- c(obs$Date[order(obs$Date)],        fd$Date)
    trend_forage <- c(obs$Forage_fitted[order(obs$Date)], fd$Forage_pred)
    
    all_dates  <- c(obs$Date, fd$Date)
    all_forage <- c(obs$ForageMass_kg_ha, trend_forage)
    xlim <- range(all_dates,  na.rm = TRUE)
    ylim <- range(all_forage, na.rm = TRUE) * c(0.95, 1.05)
    
    plot(obs$Date, obs$ForageMass_kg_ha,
         pch = 16, col = "blue", xlim = xlim, ylim = ylim,
         xlab = "Date", ylab = "Forage Mass (kg DM/ha)",
         main = "10-Day Forage Forecast (NOAA NWS)")
    
    last_obs_date      <- max(obs$Date)
    obs_idx            <- trend_dates <= last_obs_date
    fcast_idx          <- trend_dates >= last_obs_date
    
    lines(trend_dates[obs_idx],   trend_forage[obs_idx],
          col = "darkgreen", lwd = 2, lty = 1)
    lines(trend_dates[fcast_idx], trend_forage[fcast_idx],
          col = "red", lwd = 2, lty = 2)
    points(fd$Date, fd$Forage_pred, col = "red", pch = 17, cex = 0.8)
    
    legend("topleft",
           legend = c("Observed", "Fitted trend", "Forecast"),
           col    = c("blue", "darkgreen", "red"),
           pch    = c(16, NA, 17),
           lty    = c(NA, 1, 2),
           lwd    = c(NA, 2, 2),
           bty    = "n")
    grid()
  })
  
  output$gdd_status <- renderText({ gdd_status() })
}

shinyApp(ui, server)