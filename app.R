library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(mesonet)
library(tidyverse)

has_mesonet_pkg <- requireNamespace("mesonet", quietly = TRUE)

# ── Loading modal UI ──────────────────────────────────────────────────────────
loading_modal <- function(msg = "Fetching weather data from Mesonet…") {
  modalDialog(
    title = NULL,
    footer = NULL,
    easyClose = FALSE,
    fade = TRUE,
    tags$div(
      style = "text-align:center; padding: 20px 10px; background-color: #ffffff;",
      tags$div(
        class = "progress",
        style = "height: 8px; margin-bottom: 16px; background-color: #d0eaf5;",
        tags$div(
          class = "progress-bar progress-bar-striped active",
          role  = "progressbar",
          style = "width: 100%; background-color: #2a86c8;",
          `aria-valuenow` = "100",
          `aria-valuemin` = "0",
          `aria-valuemax` = "100"
        )
      ),
      tags$p(
        tags$strong(msg),
        style = "color: #1a6e3c; font-size: 15px; margin: 0;"
      ),
      tags$p(
        "This may take a few seconds…",
        style = "color: #aaa; font-size: 12px; margin-top: 6px;"
      )
    )
  )
}

# ── Plot description text (edit these to change what appears above each plot) ──
PLOT1_DESC <- "This chart shows forage mass over time based on plate meter readings. Each point represents a field measurement converted to kg DM/ha using the formula: (reading × 140) + 500."

PLOT2_DESC <- "This calibration plot relates cumulative growing degree days (GDD, base 32°F) to observed forage mass. The relationship is used to fit a linear model that drives the 10-day forecast."

PLOT3_DESC <- "This forecast combines the observed trend (fitted via GDD) with NOAA NWS 10-day temperature data to project forage mass over the coming days. The dashed blue line represents predicted values beyond the last observation."

plot_description_box <- function(text) {
  tags$div(
    style = paste(
      "background-color: #eef7f0;",
      "border-left: 4px solid #2a86c8;",
      "border-radius: 4px;",
      "padding: 10px 14px;",
      "margin-bottom: 10px;",
      "color: #1a5c38;",
      "font-family: Georgia, serif;",
      "font-size: 14px;",
      "line-height: 1.5;"
    ),
    text
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {
      background-color: #ffffff;
      color: #0d2b1a;
      font-family: Georgia, serif;
    }
    .well {
      background-color: #eef7f0 !important;
      border-color: #9fd4b2 !important;
    }
    h4 {
      color: #1a6e3c;
      font-weight: bold;
    }
    .btn-primary {
      background-color: #2a86c8 !important;
      border-color: #1a6aab !important;
      color: #ffffff !important;
    }
    .btn-primary:hover {
      background-color: #1a6aab !important;
    }
    .btn-danger {
      background-color: #1a6e3c !important;
      border-color: #145730 !important;
      color: #ffffff !important;
    }
    .btn-danger:hover {
      background-color: #145730 !important;
    }
    .shiny-input-container label {
      color: #1a5c38;
      font-weight: bold;
    }
    pre {
      background-color: #eef7f0;
      border: 1px solid #9fd4b2;
      color: #1a5c38;
    }
    .modal-content {
      border: 2px solid #2a86c8;
      border-radius: 8px;
    }
    hr {
      border-color: #9fd4b2;
    }
    .dataTables_wrapper {
      color: #0d2b1a;
    }
  "))),
  titlePanel(
    tags$span("Forage Mass Tracker & GDD Calibration",
              style = "color: #1a6e3c; font-weight: bold;")
  ),
  
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
      
      h4("10-Day GDD & Forage Forecast"),
      DTOutput("forecast_table"),
      
      plot_description_box(PLOT1_DESC),
      plotOutput("plot1"),
      
      plot_description_box(PLOT2_DESC),
      plotOutput("plot2"),
      
      plot_description_box(PLOT3_DESC),
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
    
    src_label <- if (has_mesonet_pkg) "Mesonet package" else "IEM API"
    showModal(loading_modal(
      paste0("Fetching weather data via ", src_label, "…")
    ))
    gdd_status("Fetching weather data…")
    on.exit(removeModal(), add = TRUE)
    
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
  
  # ── Forecast data ─────────────────────────────────────────────────────────
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
  output$forecast_table <- renderDT({
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
      options   = list(pageLength = 10, dom = "t"),
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
         type = "b", pch = 16, col = "#2a86c8", lwd = 2,
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
         type = "b", pch = 16, col = "#1a6e3c",
         xlab = "Cumulative GDD (base 32°F)", ylab = "Forage Mass (kg DM/ha)",
         main = "Calibration Plot - Forage Mass vs Growing Degree Days")
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
    
    fd <- forecast_data()
    req(fd)
    
    trend_dates  <- c(obs$Date[order(obs$Date)],        fd$Date)
    trend_forage <- c(obs$Forage_fitted[order(obs$Date)], fd$Forage_pred)
    
    all_dates  <- c(obs$Date, fd$Date)
    all_forage <- c(obs$ForageMass_kg_ha, trend_forage)
    xlim <- range(all_dates,  na.rm = TRUE)
    ylim <- range(all_forage, na.rm = TRUE) * c(0.95, 1.05)
    
    plot(obs$Date, obs$ForageMass_kg_ha,
         pch = 16, col = "#2a86c8", xlim = xlim, ylim = ylim,
         xlab = "Date", ylab = "Forage Mass (kg DM/ha)",
         main = "10-Day Forage Forecast (NOAA NWS)")
    
    last_obs_date      <- max(obs$Date)
    obs_idx            <- trend_dates <= last_obs_date
    fcast_idx          <- trend_dates >= last_obs_date
    
    lines(trend_dates[obs_idx],   trend_forage[obs_idx],
          col = "#1a6e3c", lwd = 2, lty = 1)
    lines(trend_dates[fcast_idx], trend_forage[fcast_idx],
          col = "#145aab", lwd = 2, lty = 2)
    points(fd$Date, fd$Forage_pred, col = "#145aab", pch = 17, cex = 0.8)
    
    legend("topleft",
           legend = c("Observed", "Fitted trend", "Forecast"),
           col    = c("#2a86c8", "#1a6e3c", "#145aab"),
           pch    = c(16, NA, 17),
           lty    = c(NA, 1, 2),
           lwd    = c(NA, 2, 2),
           bty    = "n")
    grid()
  })
  
  output$gdd_status <- renderText({ gdd_status() })
}

shinyApp(ui, server)