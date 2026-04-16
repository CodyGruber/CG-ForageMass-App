library(shiny)
library(DT)
library(mesonet)
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
      raw <- read.csv(url, na.strings = c("", "M", "T", "None"))
      if (nrow(raw) == 0) return(NULL)
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
        stid       = "STIL",
        start_date = format(start_date, "%Y-%m-%d"),
        end_date   = format(end_date,   "%Y-%m-%d")
      )
      daily <- mesonet::mnet_summarize(raw)
      daily$Date <- as.Date(daily$DATE)
      # mnet_summarize gives TAIR_MAX / TAIR_MIN in °C — convert to °F
      daily$TMAX <- daily$TAIR_MAX * 9/5 + 32
      daily$TMIN <- daily$TAIR_MIN * 9/5 + 32
      daily[, c("Date","TMAX","TMIN")]
    }, error = function(e) {
      message("mesonet pkg error: ", e$message); NULL
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
    df                  <- data()
    df$Date             <- parse_date(df$Date)
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    gdd <- gdd_data()
    req(gdd)
    
    obs <- merge(df, gdd[, c("Date","GDD_cum")], by = "Date", all.x = TRUE)
    obs <- obs[!is.na(obs$GDD_cum) & !is.na(obs$ForageMass_kg_ha), ]
    req(nrow(obs) >= 2)
    
    last_date   <- max(obs$Date)
    future_gdd  <- gdd[gdd$Date > last_date, ]
    
    # Fit linear model: ForageMass ~ GDD_cum (add log transform option)
    fit <- tryCatch(
      lm(ForageMass_kg_ha ~ GDD_cum, data = obs),
      error = function(e) NULL
    )
    req(!is.null(fit))
    
    all_gdd  <- c(obs$GDD_cum, future_gdd$GDD_cum)
    pred_df  <- data.frame(GDD_cum = all_gdd)
    pred_df$ForageMass_pred <- predict(fit, newdata = pred_df)
    
    n_obs   <- nrow(obs)
    xlim    <- range(all_gdd, na.rm = TRUE)
    ylim    <- range(c(obs$ForageMass_kg_ha, pred_df$ForageMass_pred), na.rm = TRUE)
    
    plot(obs$GDD_cum, obs$ForageMass_kg_ha,
         pch = 16, col = "blue",
         xlim = xlim, ylim = ylim,
         xlab = "Cumulative GDD (base 32°F)",
         ylab = "Forage Mass (kg DM/ha)",
         main = "10-Day Forage Mass Forecast")
    
    # Full fitted + forecast line
    ord <- order(pred_df$GDD_cum)
    lines(pred_df$GDD_cum[ord], pred_df$ForageMass_pred[ord],
          col = "gray40", lty = 2, lwd = 1.5)
    
    # Highlight forecast portion
    if (nrow(future_gdd) > 0) {
      fut_pred <- predict(fit, newdata = data.frame(GDD_cum = future_gdd$GDD_cum))
      lines(future_gdd$GDD_cum, fut_pred, col = "red", lwd = 2)
      points(future_gdd$GDD_cum, fut_pred, pch = 17, col = "red", cex = 0.9)
      
      # Annotate last forecast point
      last_i <- which.max(future_gdd$GDD_cum)
      text(future_gdd$GDD_cum[last_i], fut_pred[last_i],
           labels = paste0(round(fut_pred[last_i]), " kg/ha\n",
                           format(max(future_gdd$Date), "%b %d")),
           pos = 3, col = "red", cex = 0.85)
    }
    
    legend("topleft",
           legend = c("Observed", "Fitted", "Forecast (+10 days)"),
           col    = c("blue", "gray40", "red"),
           pch    = c(16, NA, 17),
           lty    = c(NA, 2, 1),
           lwd    = c(NA, 1.5, 2),
           bty    = "n")
    grid()
  })
  
  output$gdd_status <- renderText({ gdd_status() })
}

shinyApp(ui, server)