#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(httr)
library(readr)
library(jsonlite)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Data Upload/Entry"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #manual input
      textInput("Date", "Date"),
      numericInput("AvgPlateMeterReading", "AvgPlateMeterReading", value = NA),
      actionButton("add_row", "Add Row"),
      
      #deletebutton
      actionButton("delete_rows", "Delete Selected Rows"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      DTOutput("contents"),
      plotOutput("plot1"),
      plotOutput("plot2")
      
    )
    
  )
)

# Define server logic to read selected file ----

  # GDD reactive
  server <- function(input, output) {

  parse_date <- function(x) {
    as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y"))
  }

  data <- reactiveVal(data.frame(
    Date = character(),
    AvgPlateMeterReading = numeric()
  ))

  observeEvent(input$delete_rows, {
    req(input$contents_rows_selected)

    df <- data()
    df <- df[-input$contents_rows_selected, ]
    data(df)
  })

  observeEvent(input$add_row, {
    df <- data()

    new_row <- data.frame(
      Date = input$Date,
      AvgPlateMeterReading = input$AvgPlateMeterReading
    )

    data(rbind(df, new_row))
  })

  observeEvent(input$file1, {
    req(input$file1)

    df_list <- lapply(input$file1$datapath, function(path) {
      read.csv(path,
               header = input$header,
               sep = input$sep,
               quote = input$quote)
    })

    data(do.call(rbind, df_list))
  })

  gdd_data <- reactive({
    
    req(data())
    
    df <- data()
    df$Date <- parse_date(df$Date)
    df <- df[!is.na(df$Date), ]
    
    req(nrow(df) > 0)
    
    start_date <- min(df$Date)
    end_date   <- max(df$Date)
    
    year <- format(start_date, "%Y")
    
    url <- paste0(
      "https://mesonet.org/data/public/mesonet/mts/",
      year, "/",
      year, "mesonet.txt"
    )
    raw <- tryCatch({
      read.table(url, header = TRUE)
    }, error = function(e) {
      message("Download error: ", e$message)
      return(NULL)
    })
    
    if (is.null(raw)) return(NULL)
    
    # Filter for station STIL (Stillwater)
    raw <- raw[raw$STID == "STIL", ]
    
    # Convert date
    raw$Date <- as.Date(raw$YYYYMMDD, format = "%Y%m%d")
    
    # Filter to your date range
    raw <- raw[raw$Date >= start_date & raw$Date <= end_date, ]
    
    if (nrow(raw) == 0) return(NULL)
    
    # Use TMAX and TMIN directly
    raw$GDD <- pmax(((raw$TMAX + raw$TMIN) / 2) - 32, 0)
    raw$GDD_cum <- cumsum(raw$GDD)
    
    raw[, c("Date", "GDD", "GDD_cum")]
  })
  output$contents <- renderDT({

    req(data())

    df <- data()
    df$Date <- parse_date(df$Date)

    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500

    gdd <- gdd_data()

    if (!is.null(gdd)) {
      df <- merge(df, gdd[, c("Date", "GDD", "GDD_cum")],
                  by = "Date", all.x = TRUE)
    }

    datatable(df, editable = TRUE, selection = "multiple")
  })

  observeEvent(input$contents_cell_edit, {
    info <- input$contents_cell_edit
    df <- data()

    df[info$row, info$col] <- info$value
    data(df)
  })

  output$plot1 <- renderPlot({

    req(data())

    df <- data()
    df$Date <- parse_date(df$Date)

    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500

    df <- df[order(df$Date), ]

    plot(df$Date,
         df$ForageMass_kg_ha,
         type = "l",
         col = "darkgreen",
         lwd = 2,
         xlab = "Date",
         ylab = "Forage Mass (kg/ha)",
         main = "Forage Mass Over Time")
  })

  output$plot2 <- renderPlot({

    req(data())

    df <- data()
    df$Date <- parse_date(df$Date)

    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500

    gdd <- gdd_data()
    req(gdd)

    df <- merge(df, gdd[, c("Date", "GDD_cum")],
                by = "Date", all.x = TRUE)

    df <- df[order(df$GDD_cum), ]

    plot(df$GDD_cum,
         df$ForageMass_kg_ha,
         type = "b",
         pch = 16,
         col = "blue",
         xlab = "Cumulative GDD (base 32°F)",
         ylab = "Forage Mass (kg/ha)",
         main = "Forage Mass vs Growing Degree Days")
  })
}
# Create Shiny app ----
shinyApp(ui, server)