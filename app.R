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
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
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
server <- function(input, output) {
  
  data <- reactiveVal(data.frame(
    Date = character(),
    AvgPlateMeterReading = numeric()
  ))
  
  observeEvent(input$delete_rows, {
    req(input$contents_rows_selected)
    
    df <- data()
    
    # remove selected rows
    df <- df[-input$contents_rows_selected, ]
    
    data(df)
  })
  
  # GDD reactive
  gdd_data <- reactive({
    req(data())
    
    df <- data()
    
    start_date <- min(as.Date(df$Date), na.rm = TRUE)
    end_date   <- max(as.Date(df$Date), na.rm = TRUE)
    
    dates <- seq(start_date, end_date, by = "day")
    
    all_data <- lapply(dates, function(d) {
      url <- paste0(
        "https://www.mesonet.org/data/public/mesonet/mts/STIL/",
        format(d, "%m/%d/%Y"),
        ".mts"
      )
      
      tryCatch({
        read_table(url, col_names = FALSE, skip = 2)
      }, error = function(e) NULL)
    })
    
    all_data <- do.call(rbind, all_data)
    if (is.null(all_data)) return(NULL)
    
    colnames(all_data)[c(1, 9, 10)] <- c("DateTime", "Tmax", "Tmin")
    
    all_data$Date <- as.Date(substr(all_data$DateTime, 1, 8), "%Y%m%d")
    
    daily <- aggregate(cbind(Tmax, Tmin) ~ Date, data = all_data, FUN = mean)
    
    daily$GDD <- pmax(((daily$Tmax + daily$Tmin)/2) - 32, 0)
    daily$GDD_cum <- cumsum(daily$GDD)
    
    daily
  })
  
  # manual entry
  observeEvent(input$add_row, {
    df <- data()
    
    new_row <- data.frame(
      Date = input$Date,
      AvgPlateMeterReading = input$AvgPlateMeterReading
    )
    
    data(rbind(df, new_row))
  })
  
  # file upload
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
  
  # table
  output$contents <- renderDT({
    req(data())
    
    df <- data()
    df$Date <- as.Date(df$Date, tryFormats = c(
      "%Y-%m-%d",
      "%m/%d/%Y",
      "%m/%d/%y"
    ))
    
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    
    gdd <- gdd_data()
    
    if (!is.null(gdd)) {
      df <- merge(df, gdd[, c("Date", "GDD", "GDD_cum")], by = "Date", all.x = TRUE)
    }
    
    datatable(df, editable = TRUE, selection = "multiple")
  })
  
  # edits
  observeEvent(input$contents_cell_edit, {
    info <- input$contents_cell_edit
    df <- data()
    
    df[info$row, info$col] <- info$value
    data(df)
  })
  
  # plot 1: Date vs Forage
  output$plot1 <- renderPlot({
    req(data())
    
    df <- data()
    df$Date <- as.Date(df$Date, tryFormats = c(
      "%Y-%m-%d",
      "%m/%d/%Y",
      "%m/%d/%y"
      ))
    
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
  
  # plot 2: GDD vs Forage
  output$plot2 <- renderPlot({
    req(data())
    
    df <- data()
    df$Date <- as.Date(df$Date, tryFormats = c(
      "%Y-%m-%d",
      "%m/%d/%Y",
      "%m/%d/%y"
      ))
    
    df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
    
    gdd <- gdd_data()
    req(gdd)
    
    df <- merge(df, gdd[, c("Date", "GDD_cum")], by = "Date", all.x = TRUE)
    
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