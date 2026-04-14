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
      plotOutput("plot1")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  data <- reactiveVal(data.frame(
    col1 = character(),
    col2 = numeric()
  ))
  
  #manual data entry
  observeEvent(input$add_row, {
    req(data())
    
    df <- data()
    
    new_row <- data.frame(
      Date = input$Date,
      AvgPlateMeterReading = input$AvgPlateMeterReading
    )
    
    data(rbind(df, new_row))
  })
  
  #fileupload
  observeEvent(input$file1, {
    req(input$file1)
    
    files <- input$file1
    
    df_list <- lapply(files$datapath, function(path) {
      read.csv(path,
               header = input$header,
               sep = input$sep,
               quote = input$quote)
    })
    
    df <- do.call(rbind, df_list)
    data(df)
  })
  
  output$contents <- renderDT({
    req(data())
    
    df <- data()
    
    # compute forage mass
    df$ForageMass_kg_ha <- ((df$AvgPlateMeterReading * 140) + 500)
    
    datatable(df, editable = TRUE)
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
  
  # compute forage mass (same logic as table)
  df$ForageMass_kg_ha <- (df$AvgPlateMeterReading * 140) + 500
  
  # make sure Date is in proper format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  
  # order by date (VERY important for line plots)
  df <- df[order(df$Date), ]
  
  plot(df$Date,
       df$ForageMass_kg_ha,
       type = "l",                 # line graph
       col = "darkgreen",
       lwd = 2,
       xlab = "Date",
       ylab = "Forage Mass (kg/ha)",
       main = "Forage Mass Over Time")
})
}

# Create Shiny app ----
shinyApp(ui, server)