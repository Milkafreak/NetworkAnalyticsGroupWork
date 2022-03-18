library(shiny)
library(data.table)
library(rsconnect)
library(dplyr)

setwd("C:\\Users\\Edwin\\OneDrive - NovaSBE\\Documentos\\1. Uni\\Master\\2. Semester\\Advanced Programming\\Group Project\\NetworkAnalyticsGroupWork")
dt.trump <- fread("TrumpWorld-Data.csv")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# See above for the definitions of ui and server
ui <- ...

server <- ...

shinyApp(ui = ui, server = server)
