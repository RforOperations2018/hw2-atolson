# Andrew Olson
# Assignment 2

# Loading libraries used
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)

#reading in the data
employ <- read.csv(file="employment.csv", header = TRUE, sep = ",")

pdf(NULL)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Pittsburgh Employment Statistics"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Neighborhood select
      selectInput("HoodSelect",
                  "Neighborhood:",
                  choices = sort(employ$Neighborhood),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("North Oakland", "Squirrel Hill North", "Squirrel Hill South")),
      
      #Population Selection
      sliderInput("popSelect",
                  "Population:",
                  min = min(employ$Population.2010, na.rm = T),
                  max = max(employ$Population.2010, na.rm = T),
                  value = c(min(employ$Population.2010, na.rm = T), max(employ$Population.2010, na.rm = T)),
                  step = 1),
      
      #Reset Filters button
      actionButton("reset", "Reset Filters", icon = icon("refresh")),
      
      #Download data button
      downloadButton("downloadData","Download Employment Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot1", 
                 plotlyOutput("plot1")
        ),
        tabPanel("Plot2", 
                 plotlyOutput("plot2")
        ),
        tabPanel("Plot3", 
                 plotlyOutput("plot3")
        ),
        tabPanel("Table",
                 DT::dataTableOutput("table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    dat <- subset(employ, Neighborhood %in% input$HoodSelect)
    ggplot(data = dat, aes(x = Neighborhood, y = as.numeric(Total_Adult_Residents_Employed.2010), fill = Neighborhood)) + geom_bar(stat = "identity")
  })
  output$table <- DT::renderDataTable({
    subset(employ, Neighborhood %in% input$HoodSelect, select = c(Employment, Population.2010, Sector, Total_Adult_Residents_Employed.2010))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")