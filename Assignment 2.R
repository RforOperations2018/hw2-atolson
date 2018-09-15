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
  # Filtered Employment data
  emInput <- reactive({
    employFilter <- employ %>%
      # Population Filter
      filter(Population.2010 >= input$popSelect[1] & Population.2010 <= input$popSelect[2])
    # Neighborhood Filter
    if (length(input$HoodSelect) > 0 ) {
      employFilter <- subset(employFilter, Neighborhood %in% input$HoodSelect)
    }
    
    return(employ)
  })
  output$plot1 <- renderPlotly({
    dat <- emInput()
    ggplotly(
      ggplot(data = dat, aes(x = Total_Adult_Residents_Employed.2010, y = Population.2010, color = Sector, text = paste0("<b>", Neighborhood, ":</b> ",
                                                                                  "<br>Sector: ", Sector,
                                                                                  "<br>Population: ", Population.2010,
                                                                                  "<br>Employed Adults: ", Total_Adult_Residents_Employed.2010))) + 
        geom_point()+
        xlab("Adults Employed") +
        ylab("Population") +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$plot2 <- renderPlotly({
    dat <- emInput()
    ggplotly(
      ggplot(data = dat, aes(x = Neighborhood, y = round(Total_Adult_Residents_Employed.2010/Population.2010,2), fill = Neighborhood, text = paste0("<b>", Neighborhood, ":</b> ",
                                                                                                                         "<br>Sector: ", Sector,
                                                                                                                         "<br>Population: ", Population.2010,
                                                                                                                         "<br>Employed Adults: ", Total_Adult_Residents_Employed.2010))) + 
        geom_bar(stat = "identity")+
        xlab("Neighborhood") +
        ylab("% Employed") +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$plot3 <- renderPlotly({
    dat <- emInput()
    ggplotly(
      ggplot(data = dat, aes(x = Sector, y = round(Total_Adult_Residents_Employed.2010/Population.2010,2))) + 
        geom_boxplot()+
        xlab("Sector") +
        ylab("% Employed") +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    employFilter <- emInput()
    subset(employFilter, select = c(Neighborhood, Sector, Population.2010, Total_Adult_Residents_Employed.2010))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")