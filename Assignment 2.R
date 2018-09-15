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
employ.load <- read.csv(file="employment.csv", header = TRUE, sep = ",")

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
                  choices = sort(employ.load$Neighborhood),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("North Oakland", "Squirrel Hill North", "Squirrel Hill South")),
      
      #Population Selection
      sliderInput("popSelect",
                  "Population:",
                  min = min(employ.load$Population.2010, na.rm = T),
                  max = max(employ.load$Population.2010, na.rm = T),
                  value = c(min(employ.load$Population.2010, na.rm = T), max(employ.load$Population.2010, na.rm = T)),
                  step = 1),
      
      #Reset Filters button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", 
                 plotlyOutput("plot1")
        ),
        tabPanel("Bar Chart", 
                 plotlyOutput("plot2")
        ),
        tabPanel("Boxplot", 
                 plotlyOutput("plot3")
        ),
        tabPanel("Table",
                 DT::dataTableOutput("table"),
                 
                 #Download data button
                 downloadButton("downloadData","Download Employment Data")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Employment data
  emInput <- reactive({
    employ <- employ.load %>%
      # Population Filter
      filter(Population.2010 >= input$popSelect[1] & Population.2010 <= input$popSelect[2])
    # Neighborhood Filter
    if (length(input$HoodSelect) > 0 ) {
      employ <- subset(employ, Neighborhood %in% input$HoodSelect)
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
        labs(x = "Adults Employed", y = "Population", title = "Employed Adults by Population") +
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
        labs(x = "Neighborhood", y = "% Employed", title = "% Employed by Neighborhood") +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$plot3 <- renderPlotly({
    dat <- emInput()
    ggplotly(
      ggplot(data = dat, aes(x = Sector, y = round(Total_Adult_Residents_Employed.2010/Total_Jobs_Located_in_Neighborhood.2000,2))) + 
        geom_boxplot()+
        labs(x = "Neighborhood", y = "% Residents Employed", title = "% Jobs Taken by Residents in Neighborhood") +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    employ <- emInput()
    subset(employ, select = c(Neighborhood, Sector, Population.2010, Total_Adult_Residents_Employed.2010, Total_Jobs_Located_in_Neighborhood.2000))
  })
  
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("pit-employment-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(emInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "HoodSelect", selected = c("North Oakland", "Squirrel Hill North", "Squirrel Hill South"))
    updateSliderInput(session, "popSelect", value = c(min(employ.load$Population.2010, na.rm = T), max(employ.load$Population.2010, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")