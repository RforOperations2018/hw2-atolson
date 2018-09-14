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
ui <- navbarPage("Pittsburgh Employment Statistics", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Neighborhood select
                              selectInput("HoodSelect",
                                          "Neighborhood:",
                                          choices = sort(employ$Neighborhood),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("North Oakland", "Squirrel Hill North", "Squirrel Hill South")),
                              # Population Selection
                              sliderInput("popSelect",
                                          "Population:",
                                          min = min(employ$Population.2010, na.rm = T),
                                          max = max(employ$Population.2010, na.rm = T),
                                          value = c(min(employ$Population.2010, na.rm = T), max(employ$Population.2010, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Employment Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")