# ----------------------------
# Description: Shiny app for Udemy course "Identify Problem with Artificial Intelligence"
# Author: Vladimir Zhbanko
# Date: 2017-08-27
# Version: 00.01
# Date: 
# Version: 
# Changed: 
# ----------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(magrittr)
library(tidyverse)

# Define choices for selectInput function (it is containing steps the user can filter)
stepsChoices <- read_csv("DF_EvCodeDataProject.csv") %$% sort(EventText)
Machines <- c("Machine #1", "Machine #2", "Machine #3", "Machine #4")

# Shiny User Interface 
dashboardPage(
  dashboardHeader(title = "Industrial process overview"),
  dashboardSidebar(
    # Elements on the Sidebar of the App
    img(height = 100, width  = 230, src    = "logo.png"), 
    dateInput(inputId = "DateStart", label = "Insert Start Date", value = "2017-01-01"),
    dateInput(inputId = "DateEnd",   label = "Insert End Date"),
    helpText("Note: Set Desired dates of interest",
                                            "and select plots below to visualize",
                                            "specific step of interest."),
    selectInput(inputId = "selInput",label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                selected = stepsChoices[1], multiple = FALSE, selectize = TRUE, width = '100%', size = NULL),
    checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL),
    checkboxInput(inputId = "points", label = "Add Points?", value = TRUE),
    div(style="display:inline-block;width:65%;text-align: right;",downloadButton(outputId = "downloadPlot",label = "Download Plot"))
    
  ),
  dashboardBody(
    
    mainPanel(
      
      # Elements of the Dashboard: header and tabset panel
      headerPanel("Visualization of Process parameters"),
      tabsetPanel(
        # Default chart visualizing the overall performance of the systems
        tabPanel("Plot - Overview", plotOutput(outputId = "Plot")),
        # Box plot helping to perform comparison
        tabPanel("Plot - Box Plot", plotOutput(outputId = "Plot2")),
        tabPanel("Plot - Anomaly", column(4, selectInput(inputId = "Step",label = "ChooseStep", choices = stepsChoices, 
                                                      selected = stepsChoices[1], multiple = FALSE, selectize = TRUE, size = NULL)),
                                   column(4, numericInput(inputId = "numClasses", label = "Select Number of Classes", 
                                                          value = 2, min = 1, max = 4, step = 1)),
                                   column(4, checkboxInput(inputId = "scaled", label = "Scale Data?", value = FALSE)), hr(),
                                   plotOutput(outputId = "Plot3")),
        tabPanel("Plot - Anomaly NN", checkboxGroupInput(inputId = "machInp", label = "Select Machines", choices = Machines,
                                                         selected = Machines, inline = TRUE, width = NULL, 
                                                         choiceNames = NULL, choiceValues = NULL),
                 plotOutput(outputId = "Plot4"))
      )  
    )
  )
)
