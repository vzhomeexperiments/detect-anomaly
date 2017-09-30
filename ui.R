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

# Define steps choices for selectInput function (it is containing steps the user can filter)
stepsChoices <- c("Tubing Process, phase angle",
                  "Tubing Process, resistance Ohm",
                  "Tubing Process, power (Setting)",
                  "Edging Process, phase angle",
                  "Edging Process, resistance Ohm",
                  "Edging Process, power (Setting)",
                  "Cutting Process, phase angle",
                  "Cutting Process, resistance Ohm",
                  "Cutting Process, power (Setting)",
                  "Cutting Process, pressure")

# 
dashboardPage(
  dashboardHeader(title = "Preparation Steps Duration Overview"),
  dashboardSidebar(
    # Elements on the Sidebar of the App
    img(height = 100, width  = 230, src    = "logo.png"), 
    dateInput(inputId = "DateStart", label = "Insert Start Date", value = "2017-01-01"),
    dateInput(inputId = "DateEnd",   label = "Insert End Date"),
    helpText("Note: Set Desired dates of interest",
                                            "and select plots below to visualize",
                                            "specific step of interest."),
    selectInput(inputId = "selInput",label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                selected = stepsChoices[1], multiple = TRUE, selectize = TRUE, width = '100%', size = NULL),
    checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL),
    checkboxInput(inputId = "points", label = "Add Points?"),
    div(style="display:inline-block;width:65%;text-align: right;",downloadButton(outputId = "downloadPlot",label = "Download Plot"))
    
  ),
  dashboardBody(
    
    mainPanel(
      
      # Elements of the Dashboard: header and tabset panel
      headerPanel("Visualization of steps duration"),
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
                                   plotOutput(outputId = "Plot3"))
      )  
    )
  )
)
