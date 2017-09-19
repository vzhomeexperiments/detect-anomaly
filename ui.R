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
stepsChoices <- c("Step 1 SubStep 1",
                  "Step 1 SubStep 2",
                  "Step 1 SubStep 3",
                  "Step 1 SubStep 4",
                  "Step 2 SubStep 1",
                  "Step 2 SubStep 2",
                  "Step 2 SubStep 3",
                  "Step 2 SubStep 4",
                  "Step 2 SubStep 5",
                  "Step 2 SubStep 6",
                  "Step 2 SubStep 7")

# 
dashboardPage(
  dashboardHeader(title = "Preparation Steps Duration Overview"),
  dashboardSidebar(
    # Elements on the Sidebar of the App
    img(height = 75, width  = 136, src    = "logo.png"), 
    dateInput(inputId = "DateStart", label = "Insert Start Date", value = "2017-01-01"),
    dateInput(inputId = "DateEnd",   label = "Insert End Date", value = "2017-09-01"),
    helpText("Note: Set Desired dates of interest",
                                            "and select plots below to visualize",
                                            "specific step of interest."),
    selectInput(inputId = "selInput",label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                selected = stepsChoices[1], multiple = TRUE, selectize = TRUE, width = '100%', size = NULL),
    checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL),
    checkboxInput(inputId = "points", label = "Add Points?")
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
        tabPanel("Plot - Anomaly", selectInput(inputId = "Step",label = "ChooseStep", choices = stepsChoices, 
                                               selected = stepsChoices[1], multiple = FALSE, 
                                               selectize = TRUE, size = NULL), hr(),
                                               plotOutput(outputId = "Plot3"))
      )  
    )
  )
)
