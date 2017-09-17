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

shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Page Row 1 == Information Pane ==
  # Adding Logo [optional]
  fluidRow(column(2, img(height = 50, width  = 106, src    = "logo.JPG")),
           column(9, 
                  # Application title
                  titlePanel("Preparation Steps Duration Overview"))),
  
  # Adding a horizontal line
  hr(),
  
  # Page Row 2 == User Inputs ==
  
  fluidRow(column(2, dateInput(inputId = "DateStart", label = "Insert Start Date", value = "2017-01-01")),
           column(2, dateInput(inputId = "DateEnd",   label = "Insert End Date")),
           column(1, checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL)),
           column(3, helpText("Note: while the data view will show only",
                              "the specified number of observations, the",
                              "summary will be based on the full dataset."))),
  fluidRow(column(8, selectInput(inputId = "selInput",label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                                 selected = stepsChoices[1], multiple = TRUE, 
                                 selectize = TRUE, width = '100%', size = NULL))),
  
  # Adding a horizontal line
  hr(),
  
  # Page Row 3 == Plot or other Outputs ==
  fluidRow(column(12,  
                  # Show a plot 
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot - Smoothed", plotOutput(outputId = "Plot")),
                      tabPanel("Plot - Points", plotOutput(outputId = "Plot1")),
                      tabPanel("Plot - Box Plot", plotOutput(outputId = "Plot2")),
                      tabPanel("Deviation Auto Detection", "Select machine step and choose the dates of interest",
                               hr(), 
                               selectInput(inputId = "Step",label = "ChooseStep", choices = stepsChoices, 
                                           selected = stepsChoices[1], multiple = FALSE, 
                                           selectize = TRUE, size = NULL), hr(),
                               plotOutput(outputId = "Plot3"))
                    )

                  )
  ))
))

