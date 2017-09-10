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

# ============================ THIS CODE WILL BE RUN ONCE ============================
# Use this part as "No brainer" start - define elements you will use in your App...
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

# define variable containing a date 30 days ago
daysBack30 <- Sys.Date() - 30

# ============================ THIS CODE BELOW WILL BUILD HTML PAGE ==================

shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  # Page Row 1 == Information Pane ==
                  # Adding Logo [optional]
                  fluidRow(column(2, img(height = 50, width  = 106, src    = "logo.JPG")),
                           # Application title
                           column(9, titlePanel("Change Over Steps Duration Overview"),
                                  "Artificial Intelligence is helping Human to identify Problem")),
                  
                  hr(), # Adding a horizontal line
                  
                  # Page Row 2 == User Inputs ==
                  # Date inputs  
                  fluidRow(column(2, dateInput(inputId = "DateStart", label = "Insert Start Date", value = daysBack30)),
                           column(2, dateInput(inputId = "DateEnd",   label = "Insert End Date")),
                           # Check box input
                           column(1, checkboxInput(inputId = "cboxSE", label = "Add Stat Error?", value = FALSE, width = NULL)),
                           # Another check box
                           column(1, checkboxInput(inputId = "happy", label = "I am Happy", value = TRUE, width = NULL),
                                  checkboxInput(inputId = "inspired", label = "I am Inspired", value = FALSE, width = NULL)),
                           # Help text
                           column(3, helpText("Note: This App was created especially for the Udemy Course",
                                              "Identifying Problems with Artificial Intelligence",
                                              "provided method to detect anomalies using",
                                              "Unsupervised Machine Learning"))),
                  fluidRow(column(8, selectInput(inputId = "selInput",label = "Add Machine Steps to Analysis", choices = stepsChoices, 
                                                 selected = stepsChoices[1], multiple = TRUE, 
                                                 selectize = TRUE, width = '100%', size = NULL),
                                  column(10,sliderInput(inputId = "mySlider", label = "This is my slider input element for the ShinyApp",
                                                        value = 0, min = 0, max = 100, width = '100%', step = 5)),
                                  column(2, actionButton(inputId = "goButton", label = "Go!",icon = icon("car"))))),
                  
                  hr(), # Adding a horizontal line
                  
                  # Page Row 3 == Plot or other Outputs ==
                  fluidRow(column(12,  
                                  # Show a plot 
                                  mainPanel(
                                    tabsetPanel(
                                      #tabPanel("Plot - Smoothed", plotOutput("Plot")),
                                      tabPanel("Plot - Points", plotOutput("Plot1")),
                                      tabPanel("Plot - Box Plot", plotOutput("Plot2")),
                                      tabPanel("Deviation Auto Detection", "Select machine step and choose the dates of interest",
                                               hr(), 
                                               selectInput(inputId = "Step",label = "ChooseStep", choices = stepsChoices, 
                                                           selected = stepsChoices[1], multiple = FALSE, 
                                                           selectize = TRUE, size = NULL), hr(),
                                               plotOutput("Plot3"))
                                    )
                                    
                                  )
                  )),
                  
                  fluidRow(column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")),
                           column(1, img(height = 50, width  = 106, src    = "logo.JPG")))
))

