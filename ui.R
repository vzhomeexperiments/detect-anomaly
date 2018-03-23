# ----------------------------
# Description: Shiny app for Process Anomaly Detection
# Author: Vladimir Zhbanko
# Date: 2018-03-21
# ----------------------------
# 
library(tidyverse)
library(shiny)
library(shinyTime)
library(DT)
library(magrittr)


shinyUI(fluidPage(theme = "bootstrap.css",


                  # Adding Logo
                  fluidRow(column(1, img(height = 100, width  = 158, src    = "logo.png")),
                           # Application title
                           column(11, titlePanel("Demo: Anomaly Detection System in Industrial Process"))),
                  hr(),

                 #### ***************************************************************
                 ####        USER INPUT
                 #### ***************************************************************
                  # User General Inputs about property ==
                  
                  fluidRow(column(8,  tableOutput("table")),
                           column(1, actionButton("submit", "Validate", icon = icon("sign-in"))),
                           column(1, actionButton("reset", "Reset", icon = icon("refresh")))),
                  
                  
                  # a horizontal rule on the page
                  tags$hr(),
                  
                  # Page Row 3 == User's specific Inputs about property  ==
                  
                  # showing user input windows
                  fluidRow(column(1, sliderInput(inputId = "Inf1", value = 1130,   label = "Inf1", min = 0, max = 4500, step = 1)),
                           column(1, sliderInput(inputId = "Inf2", value = 1436, label = "Inf2", min = 0, max = 4500, step = 1)), 
                           column(1, sliderInput(inputId = "Inf3", value = 1973,   label = "Inf3", min = 0, max = 4500, step = 1)),
                           column(1, sliderInput(inputId = "Inf4", value = 2052,   label = "Inf4", min = 0, max = 4500, step = 1)),
                           column(1, sliderInput(inputId = "Centr1", value = 2825,   label = "Centr1",  min = 0, max = 4500, step = 1)),  
                           column(1, sliderInput(inputId = "Centr2", value = 3400, label = "Centr2", min = 0, max = 4500, step = 1)),
                           column(1, sliderInput(inputId = "Out1", value = 1717, label = "Out1", min = 0, max = 4500, step = 1)), 
                           column(1, sliderInput(inputId = "Out2", value = 2069,   label = "Out2", min = 0, max = 4500, step = 1)),
                           column(1, sliderInput(inputId = "Out3", value = 69.5,   label = "Out3", min = 50, max = 100, step = 0.1)),
                           column(1, sliderInput(inputId = "Out4", value = 83.1,   label = "Out4",  min = 50, max = 100, step = 0.1))),
                                              
                  #### ***************************************************************
                  ####        DATA OUTPUT
                  #### ***************************************************************
                  # arrange content using panels 
                  tabsetPanel(
                              tabPanel("Data Graph", 
                                       #### ***************************************************************
                                       ####        DASHBOARD
                                       #### ***************************************************************
                                       navlistPanel(widths = c(2, 8),
                                                    tabPanel("1. Parameters Overview", 
                                                             wellPanel(
                                                               "press button 'Validate' above to start Anomaly Detection",
                                                               # Adding Plots
                                                               plotOutput("PlotA"), # Plot showing Selected measures
                                                               downloadButton(outputId = "downloadPlotA",label = "Download Plot A")
                                                             )
                                                    ),
                                                    tabPanel("2. Process Time-line", 
                                                             wellPanel(
                                                               "press button 'Validate' above to start Anomaly Detection",
                                                               # Adding Plots
                                                               tableOutput("mse"),
                                                               plotOutput("PlotB"), # Plot showing Anomaly over time
                                                               downloadButton(outputId = "downloadPlotB",label = "Download Plot B")
                                                             )
                                                    ),
                                                    tabPanel("3. Info", 
                                                             wellPanel(
                                                               # text info
                                                               img(height = 500, width  = 700, src    = "info.png")
                                                             )          ),
                                                    tabPanel("4. Contact", 
                                                             wellPanel(
                                                               # Adding Product Logo
                                                               titlePanel("Created by:"),
                                                               "(C) 2018 Vladimir Zhbanko",
                                                               "https://www.udemy.com/identify-problems-with-ai-case-study/?couponCode=IDENTIFY-PROBLEM-10"
                                       )
                                                   )
                                    )),
                              tabPanel("Data Table",
                                                 # Page Row 3 == Last results view as interactive data table ==
                                                 fluidRow(column(12,  DT::dataTableOutput("responses", width = 400)),
                                                          # add horizontal line
                                                          tags$hr(),
                                                          
                                                          # action button aim: when user click save information to csv file        
                                                          fluidRow(column(1, downloadButton('downloadTable', 'Download')))
                                                 )         
                                      )
                                    


)))