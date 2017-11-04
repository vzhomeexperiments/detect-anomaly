# ----------------------------
# Description: Shiny app for Udemy course "Identify Problem with Artificial Intelligence"
# Author: Vladimir Zhbanko
# Date: 2017-08-27
# Date: 
# Version: 
# Changed: 
# ----------------------------
library(shiny)
library(tidyverse)
library(scales)
library(plyr)
library(magrittr)
library(AnomalyDetection)



shinyServer(function(input, output, session) {

# =================================  
  # save variables to use in other reactive functions and render functions
  StartDate <- reactive( {as.POSIXct(input$DateStart)} )
  EndDate <- reactive( {as.POSIXct(input$DateEnd)} )
  StatErr <- reactive( {input$cboxSE} )
  Classes <- reactive({input$numClasses})
  # # uncomment for debugging...
  # StartDate <- "2017-04-20 00:10:20"
  # EndDate <- "2017-08-20 00:10:20"
  # StatErr <- FALSE
  
# =================================  

# =================================
    # save as data frame data used for statistics in other render functions
  DF_SUM <- reactive({
    
    DF_SUM <- DF_TEMP %>% 
      # filters for categories
      filter(EventText == input$selInput) %>% 
      # filter for machines
      filter(Name == input$machineInput) %>% 
      # arrange by date
      arrange(StartDate) %>% 
      # limit number of observations
      head(40000) %>%
      # select only 2 columns
      select(StartDate, AnalogVal) %>% 
      # run statistical function to detect anomaly
      AnomalyDetectionTs(max_anoms=0.02, direction='pos', 
                         xlabel = "Production Dates", ylabel = "Process value, Arbitrary Unit",
                         title = "Anomaly Detection on Time Series data",
                         #only_last = 'day',
                         plot=TRUE)
    
  })
  
# =================================  
  # Prepare data for clustering and do clustering with if/else statement...
  DF_SUM_ALL <- reactive({
    
    
    
  })
  
  
# =================================  
# FUNCTIONS
# =================================  

  # # Function to draw deviation plot
  # deviationPlot <- function(){
  #   
  #   
  #   
  # }
# =================================  
# OUTPUTS
# =================================  
    
  # ================================= 
  ### Render function to create plot Anomaly:
  output$Plot3 <- renderPlot({ 
    DF_SUM()$plot
    #ggsave("plot.png", plot = deviationPlot(), device = "png")
    #deviationPlot() 
    }, height = "auto", width = 650)
  
  # ================================= 
  ### download plot:
  # output$downloadPlot <- downloadHandler(
  #     filename = function(){ "plot.png"},
  #     content = function(file){
  #     file.copy("plot.png", file, overwrite = TRUE)
  #     }
  #   )
  
})