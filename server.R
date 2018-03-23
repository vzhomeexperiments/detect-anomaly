# ----------------------------
# Description: Shiny app for Process Anomaly Detection
# Author: Vladimir Zhbanko
# Date: 2018-03-21
# ----------------------------

library(shiny)
library(DT)
library(lubridate)
library(tidyverse)
library(h2o)
source("score_process.R")
# =======================================
# DATA GATHERING AND PREPROCESSING      #
# =======================================  
### Generate dataset for the Dashboard visualization
DF_Data <-  read_rds("DATA_app.rds")

### set dates as 'today'
DF_Data[1, 1] <- as.POSIXct(Sys.time()-100)
DF_Data[2, 1] <- as.POSIXct(Sys.time()-200)
DF_Data[3, 1] <- as.POSIXct(Sys.time()-300)
DF_Data[4, 1] <- as.POSIXct(Sys.time()-400)
DF_Data[5, 1] <- as.POSIXct(Sys.time()-500)
DF_Data[6, 1] <- as.POSIXct(Sys.time()-600)

## extract data without date column
Process_NN <- DF_Data %>%
  select(2:11) %>%
  as.matrix()

# score all data getting the output dataframe with 1 column
Scored_process <- score_process(Process_NN, "www/tmp/normality_model.bin/DeepLearning_ProcessDemo")
 
# add new column to original dataframe
PROCESS_NN <- bind_cols(DF_Data, Scored_process)

shinyServer(function(input, output, session) {
  
 
  ## **-------------- **
  # reset to original values when pressing the button
  observeEvent(input$reset, {
    updateNumericInput(session, inputId = "Inf1", value = 1130)
    updateNumericInput(session, inputId = "Inf2", value = 1436)
    updateNumericInput(session, inputId = "Inf3", value = 1973)
    updateNumericInput(session, inputId = "Inf4", value = 2052)
    updateNumericInput(session, inputId = "Centr1", value = 2825)
    updateNumericInput(session, inputId = "Centr2", value = 3400)
    updateNumericInput(session, inputId = "Out1", value = 1717)
    updateNumericInput(session, inputId = "Out2", value = 2069)
    updateNumericInput(session, inputId = "Out3", value = 69.5)
    updateNumericInput(session, inputId = "Out4", value = 83.1)

  })
  ## **-------------- **
  
  ## **-------------- **
  # score the values for anomaly detection by pressing a button 'Validate'
  ScoreNew <- eventReactive(input$submit, {
    ## score this data getting mse
    DF_MSE <<- DF() %>% select(2:11) %>% score_process("www/tmp/normality_model.bin/DeepLearning_ProcessDemo")
  })
  ## **-------------- **

  ## **-------------- **
  # prepare reactive data object for the plot that will highlight anomaly 
  ScoreJoined <- reactive({
    A <- DF() %>% select(2:11) %>% t() %>% as.vector()
    B <- ScoreNew() %>% t() %>% as.vector()
    ScoreJoined <- data.frame(Param = A, Index = 1:10, Anomaly = B)
  })
  ## **-------------- **
  
  ## **-------------- **
  # Object DF will contain the resulting 'input' to the process (created for DEMO purposes)
  DF <- reactive({
    # building into the dataframe
    DF <- data.frame(DateCheck = as.POSIXct(Sys.time()+100),
                     Inf1 = input$Inf1, Inf2 = input$Inf2, Inf3 = input$Inf3, Inf4 = input$Inf4,
                     Centr1 = input$Centr1, Centr2 = input$Centr2,
                     Out1 = input$Out1, Out2 = input$Out2, Out3 = input$Out3, Out4 = input$Out4,
                     stringsAsFactors = F)
  })
  ## **-------------- **
  
  ## **-------------- **
  # object to keep reactive values on the table responses
  RESP <- eventReactive(input$submit, {
    # joining responses to see a time-series plot
    if(!exists("OUT")){OUT <<- PROCESS_NN} else {
      
      ADD <<- bind_cols(DF(), ScoreNew())
      OUT <<- bind_rows(OUT, ADD)
      }
    RESP <- OUT
  })
  ## **-------------- **

  ## **-------------- **
  # output to download file if needed
  output$downloadTable <- downloadHandler(
    filename = function() { 
      paste("Data-", Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(RESP(), file)
    }
  )
  ## **-------------- **

  ## ****-------------- ****   
  # output the results of the data input
  output$table <- renderTable({ DF()  })
  ## ****-------------- ****   
  
  ## ****-------------- ****   
  # output the results of the anomaly rating
  output$mse <- renderTable({ ScoreNew()  })
  
  ## ****-------------- ****   
  # Visualize responses in the interactive tables, refresh when some buttons are pressed...
  output$responses <- DT::renderDataTable({  RESP()  })     
  ## ****-------------- ****     

  ## *******--Functions for Plots-- *******        
  
  # Function to draw plotA
  PlotA <- function(){
    ScoreJoined() %>% ggplot(aes(x = Index, y = Param, size = 5, col = Anomaly))+geom_point()
  }
  
  # Function to draw plotB
  PlotB <- function(){
     RESP() %>% 
       select(DateCheck, AnomalyRating) %>% 
      ggplot(aes(x = DateCheck, y = AnomalyRating))+geom_point()+
      geom_hline(yintercept = 0.15 , color = "yellow", size = 2)+
      geom_hline(yintercept = 0.5 , color = "red", size = 2)
  }    

  ## *******--Functions for Plots-- *******           
  
  output$PlotA <- renderPlot({  
    ggsave("plotA.png", plot = PlotA(), device = "png")
    PlotA() }) 
  
  output$PlotB <- renderPlot({
    ggsave("plotB.png", plot = PlotB(), device = "png")
    PlotB() 
  })
  

  # ================================= 
  ### download plot:
  output$downloadPlotA <- downloadHandler(
    filename = function(){ "plotA.png"},
    content = function(file){
      file.copy("plotA.png", file, overwrite = TRUE)
    }
  )
  ### download plot:
  output$downloadPlotB <- downloadHandler(
    filename = function(){ "plotB.png"},
    content = function(file){
      file.copy("plotB.png", file, overwrite = TRUE)
    }
  ) 
  

})