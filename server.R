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
library(h2o)
library(RColorBrewer)

# ================================= 
# importing data (code will be run once)

# data frame containing information from multiple sensors
DF_Data <- readRDS("DF_Data_Process_Recent.data") #
# data frame containing equipment information
DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP <- DF_Data %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDate, Name, AnalogVal, EventText)
# ================================= 

# ================================= 
# scoring using Deep Learning Model - perform scoring before app loading!
Machines <- c("Machine #1", "Machine #2", "Machine #3", "Machine #4")

# =========== DEEP LEARNING ========== #
h2o.init()
# Datasets generated for Anomaly detection with Deep Learning Algorithm
# "Tubing Process, resistance Ohm"
DF_TEMP_TPR <- DF_TEMP %>% 
  filter(EventText == "Tubing Process, resistance Ohm") %>% 
  anomalyscore_machines(Machines = Machines,
                        path_to_model = "www/tmp/normality_model.bin/DeepLearning_model_R_1510597411656_1",
                        n_cols = 150)

# "Tubing Process, Phase"
DF_TEMP_TPA <- DF_TEMP %>%
  filter(EventText == "Tubing Process, phase angle") %>%
  anomalyscore_machines(Machines = Machines,
                        path_to_model = "www/tmp/normality_model.bin/DeepLearning_model_R_1510950315727_1",
                        n_cols = 150)


h2o.shutdown(prompt = F)
# bind results
DF_TEMP_MSE <- bind_rows(DF_TEMP_TPR, DF_TEMP_TPA)

# =========== DEEP LEARNING ========== #


# ================================= 


shinyServer(function(input, output, session) {

# =================================  
  # save variables to use in other reactive functions and render functions
  StartDate <- reactive( {as.POSIXct(input$DateStart)} )
  EndDate <- reactive( {as.POSIXct(input$DateEnd)} )
  StatErr <- reactive( {input$cboxSE} )
  Classes <- reactive({input$numClasses})
  MachInp <- reactive({input$machInp})
  # # uncomment for debugging...
  # StartDate <- "2017-04-20 00:10:20"
  # EndDate <- "2017-08-20 00:10:20"
  # StatErr <- FALSE
  
# =================================  

  
# =================================
    # save as data frame data used for statistics in other render functions
  DF_SUM <- reactive({
    
    DF <- DF_TEMP %>% 
      # filters for categories
      filter(EventText == input$selInput) 
    # make vector of machines
    Machines <- DF %>% select(Name) %>% unique() %$% Name
    
    DF_SUM <- feature_eng_machines(DF, Machines)
  })
  
# =================================  
  # Prepare data for clustering and do clustering with if/else statement...
  DF_SUM_ALL <- reactive({
    
    # Data manipulation and saving to the DF_Data reactive value
    DF_KM <- DF_TEMP %>% #filter(EventText == "Cutting Process, phase angle") 
      # filters for category
      filter(EventText == input$Step) 
    
    # make vector of machines
    Machines <- DF_KM %>% select(Name) %>% unique() %$% Name
    
    # perform feature engineering, note this dataframe will contain 'AnalogVal_mean' and 'AnalogVal_sd'
    # 'AnalogVal_mean' will be used for visualization and 'AnalogVal_sd' for clustering
    DF_FE <- feature_eng_machines(DF_KM, Machines)
    
    # make intermediate dataframe
    KM1 <- DF_FE %>%
      # performed clustering on two new features (example, it does not make sense here)
      select(Name, AnalogVal_mean, AnalogVal_sd) %>%
      mutate(Name = revalue(Name, c("Machine #1" = "1", "Machine #2" = "2", "Machine #3" = "3", "Machine #4" = "4"))) %>%
      mutate(Name = as.numeric(Name)) 
    
    # perform different modelling depending on the user choice...
    if(!input$scaled) {
      KM <- KM1 %>% 
        kmeans(centers = Classes(), nstart = 20)
    } else {
      KM <- KM1 %>% 
        scale() %>%
        as.data.frame() %>%
        kmeans(centers = Classes(), nstart = 20)
    }
    
    # saving clustering result to the new data frame
    vector <- as.data.frame.vector(KM$cluster)
    names(vector) <- "Clust"
    
    DF_SUM_ALL <- DF_FE %>% 
      select(StartDate, AnalogVal_mean, Name) %>%
      # join clustering result
      bind_cols(vector) %>% 
      mutate(Clust = as.factor(Clust))  
    
    
  })
  
  # =================================  
  # Filter data with anomaly detection
  DF_SUM_NN <- reactive({
    
    # filter data by date
    DF_TEMP_MSE %>% 
      filter(Name %in% MachInp()) %>% 
      filter(StartDate > StartDate()) %>% 
      filter(StartDate < EndDate())
    
  })
  
    
# =================================  
# FUNCTIONS
# =================================  
  # Function to draw main plot, using condition of input$point to add points to the graph
  mainPlot <- function(){
    if(input$points){
      DF_SUM() %>% 
        ggplot(aes(x = StartDate, y = AnalogVal_mean, col = EventText)) + 
        geom_smooth(se = StatErr()) +
        geom_point(alpha = 0.4) +
        facet_wrap(~Name) + ylab("Process parameter, Arbitrary unit") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
      
    } else {
      DF_SUM() %>% 
        ggplot(aes(x = StartDate, y = AnalogVal_mean, col = as.factor(EventText))) + 
        geom_smooth(alpha = 0.5, se = StatErr()) +
        facet_wrap(~Name) + ylab("Process parameter, Arbitrary unit") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
      
    }
    
  }
  
  # Function to draw Box plot
  boxPlot <- function(){
    DF_SUM() %>% 
      ggplot(aes(x = StartDate, y = AnalogVal_mean, col = EventText)) + geom_boxplot() +
      facet_grid(~Name) + 
      ylab("Process parameter, Arbitrary unit") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Box Plot from all data. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Box plots can help to indicate average values and outliers") 
    
  }
  # Function to draw deviation plot
  deviationPlot <- function(){
    
    DF_SUM_ALL() %>% 
      filter(StartDate > StartDate(), StartDate < EndDate()) %>% 
      ggplot(aes(x = StartDate, y = AnalogVal_mean, col = Clust)) + geom_point() + facet_wrap(~Name)+
      ylab("Process parameter, Arbitrary unit") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Anomaly Detection of the Arbitrary Parameter. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Different colors may highlight potential anomaly") 

  }
  
  # Function to draw Anomaly plot neural network
  nnPlot <- function(){
    # define color palette
    mypalette <- c("#91cf60", "#fc8d59", "#fc8d60") 
    
    # create simple chart with color layer
    DF_SUM_NN() %>% 
      filter(EventText == input$selInput) %>% 
      ggplot(aes(x = StartDate, y = AnalogVal, colour = AnomalyRating)) + 
      geom_line() + facet_wrap(~Name) +
      scale_colour_gradientn(colours=mypalette)
    
  }
# =================================  
# OUTPUTS
# =================================  
    
  ### Render function to create a main plot:
  output$Plot <- renderPlot({ 
    ggsave("plot.png", plot = mainPlot(), device = "png")
    mainPlot() },  height = "auto", width = 650)

  # ================================= 
  
  ### Render function to create Box Plot:
  output$Plot2 <- renderPlot({ 
    ggsave("plot.png", plot = boxPlot(), device = "png")
    boxPlot() }, height = "auto", width = 650)
  
  
  # ================================= 
  ### Render function to create plot Anomaly:
  output$Plot3 <- renderPlot({ 
    ggsave("plot.png", plot = deviationPlot(), device = "png")
    deviationPlot() }, height = "auto", width = 650)
  
  # ================================= 
  ### Render function to create plot Anomaly:
  output$Plot4 <- renderPlot({ 
    ggsave("plot.png", plot = nnPlot(), device = "png")
    nnPlot() }, height = "auto", width = 650)
  
  # ================================= 
  ### download plot:
  output$downloadPlot <- downloadHandler(
      filename = function(){ "plot.png"},
      content = function(file){
      file.copy("plot.png", file, overwrite = TRUE)
      }
    )
  
})