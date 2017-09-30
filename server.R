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

# ================================= 
# importing data (code will be run once)

# data frame containing information from multiple sensors
DF_Data <- readRDS("DF_Data_Seals_Recent.data")
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
    
    DF_TEMP %>% 
      # filters for categories
      filter(EventText == input$selInput) %>% 
      # group by
      group_by(Name) %>%
      # filters X date
      filter(StartDate > StartDate(), StartDate < EndDate())
  })
  
# =================================  
  # Prepare data for clustering and do clustering with if/else statement...
  DF_SUM_ALL <- reactive({
    
    # Data manipulation and saving to the DF_Data reactive value
    DF_KM <- DF_TEMP %>% #filter(EventText == "Strip applicator, impedance") 
      # filters for category
      filter(EventText == input$Step) 
    
    # make intermediate dataframe
    KM1 <- DF_KM %>%
      select(Name, AnalogVal) %>%
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
    
    DF_SUM_ALL <- DF_KM %>% 
      select(StartDate, AnalogVal, Name) %>%
      # join clustering result
      bind_cols(vector) %>% 
      mutate(Clust = as.factor(Clust))  
    
    
  })
  
  
# =================================  
# FUNCTIONS
# =================================  
  # Function to draw main plot, using condition of input$point to add points to the graph
  mainPlot <- function(){
    if(input$points){
      DF_SUM() %>% 
        ggplot(aes(x = StartDate, y = AnalogVal, col = EventText)) + 
        geom_smooth(se = StatErr()) +
        geom_point(alpha = 0.4) +
        facet_wrap(~Name) + ylab("Duration of Step, seconds") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
      
    } else {
      DF_SUM() %>% 
        ggplot(aes(x = StartDate, y = AnalogVal, col = as.factor(EventText))) + 
        geom_smooth(alpha = 0.5, se = StatErr()) +
        facet_wrap(~Name) + ylab("Duration of Step, seconds") +
        ggtitle(paste("Overview of Steps ", "from: ",
                      StartDate(), " to: ", EndDate(), sep = "")) 
      
    }
    
  }
  
  # Function to draw Box plot
  boxPlot <- function(){
    DF_SUM() %>% 
      ggplot(aes(x = StartDate, y = AnalogVal, col = EventText)) + geom_boxplot() +
      facet_grid(~Name) + 
      ylab("Duration of Step, seconds") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Box Plot from all data. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Box plots can help to indicate average values and outliers") 
    
  }
  # Function to draw deviation plot
  deviationPlot <- function(){
    
    DF_SUM_ALL() %>% 
      filter(StartDate > StartDate(), StartDate < EndDate()) %>% 
      ggplot(aes(x = StartDate, y = AnalogVal, col = Clust)) + geom_point() + facet_wrap(~Name)+
      ylab("Duration of Step, seconds") +
      theme(legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label = paste("Anomaly Detection of the Step Duration. From: ", StartDate(), " To: ", EndDate(), sep = ""), 
              subtitle = "Different colors may highlight potential anomaly") 

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
  ### download plot:
  output$downloadPlot <- downloadHandler(
      filename = function(){ "plot.png"},
      content = function(file){
      file.copy("plot.png", file, overwrite = TRUE)
      }
    )
  
})