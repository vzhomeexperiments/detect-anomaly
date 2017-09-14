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
DF_Data <- read_csv("DF_Data.csv")
# data frame containing equipment information
DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeData.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP <- DF_Data %>% 
  # arrange by date 
  arrange(StartDateTime) %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # select only column needed
  select(StartDateTime, Name, EventCode, TimeTotal) %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDateTime, Name, EventCode, TimeTotal, EventText)
# ================================= 

shinyServer(function(input, output, session) {

# =================================  
  # save variables to use in other reactive functions and render functions
  StartDate <- reactive( {as.POSIXct(input$DateStart)} )
  EndDate <- reactive( {as.POSIXct(input$DateEnd)} )
  StatErr <- reactive( {input$cboxSE} )
  # # uncomment for debugging...
  # StartDate <- "2017-04-20 00:10:20"
  # EndDate <- "2017-08-20 00:10:20"
  # StatErr <- FALSE
  
# =================================  

  ## ****** ---------------*******
  # Uploading data using the app
  observeEvent(input$myTable, {
    inFile <- input$myTable
    if (is.null(inFile))
      return()
    #save R object to file for further investigations
    saveRDS(inFile,file = "in.File")
    file.copy(inFile$datapath, file.path("temp_data", paste(Sys.Date(), ".csv")))
  })
  ## ******---------------********
  
  
# =================================
    # save as data frame data used for statistics in other render functions
  DF_SUM <- reactive({
    
    DF_TEMP %>% 
      # filters for categories
      filter(EventText == input$selInput) %>% 
      # group by 
      group_by(Name) %>% 
      # filters X date
      filter(StartDateTime > StartDate(), StartDateTime < EndDate())
  })
  
# =================================  
  # This object is needed exclusively for clustering
  DF_SUM_ALL <- reactive({
    
    # Data manipulation and saving to the DF_Data reactive value
    DF_KM <- DF_TEMP %>% 
      # filters for category
      filter(EventText == input$Step) 
    
    KM <- DF_KM %>% 
      select(Name, TimeTotal) %>%
      mutate(Name = revalue(Name, c("Machine #1" = "1", "Machine #2" = "2", "Machine #3" = "3", "Machine #4" = "4"))) %>% 
      kmeans(centers = 2, nstart = 20)
    
      # saving clustering result to the new data frame
      vector <- as.data.frame.vector(KM$cluster)
      names(vector) <- "Clust"
    
    DF_SUM_ALL <- DF_KM %>% 
      select(StartDateTime, TimeTotal, Name) %>%
      # join clustering result
      bind_cols(vector) %>% 
      mutate(Clust = as.factor(Clust))  
      
  })
  
# =================================  
# OUTPUTS
# =================================  
    
  ### Render function to create a main plot:
  output$Plot <- renderPlot({
    # generate object for the plot using DF_SUM
    DF_SUM() %>% 
      ggplot(aes(x = StartDateTime, y = TimeTotal, col = EventText)) + 
      geom_smooth(alpha = 0.5, se = StatErr()) +
      facet_wrap(~Name) + ylab("Duration of Step, seconds") +
      ggtitle(paste("Overview of Steps ", "from: ",
                                      StartDate(), " to: ", EndDate(), sep = "")) 
  })
  
  # ================================= 
  
  ### Render function to create a main plot:
  output$Plot1 <- renderPlot({
    # generate object for the plot using DF_SUM
    DF_SUM()  %>% 
      ggplot(aes(x = StartDateTime, y = TimeTotal, col = EventText)) + geom_point()+
      geom_smooth(alpha = 0.5, se = StatErr()) +
      facet_wrap(~Name) + 
      ylab("Duration of Step, seconds") +
      ggtitle(paste("Overview of Steps ", "from: ",
                                      StartDate(), " to: ", EndDate(), sep = "")) 
  })
  
  # ================================= 
  
  ### Render function to create another plot:
  output$Plot2 <- renderPlot({
    # generate object for the plot using DF_SUM
    DF_SUM() %>% 
      ggplot(aes(x = StartDateTime, y = TimeTotal, col = EventText)) + geom_boxplot() +
      facet_wrap(Name ~ EventText) +
      ggtitle(paste("Overview of Steps ", "from: ",
                                      StartDate(), " to: ", EndDate(), sep = "")) 
  })
  
  
  # ================================= 
  ### Render function to create another plot:
  output$Plot3 <- renderPlot({
    # generate object for the plot using DF_SUM
    DF_SUM_ALL() %>% 
      filter(StartDateTime > StartDate(), StartDateTime < EndDate()) %>% 
      ggplot(aes(x = StartDateTime, y = TimeTotal, col =Clust)) + geom_point() + facet_wrap(~Name)+
      
      ggtitle(paste("Anomaly Detection of the Step Duration", "from: ",
                    StartDate(), " to: ", EndDate(), ". Different colour indicates potential anomaly", sep = "")) 
  })
  
  # ================================= 
  # visualize the table that user loads
  output$inFilecontents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, all rows will be shown.
    
    req(input$myTable)
    
    df <- read_csv(input$myTable$datapath)
    
      # show maximum 10 lines
      return(head(df, 10))
    
  })
  
  
})