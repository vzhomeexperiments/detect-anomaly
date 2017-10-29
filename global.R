library(tidyverse)
library(magrittr)

# ================================= 
# importing data (code will be run once)

# data frame containing ALL information from multiple sensors
DF_Data <- readRDS("DF_Data_Process.data") #
# data frame containing equipment information
DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP for RECENT data
DF_TEMP <- DF_Data %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDate, Name, AnalogVal, EventText)

# Get only Machine names to vector 'Machines'
machines <- DF_TEMP %>% select(Name) %>% unique() %$% sort(Name)

# Define choices for selectInput function (it is containing steps the user can filter)
stepsChoices <- read_csv("DF_EvCodeDataProject.csv") %$% sort(EventText)
# ================================= 

# we will need to run all events for all machines to made plots available
makePlot <- function(x){
  res <- AnomalyDetectionTs(x, max_anoms=0.02, direction='pos', 
                     xlabel = "Production Dates", ylabel = "Process value, Arbitrary Unit",
                     title = "Anomaly Detection on Time Series data",
                     #only_last = 'day',
                     plot=TRUE)
  return(res$plot)
}

# make for loop for each machine
for(i in 1: length(machines)){
  # filter data belonging to each machine
  DF <- DF_TEMP %>% filter(Name == machines[i])
  
    # run another loop for events
    for(j in 1:length(stepsChoices)){
      df <- DF %>% filter(EventText == stepsChoices[j]) %>% 
        arrange(StartDate) %>% 
        # limit number of observations
        head(50000) %>%
        # select only 2 columns
        select(StartDate, AnalogVal) %>% 
        AnomalyDetectionTs(max_anoms=0.02, direction='pos', 
                           xlabel = "Production Dates", ylabel = "Process value, Arbitrary Unit",
                           title = "Anomaly Detection on Time Series data",
                           #only_last = 'day',
                           plot=TRUE)
      
      AllPlots[i][j] <- df$plot  
    }
  

  
    
}





## Turn this into function
# function to create new features from original feature...
feature_eng_ts <- function(x, funcToApply = c("mean", "min", "max", "sd")){
  #function will return new dataframe with new features
  #x = dataframe with columns to perform feature engineering, it must contain data from one specific category
  #tscolname = column name containing time series data
  #avcolname - column name contining data to perform manipulations
  #funcToApply - vector containing functions to apply data transformations
  # convert to xts object
  DF_M1_xts <- as.xts(x[ ,-1], order.by = as.POSIXct(x$StartDate))
  # aggregate by hour and create new features
  # convert periodicity from seconds to hours and apply some functions to create new features
  for(i in 1: length(funcToApply)){
    # apply functions and merge them  
    res <- period.apply(DF_M1_xts, endpoints(DF_M1_xts, "hours"), funcToApply[i])
    names(res) <- paste("AnalogVal_", funcToApply[i], sep = "")
    # merge new features together
    if(i == 1){
      DF_M1_newfeatures <- res
      } else {
        DF_M1_newfeatures <- merge(DF_M1_newfeatures, res)
        }
  }
  # return to the dataframe
  DF_M1_NF <- coredata(DF_M1_newfeatures) %>% as.data.frame(row.names = F)
  DF_M1_NF$StartDate <- index(DF_M1_newfeatures)
  # removing rows with missing data!!!
  DF_M1_NF <- na.omit(DF_M1_NF)
  # return result
  return(DF_M1_NF)
}

# function that apply function feature_eng_ts to each machine and return common dataframe
feature_eng_machines <- function(x, Machines){
  # apply transformation and feature engineering for each machine
  # x - dataframe containing only one selected event for each machine
  # Machines - vector containing machine names
      
  for(i in 1:length(Machines)){
    
    DF_A <- x %>% 
      filter(Name == Machines[i]) %>% 
      select(StartDate, AnalogVal)
    DF_B <- x %>% 
      filter(Name == Machines[i]) %>% 
      select(EventText, Name) %>% unique()
    DF_F <- feature_eng_ts(DF_A)
    DF_F$EventText <- DF_B[1,1]
    DF_F$Name <- DF_B[1,2]
    
    if(i == 1){
      DF_SUM <- DF_F
    } else {
      DF_SUM <- DF_SUM %>% bind_rows(DF_F)
    }  
    
  }
    return(DF_SUM)
}