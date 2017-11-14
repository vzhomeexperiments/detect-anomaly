library(xts)
library(tidyverse)
library(h2o)


# function that perform scoring using Deep Learning model to each machine and return common dataframe with MSE score
anomalyscore_machines <- function(x, Machines, path_to_model, eventName, n_cols = 150){
  # apply Neural Network Model for each machine
  # x - dataframe containing DF_TEMP data structure
  # path to model - location of the Model stored as a .bin object
  # Machines - vector containing machine names
  
  for(i in 1:length(Machines)){
    # extract one machine, all events
    DF_A <- x %>% 
      filter(Name == Machines[i]) %>% 
      filter(EventText == eventName) %>% 
      select(StartDate, AnalogVal, EventText)
    # extract 
    DF_B <- x %>% 
      filter(Name == Machines[i]) %>% 
      select(EventText, Name) %>% unique()
    
    DF_F <- anomalyscore_nn(DF_A, path_to_model, eventName, machineName = Machines[i])
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

## Turn this into function
# function that perform Neural Network on data from every machine and event
anomalyscore_nn <- function(x, path_to_model, eventName, machineName, n_cols){
  #function will convert data frame to image, apply NN and apply score to the original dataframe
  #x = dataframe with columns to perform Anomaly Detection scoring, it must contain data from one specific category, machine
  #path_to_model - path to model
  source("to_matrix.R")
  source("to_matrixDT.R")
  source("from_matrix.R")
  
  # get matrix for NN
  DF_M4 <- x %>% 
    to_matrix(filter_Event = eventName,
              filter_Machine = machineName,n_cols = n_cols)
  # get corresponding Date Time matrix
  DF_M4DT <- to_matrixDT(x, 
                         filter_Event = eventName,
                         filter_Machine = machineName, 
                         n_cols = n_cols)
  # initialize h2o
  h2o.init()
  # load model
  normality_model <- h2o.loadModel(path_to_model)
  # load dataset into H2O environment
  test_M4  <- as.h2o(x = DF_M4, destination_frame = "test_M4")
  # get mse output
  mse_out <- h2o.anomaly(normality_model, test_M4) %>% as.data.frame()
  # retrieve scored dataset
  DF_M4_Ready <- from_matrix(mse_out, DF_M4, DF_M4DT)
  # shutdown h2o
  h2o.shutdown(prompt = FALSE)
  # retrieve result
  return(DF_M4_Ready)
  
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