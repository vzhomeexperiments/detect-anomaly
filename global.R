library(xts)
library(tidyverse)
library(h2o)


# function that perform scoring using Deep Learning model to each machine and return common dataframe with MSE score
anomalyscore_machines <- function(x, Machines, path_to_model, n_cols = 150){
  # apply Neural Network Model for each machine
  # x - dataframe containing DF_TEMP data structure
  # path to model - location of the Model stored as a .bin object
  # Machines - vector containing machine names

  for(i in 1:length(Machines)){
    # extract one machine, all events
    DF_A <- x %>% 
      filter(Name == Machines[i]) %>% 
      select(StartDate, AnalogVal, EventText)
    # extract 
    DF_B <- x %>% 
      filter(Name == Machines[i]) %>% 
      select(EventText, Name) %>% unique()
    
    DF_F <- anomalyscore_nn(DF_A, path_to_model, n_cols = n_cols)
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
anomalyscore_nn <- function(x, path_to_model, n_cols){
  #function will convert data frame to image, apply NN and apply score to the original dataframe
  #x = dataframe with columns to perform Anomaly Detection scoring, it must contain data from one specific category, machine
  #path_to_model - path to model
  
  # get matrix for NN
  DF_M4 <- x %>% 
    to_m(n_cols = n_cols)
  # get corresponding Date Time matrix
  DF_M4DT <- to_mDT(x, n_cols = n_cols)
  
  # load model
  normality_model <- h2o.loadModel(path_to_model)
  # load dataset into H2O environment
  test_M4  <- as.h2o(x = DF_M4, destination_frame = "test_M4")
  # get mse output
  mse_out <- h2o.anomaly(normality_model, test_M4) %>% as.data.frame()
  # retrieve scored dataset
  DF_M4_Ready <- from_m(mse_out, DF_M4, DF_M4DT)
  
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

# -----------------------------------------------------
# Function converting time series data to matrix
to_m <- function(x, n_cols) {
  ### PURPOSE: Transform Time Series Column of the dataframe to the matrix
  #            with specified number of columns. Number of rows will be automatically
  #            found and remaining data points discarded
  # # Uncomment variable to debug function
  # x <- DF_TEMP
  # n_cols <- 150
  
  # get intermediate object and dimension
  Step1 <- x %>% 
    arrange(StartDate) %>% 
    select(AnalogVal)
  # find number of rows of data frame
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  # WN <- 19.2
  # WN <- 19.8
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}
  # find number of rows to extract data
  n <- n_cols * WN
  # extract relevant matrix
  Step2 <- Step1 %>% 
    head(n) %>% #only use whole number to avoid errors
    t() %>%  # this brings us a matrix
    matrix(nrow = WN, ncol = n_cols, byrow = TRUE) # transforming that into matrix size 20x150
  # return the result of the function
  return(Step2)
}

# -----------------------------------------------------
# Function converting time series data to matrix
to_mDT <- function(x, n_cols) {
  ### PURPOSE: Transform Time Series Column of the dataframe to the matrix
  #            with specified number of columns. Number of rows will be automatically
  #            found and remaining data points discarded
  # # Uncomment variable to debug function
  # filter_Event <- "Tubing Process, resistance Ohm"
  # filter_Machine <- "Machine #4"
  # x <- DF_TEMP
  # n_cols <- 150
  
  # get intermediate object and dimension
  Step1 <- x %>% 
    arrange(StartDate) %>% 
    select(StartDate)
  # find number of rows of data frame
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  # WN <- 19.2
  # WN <- 19.8
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}
  # find number of rows to extract data
  n <- n_cols * WN
  # extract relevant matrix
  Step2 <- Step1 %>% 
    head(n) %>% #only use whole number to avoid errors
    t() %>%  # this brings us a matrix
    matrix(nrow = WN, ncol = n_cols, byrow = TRUE) # transforming that into matrix size 20x150
  # return the result of the function
  return(Step2)
}

#-----------------------------------------------------------
# function from_matrix
# PURPOSE: output original time-series dataframe with attached column for Mean Square Error of the model
# USAGE: function will replicate the MSE error result and parse it back to corresponding rows of matrix
#        then it will convert matrix to vectors and construct the dataframe
from_m <- function(x_mse, x_Val, x_DT) {
  # x_mse - dataframe with model scoring results
  # x_Val - original matrix with Analog values
  # x_DT  - original matrix with corresponding Date Time values
  # x_Val <- DF_M4
  # x_DT  <- DF_M4DT
  # find the number of rows to replicate
  n_cols <- dim(x_Val)[2]
  # check for dimension of supplied matrixes
  # Add stopifnot() to check dimensions of both matrixes
  stopifnot(dim(x_Val) == dim(x_DT))
  # mse_out is a dataframe containing MSE error for each row of the matrix, we replicate this... 150 times...
  DF_M4mse <- do.call(cbind, replicate(n_cols, as.matrix(x_mse), simplify=FALSE))
  
  # going to combine this result with our objects DF_M4 and DF_M4DT
  DF_V <- x_Val %>% t() %>% c() %>% as.data.frame() 
  colnames(DF_V) <- "AnalogVal"
  DF_T <- x_DT %>% t() %>% c() %>% as.POSIXct() %>%  as.data.frame() 
  colnames(DF_T) <- "StartDate"
  DF_A <- DF_M4mse %>% t() %>% c() %>% as.data.frame() 
  colnames(DF_A) <- "AnomalyRating"
  
  # return final dataframe:
  DF_F <- DF_V %>% bind_cols(DF_T, DF_A)
  return(DF_F)
}
