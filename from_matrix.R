# function from_matrix
# PURPOSE: output original time-series dataframe with attached column for Mean Square Error of the model
# USAGE: function will replicate the MSE error result and parse it back to corresponding rows of matrix
#        then it will convert matrix to vectors and construct the dataframe
from_matrix <- function(x_mse, x_Val, x_DT) {
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