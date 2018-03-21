# Function score_process
# PURPOSE: run seals data through the h2o deep learning model
# OUTPUT: matrix with MSE output predicted
score_process <- function(x, path_to_model){
  #x - data typically data frame in Server.R SEAL_TEMP
  #x <- SEAL_NN
  #path_to_model - path to h2o model stored in the www folder of the app
  #path_to_model <- "www/tmp/normality_model.bin/DeepLearning_id20180321"
  #require(h2o)
  # start h2o cluster
  h2o.init(nthreads = 2)
  # load model to h2o environment
  normality_model <- h2o.loadModel(path_to_model)
  # load dataset to h2o environment
  Process_Score  <- as.h2o(x = x, destination_frame = "Process_Score")
  # get the mse result
  mso_out <- h2o.anomaly(normality_model, Process_Score) %>%  as.data.frame() # plot.ts(mso_out)
  colnames(mso_out) <- "AnomalyRating"
  # shutdown h2o cluster
  h2o.shutdown(prompt = FALSE)
  # return dataframe column
  return(mso_out)
}
