# Train and Save Model (use this script to refresh h2o model after h2o update)

### Productionize the model: Code to Train and Save the model

library(tidyverse)
library(h2o)
# start h2o
h2o.init(nthreads = 2)
# data reading
NORM_2016 <- read_rds("DATA-normal.rds") %>% select(2:11) %>% as.matrix() 
# load dataset
train <- as.h2o(x = NORM_2016, destination_frame = "train")
# create model
normality_model <- h2o.deeplearning(x = names(train), 
                                    model_id = "DeepLearning_ProcessDemo",
                                    training_frame = train, 
                                    activation = "Tanh", 
                                    autoencoder = TRUE, 
                                    hidden = c(8,5,8), 
                                    sparse = TRUE,
                                    l1 = 1e-4, 
                                    epochs = 100)
# save the model
h2o.saveModel(normality_model, file.path(getwd(), "www/tmp/normality_model.bin"), force = TRUE)

# shutdown JVM
h2o.shutdown(prompt = F)
