#  Esplore data ideas
# 
#  Objective: do some feature engineering, convert to time series, create some new features from the data, return back to dataframes...
#
library(tidyverse)
library(xts)

# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_All <- readRDS("DF_Data_Process.data") #

# Read our small data second ... 
DF_Data_Recent <- readRDS("DF_Data_Process_Recent.data") 

DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP <- DF_Data_Recent %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDate, Name, AnalogVal, EventText)

# ============= END OF READ DATA =================

########### Plan ###########
# 1. For each specific machine solve the problem: 
#   - convert to xts object
#   - find periodicity, esplore data
#   - aggregate by hour and create new features
#   - merge into one object
#   - return back to the dataframe
#   - join back initial features of the object
# 2. Create function, apply this to every single machine, process...
# 3. Deploy for ShinyApp
# 4. Verify on the dataset & compare...

# ============= JOIN, Visualize DATA =================


# creating human readable data and visualize them
DF_TEMP %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = Name)) + geom_point()+facet_grid(~Name)

# =================================================================================
# use xts package to manipulate time data series object and do feature engineering
library(xts)

# we can apply transformation to each specific machine and each specific sub-process
DF_M1_Cut_Ph <- DF_TEMP %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  filter(Name == "Machine #1") %>% 
  select(StartDate, AnalogVal)

# create xts object (matrix and index)
xts_M1_Cut_Ph <- as.xts(DF_M1_Cut_Ph[, -1], order.by = as.POSIXct(DF_M1_Cut_Ph$StartDate))

# getting to know the perioficity of the data and the time span
periodicity(xts_M1_Cut_Ph)
# getting to know number of hours, seconds, etc
nseconds(xts_M1_Cut_Ph)
nhours(xts_M1_Cut_Ph)

# convert periodicity from seconds to hours and apply some functions to create new features
AnalogVal_mean <- period.apply(xts_M1_Cut_Ph, endpoints(xts_M1_Cut_Ph, "hours"), mean)
AnalogVal_max <- period.apply(xts_M1_Cut_Ph, endpoints(xts_M1_Cut_Ph, "hours"), max)
AnalogVal__Min <- period.apply(xts_M1_Cut_Ph, endpoints(xts_M1_Cut_Ph, "hours"), min)
AnalogVal_sd <- period.apply(xts_M1_Cut_Ph, endpoints(xts_M1_Cut_Ph, "hours"), sd)
# review obtained object
head(AnalogVal_mean)
head(AnalogVal_max)
head(AnalogVal__Min)
head(AnalogVal_sd)

# merge these features together
xts_CUT_Ph <- merge(AnalogVal_mean, AnalogVal_max, AnalogVal__Min, AnalogVal_sd)

# view dataset we have now
head(xts_CUT_Ph)

# now we can return to the dataframe
cd <- coredata(xts_CUT_Ph) %>% as.data.frame(row.names = F) 
cd$StartDate <- index(xts_CUT_Ph)

# view dataset we have now
head(cd)


## Turn this into function
# maybe it's inefficient way to programming, but the result is ready...
feature_eng_ts <- function(x, funcToApply = c("mean", "min", "max", "sd")){
  #function will return new dataframe with new features
  #x = dataframe with columns to perform feature engineering, it must contain data from one specific category
  #tscolname = column name containing time series data
  #avcolname - column name contining data to perform manipulations
  #funcToApply - vector containing functions to apply data transformations
  # convert to xts object
  DF_M1_xts <- as.xts(x[-1], order.by = as.POSIXct(x$StartDate))
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
  # return result
  return(DF_M1_NF)
}

# example of usage
# we can apply transformation to each specific machine and each specific sub-process
DF <- DF_TEMP %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  filter(Name == "Machine #1") %>% 
  select(StartDate, AnalogVal)

# keep just categories of the data: machine and event text
DF_Cat <- DF_TEMP %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  filter(Name == "Machine #1") %>% select(EventText, Name) %>% unique()

# get new dataset with new features
DF_FE <- feature_eng_ts(DF)
# join original categories
DF_FE$EventText <- DF_Cat[1,1]
DF_FE$Name <- DF_Cat[1,2]

# review obtained dataset
head(DF_FE)

# plot some variables
ggplot(DF_FE, aes(x = StartDate, y = AnalogVal_sd))+geom_point()
