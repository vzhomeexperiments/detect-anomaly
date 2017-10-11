# Esplore data ideas
library(tidyverse)

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


# ============= JOIN, Visualize DATA =================


# creating human readable data
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
xts_CUT_Ph <- merge(AnalogVal_mean, AnalogVal_max, AnalogVal__Min,AnalogVal_sd)

# view dataset we have now
head(xts_CUT_Ph)

# now we can return to the dataframe




# let us select and group specific point in the dataframe
DF1 <- DF_TEMP %>% 
  filter(EventText == "Tubing Process, phase angle") %>% 
  arrange(StartDate) %>% 
  group_by(Name) %>% 
  
  head()
