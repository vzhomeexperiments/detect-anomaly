# Esplore data ideas
library(tidyverse)
library(lubridate)
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

# create xts object (matrix and index)
DF_RecentXTS <- as.xts(DF_TEMP[, -1], order.by = as.POSIXct(DF_TEMP$StartDate))

# getting to know the perioficity of the data and the time span
periodicity(DF_RecentXTS)
# getting to know number of hours, seconds, etc
nseconds(DF_RecentXTS)
nhours(DF_RecentXTS)

# convert periodicity from seconds to hours
DF_RecentXTS_H <- period.apply(DF_RecentXTS, endpoints(DF_RecentXTS, "hours"), mean)

# let us select and group specific point in the dataframe
DF1 <- DF_TEMP %>% 
  filter(EventText == "Tubing Process, phase angle") %>% 
  arrange(StartDate) %>% 
  group_by(Name) %>% 
  
  head()
