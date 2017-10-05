# Esplore data ideas
library(tidyverse)
# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_Process_All <- readRDS("DF_Data_Process.data")

# Read our small data second ... 
DF_Data_Process_Recent <- readRDS("DF_Data_Process_Recent.data") 
# ============= END OF READ DATA =================


# ============= JOIN, Visualize DATA =================
# data frame containing information from multiple sensors
DF_Data <- readRDS("DF_Data_Process_Recent.data")
# data frame containing equipment information
DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP <- DF_Data %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDate, Name, AnalogVal, EventText)


# Visualize the data...
DF_Data_Process_All %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(StartDate, Name, EventText, AnalogVal) %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = Name)) + geom_point()+facet_grid(~Name)



