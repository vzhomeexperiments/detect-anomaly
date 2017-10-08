# Esplore data ideas
library(tidyverse)
# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_All <- readRDS("DF_Data_Process.data") #

# Read our small data second ... 
DF_Data_Recent <- readRDS("DF_Data_Process_Recent.data") #

DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP <- DF_Data_All %>% 
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


