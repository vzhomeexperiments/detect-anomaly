# Esplore data ideas
library(tidyverse)
# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_Seals_All <- readRDS("DF_Data_Seals_All.data")

# Read our small data second ... 
DF_Data_Seals_Recent <- readRDS("DF_Data_Seals_Recent.data")

# Read our Ev.Codes...
DF_EvCode <- readRDS("DF_EvCode.data")

# Read our Equipment
DF_Equipm <- readRDS("DF_Equipm.data")

# Read our Label
DF_Data_Label <- readRDS("DF_Data_Label.data")

# ============= END OF READ DATA =================

# ============= JOIN, Visualize DATA =================

# bringing unique table of events logged with the data
DF_Temp <- DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(EventText, EventCode) %>% 
  unique()

# bringing unique table of machines logged with the data
DF_Temp1 <- DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(SN) %>% 
  unique()

# creating human readable data
DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(StartDate, SN, EventText, AnalogVal) %>% 
  filter(EventText == "Longitudinal sealing, phase") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = SN)) + geom_point()+facet_grid(~SN)

# creating human readable data
DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(StartDate, SN, EventText, AnalogVal) %>% 
  filter(EventText == "Longitudinal sealing, impedance") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = SN)) + geom_point()+facet_grid(~SN)

# creating human readable data
DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(StartDate, SN, EventText, AnalogVal) %>% 
  filter(EventText == "Strip applicator, impedance") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = SN)) + geom_point()+facet_grid(~SN)

# creating human readable data
DF_Data_Seals_Recent %>% 
  left_join(DF_EvCode, by = "EventCode") %>% 
  left_join(DF_Equipm, by = "IDEquipment") %>% 
  select(StartDate, SN, EventText, AnalogVal) %>% 
  filter(EventText == "Strip applicator, phase") %>% 
  ggplot(aes(x = StartDate, y = AnalogVal, col = SN)) + geom_point()+facet_grid(~SN)


