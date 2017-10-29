#  Esplore data ideas
# 
#  Objective: using specialized packages for Anomaly Detection in R...
#
library(tidyverse)
library(xts)
# devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_All <- readRDS("DF_Data_Process.data")
# data about equipment
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


# extract data for one machine and one sub-process
DF_M1_Cut_Ph <- DF_TEMP %>% 
  filter(EventText == "Cutting Process, phase angle") %>% 
  filter(Name == "Machine #3") %>% 
  select(StartDate, AnalogVal)

# # create xts object (matrix and index)
# xts_M1_Cut_Ph <- as.xts(DF_M1_Cut_Ph[, -1], order.by = as.POSIXct(DF_M1_Cut_Ph$StartDate))
# # use plot.xts
# plot.xts(xts_M1_Cut_Ph)
# # getting to know the perioficity of the data and the time span
# periodicity(xts_M1_Cut_Ph)
# # getting to know number of hours, seconds, etc
# nseconds(xts_M1_Cut_Ph)
# nhours(xts_M1_Cut_Ph)

# example featured by the package Anomaly Detection
data(raw_data)
res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
res$plot

# applied to our data
res = AnomalyDetectionTs(DF_M1_Cut_Ph, max_anoms=0.02, direction='both', plot=TRUE)
# getting the plot out ot it
res$plot

# we can also just plot anomalies we have
plot(res$anoms)

# as it seems to be the anomalies are distributed around 2 groups...

# ============= READ DATA =================
# Read our big data first ... 9 mln rows...
DF_Data_Recent <- readRDS("DF_Data_Process_Recent.data")
# data about equipment
DF_Equipm <- read_csv("DF_EquipmData.csv")
# data frame containing Event Names
DF_EvCode <- read_csv("DF_EvCodeDataProject.csv")

# Data manipulation and saving to the DF_TEMP
DF_TEMP1 <- DF_Data_Recent %>% 
  # join to decode equipment serial number
  inner_join(DF_Equipm, by = "IDEquipment") %>% 
  # join to decode Event Code meaning
  inner_join(DF_EvCode, by = "EventCode") %>% 
  # select only column needed
  select(StartDate, Name, AnalogVal, EventText)

# ============= END OF READ DATA =================


# extract data for one machine and one sub-process
DF_M3_Tub_Res1 <- DF_TEMP1 %>% 
  filter(EventText == "Tubing Process, resistance Ohm") %>% 
  filter(Name == "Machine #3") %>% 
  select(StartDate, AnalogVal) %>% 
  arrange(StartDate)

dim(DF_M3_Tub_Res1) # 332620 rows






