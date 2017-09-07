### Global script, executed code is available for shiny app ui.R and server.R

# Temporary data munging and cleaning

# # TO ADD OUTLIER REMOVAL FUNCTION
# 
# df <- DF_Data %>% 
#   # arrange by date 
#   arrange(StartDateTime) %>% 
#   # join to decode equipment serial number
#   inner_join(DF_Equipm, by = "IDEquipment") %>% 
#   # select only column needed
#   select(StartDateTime, SN, EventCode, TimeTotal) %>% 
#   # filter to exclude line from Pakistan
#   filter(SN != "21215/00157") %>% 
#   # join to decode Event Code meaning
#   inner_join(DF_EvCode, by = "EventCode") %>% 
#   # select only column needed
#   select(StartDateTime, SN, EventCode, TimeTotal, EventText)
# 
# df[1:200,] %>% 
#   filter(EventText == "Ladder step SIGNAL TO STERILIZER") %>% 
#   ggplot(aes(x = StartDateTime, y = TimeTotal)) + geom_point()