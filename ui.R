# ----------------------------
# Description: Shiny app for Udemy course "Identify Problem with Artificial Intelligence"
# Author: Vladimir Zhbanko
# Date: 2017-08-27
# Version: 00.01
# Date: 
# Version: 
# Changed: 
# ----------------------------

library(shiny)
library(shinydashboard)
library(DT)

# Define steps choices for selectInput function (it is containing steps the user can filter)
stepsChoices <- c("Step 1 SubStep 1",
                  "Step 1 SubStep 2",
                  "Step 1 SubStep 3",
                  "Step 1 SubStep 4",
                  "Step 2 SubStep 1",
                  "Step 2 SubStep 2",
                  "Step 2 SubStep 3",
                  "Step 2 SubStep 4",
                  "Step 2 SubStep 5",
                  "Step 2 SubStep 6",
                  "Step 2 SubStep 7")

# 
dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    # Elements on the Sidebar of the App
    
  ),
  dashboardBody(
    
    mainPanel(
      
      # Elements of the Dashboard: header and tabset panel
      headerPanel("Visualization of steps duration"),
      tabsetPanel(
        # Place Tab panels here
        
        
        
        
      )  
    )
  )
)
