#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Text output"),
   
   textInput(inputId = "myText", label = "Input Text"),
   
   actionButton(inputId = "updateText", label = "Update Text"),
   
   textOutput(outputId = "myTextOutput")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    MyText <- reactive({ input$myText })
    
    updateMe <- eventReactive(input$updateText, {
      
      MyText()
      
    })
    
    output$myTextOutput <- renderText({ updateMe() })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

