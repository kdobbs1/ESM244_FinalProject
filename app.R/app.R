library(shiny)
library(tidyverse)

ui <- fluidPage(
  navbarPage("My app name",
             tabPanel("Tab 1",
                      sidebarLayout(
                        sidebarPanel = ("WIDGETS"),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                      ),
             tabPanel("Tab 2"),
             tabPanel("Tab 3")
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)