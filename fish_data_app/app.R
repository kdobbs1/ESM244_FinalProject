library(shiny)
library(tidyverse)

ui <- fluidPage(
  navbarPage("My app name",
             tabPanel("Tab 1",
                      sidebarLayout(
                        sidebarPanel = ("WIDGETS",
                                        checkboxGroupInput(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)
                                                           )
                                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                      ),
             tabPanel("Tab 2"),
             tabPanel("Tab 3")
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)