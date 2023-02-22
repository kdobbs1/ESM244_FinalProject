library(shiny)
library(tidyverse)
library(here)
fish_info<-read_csv(here("data", "fish_info.csv"))

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