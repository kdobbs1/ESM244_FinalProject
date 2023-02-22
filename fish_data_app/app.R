library(shiny)
library(tidyverse)
library(here)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv"))

ui <- fluidPage(
  navbarPage("My app name",
             tabPanel("Tab 1",
                      sidebarLayout(
                        sidebarPanel = (checkboxGroupInput(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)
                                                           )
                                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                    ),
             tabPanel("Tab 2",
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
             tabPanel("Tab 3", 
                      sidebarLayout(
                        sidebarPanel = ("WIDGETS",
                                        checkboxGroupInput(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)
                                        )
                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                      )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)