library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv"))

ui <- fluidPage(theme=shinytheme("slate"),
  navbarPage("Fun Fish Data World",
             tabPanel("Info", fluid=TRUE, icon=icon("globe-americas"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Title Here"),
                          #Select species
                          radioButtons(inputId = "pick_species",
                                                label = "Choose species:",
                                                choices = unique(fish_info$species)),
                          #Select stressor
                          radioButtons(inputId = "pick_stressor",
                                                label = "Choose stressor:",
                                                choices = unique(fish_info$stressor))
                                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                    ),
             tabPanel("Plotting",
                      sidebarLayout(
                        sidebarPanel(
                                        radioButtons(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)),
                                     checkboxGroupInput(inputId = "pick_stressor",
                                                        label = "Choose stressor:",
                                                        choices = unique(fish_info$stressor))
                        ),
                        
                        mainPanel = ("OUTPUT!", 
                                     plotOutput(fish_info_plot))
                        
                        
                      )
                      ),
             tabPanel("Mapping", fluid=TRUE, icon=icon("globe-americas"), 
                      sidebarLayout(
                        sidebarPanel (
                                        selectInput(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)),
                                        selectInput(inputId = "pick_stressor",
                                                    label = "Choose stressor:",
                                                    choices = unique(fish_info$stressor))
                        ),
                        
                        mainPanel = ("OUTPUT", )
                        
                        
                      )
                      ),
             tabPanel("Summary Table", 
                      sidebarLayout(
                        sidebarPanel (
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

server <- function(input, output) {
  
  fish_info_reactive <- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species) %>% 
      filter(stressor %in% input$input_stressor)
  })
  
  output$fish_info_plot <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = stressor, y = vuln)) +
      geom_point(aes(color = species))
  )
}

shinyApp(ui = ui, server = server)