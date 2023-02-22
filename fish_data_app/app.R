library(shiny)
library(tidyverse)
library(here)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv"))

ui <- fluidPage(
  navbarPage("My app name",
             tabPanel("Info",
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
                        sidebarPanel("WIDGETS",
                                        checkboxGroupInput(inputId = "pick_stressor",
                                                           label = "Choose stressor:",
                                                           choices = unique(fish_info$stressor)
                                        )
                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                      ),
             tabPanel("Mapping", 
                      sidebarLayout(
                        sidebarPanel ("WIDGETS",
                                        selectInput(inputId = "pick_species",
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)
                                        )
                        ),
                        
                        mainPanel = ("OUTPUT!")
                        
                        
                      )
                      ),
             tabPanel("Summary Table", 
                      sidebarLayout(
                        sidebarPanel ("WIDGETS",
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
  
  output$fish_info_reactive <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = stressor, y = vuln)) +
      geom_point(aes(color = species))
  )
}

shinyApp(ui = ui, server = server)