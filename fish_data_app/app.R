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
                          radioButtons(inputId = "pick_species1",
                                                label = "Choose species:",
                                                choices = unique(fish_info$species)),
                          #Select stressor
                          radioButtons(inputId = "pick_stressor1",
                                                label = "Choose stressor:",
                                                choices = unique(fish_info$stressor))
                                        ),
                        
                        mainPanel ("OUTPUT!")
                        
                        
                      )
                    ),
             tabPanel("Summary Table", 
                      sidebarLayout(
                        sidebarPanel (
                          checkboxGroupInput(inputId = "pick_species2",
                                             label = "Choose species:",
                                             choices = unique(fish_info$species)
                          )
                        ),
                        
                        mainPanel ("OUTPUT!", tableOutput('table'))
                        
                        
                      )
             ),
             tabPanel("Plotting",
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "pick_species3",
                                                  label = "Choose species:",
                                                  choices = unique(fish_info$species), 
                                                  selected = fish_info$species[1]),
                                     checkboxGroupInput(inputId = "pick_stressor3",
                                                        label = "Choose stressor:",
                                                        choices = unique(fish_info$stressor))
                        ),
                        
                        mainPanel("OUTPUT!", plotOutput('fish_info_plot'))
                        
                        
                      )
                      ),
             tabPanel("Mapping", fluid=TRUE, icon=icon("globe-americas"), 
                      sidebarLayout(
                        sidebarPanel (
                                        selectInput(inputId = "pick_species4",       #need unique inputIds per widget
                                                           label = "Choose species:",
                                                           choices = unique(fish_info$species)),
                                        selectInput(inputId = "pick_stressor4",
                                                    label = "Choose stressor:",
                                                    choices = unique(fish_info$stressor))
                        ),
                        
                        mainPanel ("OUTPUT" )
                        
                        
                      )
                      )
             
  )
)

server <- function(input, output) {
  
  fish_info_reactive <- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species3) %>% 
      filter(stressor %in% input$pick_stressor3) 
  })
  # output$fish_info_plot <- renderPlot(
  #   ggplot(data = fish_info_reactive(), aes(x = stressor, y = vuln)) +
  #     geom_point(aes(color = species)) + theme_minimal()
  # )
  output$fish_info_plot <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = stressor, y=vuln)) +
      geom_col(aes(color = vuln, fill=vuln)) + theme_minimal()
  )
  
  table_reactive <-reactive({
    fish_info %>% 
      filter(stressor %in% input$input_stressor) %>% 
      filter(vuln > 0.5) %>% 
      select(species, vuln)
  })
  
 output$table<-renderTable(table_reactive(), )
}

shinyApp(ui = ui, server = server)