library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv"))
region_info<-read_csv(here("fish_data_app/data/spatial", "meow_rgns.csv"))

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
                          titlePanel("Title Here"),
                          #select species
                          checkboxGroupInput(inputId = "pick_species2",
                                             label = "Choose species:",
                                             choices = unique(fish_info$species)),
                          #select stressor
                          checkboxGroupInput(inputId = "pick_stressor2",
                                             label = "Choose stressor",
                                             choices = unique(fish_info$stressor)),
                          #select region
                          checkboxGroupInput(inputId = "pick_region",
                                             label = "Choose region:",
                                             choices = unique(region_info$realm))
                         
                           ),
                         mainPanel ("OUTPUT!", tableOutput('table'))
                         )
                      ),
             tabPanel("Plotting",
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "pick_species3",
                                                  label = "Choose species:",
                                                  choices = unique(fish_info$species), 
                                                  selected = fish_info$species[2]),
                                     checkboxGroupInput(inputId = "pick_stressor3",
                                                        label = "Choose stressor:",
                                                        choices = unique(fish_info$stressor), 
                                                        selected = c(fish_info$stressor[1], fish_info$stressor[5], fish_info$stressor[8]))
                        ),
                        
                        mainPanel("Impact of Stressors on" , plotOutput('fish_info_plot'))
                        
                        
                      )
                      ),
             tabPanel("Mapping", fluid=TRUE, icon=icon("globe-americas"), 
                      sidebarLayout(
                        sidebarPanel (
                                        selectInput(inputId = "pick_stressor4",
                                                    label = "Choose stressor:",
                                                    choices = unique(fish_info$stressor)),
                                        checkboxGroupInput(inputId = "pick_species4",       #need unique inputIds per widget
                                                           label = "Choose Specieseses:",
                                                           choices = unique(fish_info$species)),
                                        
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

  output$fish_info_plot <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = stressor, y=vuln)) +
      geom_col(aes(color = vuln, fill=vuln)) + theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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