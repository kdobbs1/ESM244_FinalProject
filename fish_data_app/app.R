library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(fontawesome)
library(dplyr)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv"))
region_info<-read_csv(here("fish_data_app/data/spatial", "meow_rgns.csv"))
#fish_name_info<-read_csv(here("fish_data_app/data", "ESM244FishSpecies.csv")) %>% 
 # mutate_all(funs=tolower)
stressor_info<-read_csv(here("fish_data_app/data", "stressor_info.csv"))


ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/4ee2c5c2ed.js"), 
  theme=shinytheme("slate"),
  navbarPage("Fun Fish Data World",
             tabPanel("Info", fluid=TRUE, icon=icon("globe-americas"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Title Here"),
                          #Select species
                          selectInput(inputId = "pick_species1",
                                                label = "Choose species:",
                                                choices = unique(fish_info$species), 
                                                selected = "oncorhynchus mykiss"),
                          #Select stressor
                          selectInput(inputId = "pick_stressor1",
                                                label = "Choose stressor:",
                                                choices = unique(fish_info$stressor), 
                                                selected="sst_rise")
                                        ),
                        
                        mainPanel ("OUTPUT!", textOutput("selected_var"), textOutput("selected_var1"))
                        
                        
                      )
                    ),
             tabPanel("Summary Table", fluid=TRUE, 
                      tags$i(class = "fa-solid fa-user"),
                      #icon=icon("", lib = "font-awesome"),
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
                                             choices = unique(fish_info$stressor))#,
                          #select region
                          #checkboxGroupInput(inputId = "pick_region",
                                            # label = "Choose region:",
                                            # choices = unique(region_info$realm))
                         
                           ),
                         mainPanel ("OUTPUT!", tableOutput('table'))
                         )
                      ),
             tabPanel("Plotting", fluid=TRUE, icon=icon("fa-solid fa-chart-column", lib = "font-awesome"), # From glyphicon library,
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "pick_species3",
                                                  label = "Choose species:",
                                                  #choices = unique(fish_info$species), 
                                                  choices = c("Brevoortia patronus"="brevoortia patronus",
                                                              "*Chanos chanos*"="chanos chanos",
                                                              "*Clupea harengus*"="clupea harengus",
                                                              "*Engrulis japonicus*"="engrulis japonicus",
                                                              "*Engraulis ringens*"="engraulis ringens",
                                                              "*Gadus morhua*"="gadus morhua",
                                                              "*Katsuwonus pelamis*"="katsuwonus pelamis",
                                                              "*Mallotus villosus*"="mallotus villosus",
                                                              "*Oncorhynchus mykiss*"="oncorhynchus mykiss",
                                                              "*Salmo salar*"="salmo salar",
                                                              "*Sardina pilchardus*"="sardina pilchardus",
                                                              "*Sardinella longiceps*"="sardinella longiceps",
                                                              "*Scomber japonicus*"="scomber japonicus",
                                                              "*Scomber scombrus*"="scomber scombrus",
                                                              "*Thunnus albacares*"="thunnus albacares",
                                                              "*Trichiurus lepturus*"="trichiurus lepturus"
                                                              ),
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
  
  #reactive fxn for stressor info text
  stressor_info_reactive <- reactive({
    stressor_info %>% 
      filter(stressor %in% input$pick_stressor1) 
  })

  #reactive fxn for highest vuln score for a species
  most_impacted_stressor_reactive<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species1) %>% 
      filter(max(vuln))
  })
  
  #output that creates text with species info
  output$selected_var<-renderText({
    paste(input$pick_species1, "has an IUCN status of", most_impacted_stressor_reactive())
  })
  
  #output that creates text with stressor info
  output$selected_var1<-renderText({
    paste(input$pick_stressor1, ":", stressor_info_reactive())
  })
  
  #reactive fxn for plot
  fish_info_reactive <- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species3) %>% 
      filter(stressor %in% input$pick_stressor3) 
  })

  #output that creates plot
  output$fish_info_plot <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = stressor, y=vuln)) +
      geom_col(aes(color = vuln, fill=vuln)) + theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
  table_reactive <-reactive({
    fish_info %>% 
      filter(species %in% input$pick_species2) %>% 
      filter(stressor %in% input$pick_stressor2) %>% 
      filter(vuln > 0.5) %>% 
      select(species, vuln)
  })
  
 output$table<-renderTable(table_reactive(), )
}

shinyApp(ui = ui, server = server)