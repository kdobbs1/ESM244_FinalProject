library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(fontawesome)
library(dplyr)
library(knitr)
library(DT)
library(ggplot2)
fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv")) %>% 
  filter(stressor!="air_temp") %>% 
  filter(stressor!="inorganic_pollution") %>%
  filter(stressor!="oceanographic") %>%
  filter(stressor!="poisons_toxins") %>%
  filter(stressor!="organic_pollution") %>%
  filter(stressor!="salinity") %>%
  filter(stressor!="storm_disturbance")
region_info<-read_csv(here("fish_data_app/data/spatial", "meow_rgns.csv"))
iucn_info<-read_csv(here("fish_data_app/data", "IUCN_data.csv")) %>% 
  janitor::clean_names()
stressor_info<-read_csv(here("fish_data_app/data", "stressor_info.csv")) %>% 
  filter(stressor!="air_temp") %>% 
  filter(stressor!="inorganic_pollution") %>%
  filter(stressor!="oceanographic") %>%
  filter(stressor!="poisons_toxins") %>%
  filter(stressor!="organic_pollution") %>%
  filter(stressor!="salinity") %>%
  filter(stressor!="storm_disturbance")

ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/4ee2c5c2ed.js"), 
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "fish.css")
  # ),
  theme=shinytheme("slate"),
 # theme = "ocean.css",
  navbarPage("Relative Impacts of Stressors on Commercially Viable Fish",
             tabPanel("Info", fluid=TRUE, icon=icon("globe-americas"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Learn more about the data here:"),
                          #Select species
                          selectInput(inputId = "pick_species1",
                                                label = "Choose species:",
                                                choices = unique(fish_info$species), 
                                                selected = "oncorhynchus mykiss"),
                          #Select stressor
                          selectInput(inputId = "pick_stressor1",
                                                label = "Choose stressor:",
                                                choices = unique(stressor_info$stressor), 
                                                selected="biomass_removal")
                                        ),
                        
                        mainPanel (textOutput("info"), textOutput("species_info_text"), textOutput("selected_var1"), imageOutput("image"), textOutput("test"))
                #
                      )
                    ),
             tabPanel("Summary Table", fluid=TRUE, tags$i(class = "fa-solid fa-user"), #icon is in the wrong location but it works?
                      #icon=icon("", lib = "font-awesome"),
                      sidebarLayout(
                        sidebarPanel (
                          titlePanel("Title Here"),
                          #select species
                          radioButtons(inputId = "pick_species2",
                                             label = "Choose species:",
                                             choices = unique(fish_info$species)),
                          #select stressor
                          # checkboxGroupInput(inputId = "pick_stressor2",
                          #                    label = "Choose stressor",
                          #                    choices = unique(fish_info$stressor))#,
                          #select region
                          #checkboxGroupInput(inputId = "pick_region",
                                            # label = "Choose region:",
                                            # choices = unique(region_info$realm))
                         
                           ),
                         mainPanel("OUTPUT!", DTOutput('table'))
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
                        
                        mainPanel(textOutput("plot_title"), plotOutput('fish_info_plot'))
                        
                        
                      )
                      ),
             tabPanel("Mapping", fluid=TRUE, icon=icon("globe-americas"), 
                      sidebarLayout(
                        sidebarPanel (
                                        selectInput(inputId = "pick_stressor4",
                                                    label = "Choose stressor:",
                                                    choices = unique(fish_info$stressor)),
                                        checkboxGroupInput(inputId = "pick_species4",       #need unique inputIds per widget
                                                           label = "Choose Species:",
                                                           choices = unique(fish_info$species)),
                                        
                        ),
                        
                        mainPanel ("OUTPUT" )
                        
                        
                      )
                      )
             
  )
)

server <- function(input, output) {
  
#info panel
  #reactive fxn for stressor info text
  stressor_info_reactive <- reactive({
    stressor_info %>% 
      filter(stressor %in% input$pick_stressor1) %>% 
      select(exp)
  })

  #reactive fxn for highest vuln score for a species
  most_impacted_stressor_reactive <- reactive({
    fish_info %>%
      filter(species %in% input$pick_species1) %>%
      select(stressor,vuln) %>%
      arrange(desc(vuln)) %>%
      slice(1) %>%
      select(stressor) %>%
      mutate(stressor = str_replace(stressor, pattern = "_", replacement = " "))
      #what to do when there is a tie????????????
  })

  ###OFFICE HOURS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #trying to make an expression that will slice whichever vulnerability scores are highest
  #depending on if there is a tie or not
  #reactive fxn for highest vuln score for a species
  # most_impacted_stressor_reactive <- reactive({
  #   fish_info %>% 
  #     filter(species %in% input$pick_species1) %>% 
  #     select(stressor,vuln) %>% 
  #     arrange(desc(vuln)) %>% 
  #     ifelse(fish_info$vuln[1]==fish_info$vuln[2], slice(2), slice(1))
  #     slice(1) %>% 
  #     select(stressor) %>% 
  #     mutate(stressor = str_replace(stressor, pattern = "_", replacement = " "))
  #   #what to do when there is a tie????????????
  # })
  
  #reactive fxn for IUCN status
  iucn_reactive<- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
      select(iucn_status)
  })
  
  #common name reactive
  cm_reactive<- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
      select(common_name)
  })
  
  #scientific name upper case
  sn_reactive<- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
      select(scientific_name_cap)
  })
  
  #reactive to remove _ from stressor for text output
  stressor_clean_reactive <- reactive({
    stressor_info %>% 
    filter(stressor %in% input$pick_stressor1) %>% 
    mutate(stressor = str_replace_all(stressor, pattern = "_", replacement = " ")) %>% 
    select(stressor)
  })
  
  #output with basic info about data that doesn't change
  output$info<-renderText({
    paste("This dataset was collected by Casey O'Hara. It examines the risk of impact 
          of different environmental stressors on different marine species by intersecting 
          spatial distributions according to each species' vulnerability to a given stressor.
          This analysis specifically explore the vulnerability of high commercial value
          marine fish species to different environmental stressors. You can learn more 
          about the species and how the stressors were defined by changing the inputs 
          in the panel on the left.")
  })
  
  #output that creates text with species info
  #replaced input$pick_species1 with reactive function
  output$species_info_text<-renderText({
    paste(sn_reactive(),", also known as", cm_reactive(), ", has an IUCN status of", 
          iucn_reactive(), ". Of the stressors tested,", sn_reactive(), " is most vulnerable to", 
          most_impacted_stressor_reactive(), ". This means that if the species was exposed to the same intensity of 
          all stressors tested, then", most_impacted_stressor_reactive() , "will have the greatest impact.")
  })
  
  #output that creates text with stressor info
  output$selected_var1<-renderText({
    paste(stressor_clean_reactive(), " is calculated according to the following:", stressor_info_reactive())
    })
  
  #reactive to produce file path of images
  #this works
  pic_reactive <- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
    # fish_info %>% 
    #   filter(species %in% input$pick_species1) %>% 
      select(scientific_name_lower) %>% 
      #fish_info[1,] %>% 
      toString() %>% 
      str_replace_all(pattern = " ", replacement = "_")
  })
  
  #############OFFICE HOURS
  #but this does not!!!!
  pic_file<-paste("www/","chanos_chanos",".jpg") %>%
   str_replace_all(pattern = " ", replacement = "")
  # pic_file<-paste("www/", pic_reactive(),".jpg") %>%
  #  str_replace_all(pattern = " ", replacement = "")
  
  
  #output for picture showing
  output$image<- renderImage({
    list(src = pic_file,
         #src = "www/brevoortia_patronus.jpg",
         width = "100%",
         height = 330)
  }, deleteFile = F)
    
  #Can remove this once the image is reactive
  #output to test image stuff
  output$test<-renderText({
    paste(pic_reactive())
  })
    
  #   renderPlot({
  #   ggdraw ()+
  #     draw_image(here("fish_data_app/data/pics", "brevoortia_patronus.jpg"))
  #     #draw_image("path to my image")
  # })

#plotting panel  
  #output that makes a reactive plot title
  output$plot_title<-renderText({
    paste("Impact of Stressors on", input$pick_species3)
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
  
#summary table panel  
  #data for the table
  table_data <- fish_info %>% 
    select(species, stressor, vuln)
  
  #reactive function for the table inputs
  table_reactive <- reactive({
    table_data %>% 
      filter(species %in% input$pick_species2) %>% 
      arrange(desc(vuln))
  })

  #output that creates the table
  output$table = renderDT({
    datatable(table_reactive()) %>% 
      DT::formatStyle(columns = names(table_data), color="lightgray") #column headers, show all rows at once
  }) 

  # Casey: We're writing the code to generate the map outside of the app to begin with, in "plot_testing.Rmd". We'll add it in once it's complete and behaving as expected.
}

shinyApp(ui = ui, server = server)