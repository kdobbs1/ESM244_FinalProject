library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(fontawesome)
library(dplyr)
library(knitr)
library(DT)
library(ggplot2)
library(RColorBrewer)

# for mapping
library(terra)
library(sf)
library(mapproj)
library(maptools)
library(ggnewscale)


################# Setting paths

data_path <- 'fish_data_app/data'

source(here(data_path, 'helper_fxns.R'))


################# Front End Data Processing

##### fish info

fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv")) %>% 
  filter(stressor!="air_temp") %>% 
  filter(stressor!="inorganic_pollution") %>%
  filter(stressor!="oceanographic") %>%
  filter(stressor!="poisons_toxins") %>%
  filter(stressor!="organic_pollution") %>%
  filter(stressor!="salinity") %>%
  filter(stressor!="storm_disturbance")%>% 
  mutate(stressor = str_replace_all(stressor, pattern = "_", replacement = " "))
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
  filter(stressor!="storm_disturbance") %>% 
  mutate(stressor = str_replace_all(stressor, pattern = "_", replacement = " "))
iucn_meaning<-read_csv(here("fish_data_app/data", "iucn_meaning.csv"))

##### For map

fish_info_map <- read_csv(here(data_path, 'fish_info.csv'))  %>% 
  filter(stressor!="air_temp",
         stressor!='biomass_removal',
         stressor!='entanglement_macroplastic',
         stressor!="inorganic_pollution",
         stressor!="habitat_loss_degradation",
         stressor!="noise_pollution",
         stressor!="oceanographic",
         stressor!="organic_pollution",
         stressor!="poisons_toxins",
         stressor!="organic_pollution",
         stressor!="salinity",
         stressor!="sedimentation",
         stressor!="storm_disturbance",
         stressor!="wildlife_strike") 

am_species <- c('chanos chanos', 
                'gadus morhua', 
                'mallotus villosus',
                'oncorhynchus mykiss',
                'salmo salar',
                'trichiurus lepturus')

iucn_species <- c('brevoortia patronus',
                  'clupea harengus',
                  'engraulis japonicus',
                  'engraulis ringens',
                  'katsuwonus pelamis',
                  'sardina pilchardus',
                  'sardinella longiceps',
                  'scomber japonicus',
                  'scomber scombrus',
                  'thunnus albacares')

iucn_species_dict <- c('brevoortia patronus' = '191208',
                       'clupea harengus' = '155123',
                       'engraulis japonicus' = '170306',
                       'engraulis ringens' = '183775',
                       'katsuwonus pelamis' = '170310',
                       'sardina pilchardus' = '198580',
                       'sardinella longiceps' = '154989',
                       'scomber japonicus' = '98969433',
                       'scomber scombrus' = '170354',
                       'thunnus albacares' = '21857')

# make dictionary with all stressor and file names
stressor_tif_dict <- c("air_temp" = "",   
                       "biomass_removal" = "",
                       "bycatch" = "bycatch_benthic_2017",  # there are two bycatch files with different suffixes?
                       "entanglement_macroplastic" = "",
                       "eutrophication_nutrient_pollution" = "nutrient_2020",
                       "habitat_loss_degradation" = "",
                       "inorganic_pollution" = "",
                       "light_pollution" = "light_2018",
                       "marine_heat_waves" = "sst_extremes_2020",             # is this where sst_extreme should go?
                       "noise_pollution" = "",
                       "ocean_acidification" = "ocean_acidification_2020",
                       "oceanographic" = "",
                       "organic_pollution" = "",
                       "plastic_pollution_microplastic" = "microplastics_2015",
                       "poisons_toxins" = "",
                       "salinity" = "",
                       "sedimentation" = "",
                       "sst_rise" = "spp_max_temp",     # this is located in a different location than the others, and is only part of the file name
                       "storm_disturbance" = "",
                       "uv_radiation" = "uv_radiation_2020",
                       "wildlife_strike" = ""
)

####################################################################
##################### Define mapping function ######################
####################################################################

map_stress_range <- function(species_name, stressor_name) {
  
  stressor_choice <- c(stressor_name)
  species_choice <- species_name
  
  # format inputs for feeding to file chains
  species_choice_formatted <- sub(' ', '_', tolower(species_choice))
  species_name_file <- species_choice_formatted
  # set source depending on species
  src <- ''
  if (species_choice %in% am_species) {
    src <- 'am'
  }
  if (species_choice %in% iucn_species) {
    src <- 'iucn'
    species_name_file <- iucn_species_dict[species_choice]
  }
  
  ##### format STRESSOR file names for calls according to choice
  # initialize path and name variables
  stressor_tif_name <- ''
  stressor_tif_path_addition <- ''
  stressor_tif_path <- ''
  # add to path and name for sea surface temperature maps
  if (stressor_choice == "sst_rise") {
    sst_tif_prefix <- '_spp_max_temp_'
    stressor_tif_path_addition <- 'sst_rise_maps'
    stressor_tif_name <- paste(src, sst_tif_prefix, species_name_file, sep = '')
    stressor_tif_path_addition <- 'sst_rise_maps'
  } else {                                    # for all other files, refer to the dictionary
    stressor_tif_name <- stressor_tif_dict[stressor_choice]
    stressor_tif_path_addition <- 'stressor_maps'
  }
  
  stressor_tif_folder <- paste(data_path, '/', stressor_tif_path_addition, sep = '')
  stressor_tif_path <- paste(stressor_tif_folder, '/', stressor_tif_name, '.tif', sep = '')
  
  ##### Format SPECIES range file name for calls
  species_range_file <- paste(src, '_spp_mol_', species_name_file, sep = '')
  species_range_csv_path <- paste(data_path, '/species_ranges/', species_range_file, '.csv', sep = '')
  species_range_df = read_csv(here(species_range_csv_path))
  # csv processing depends on the data source
  if (src == 'iucn') {
    species_which <- 'presence'
  } else {          # for src == 'am'
    species_range_df <- species_range_df %>% 
      filter(prob >= 0.5) %>% 
      drop_na()
    species_which <- 'prob'
  }
  
  ##### Capture species vulernability to the chosen stressor
  species_vuln <- fish_info_map %>% 
    filter(species == species_choice, stressor == stressor_choice) %>% 
    pull(vuln)
  
  ##### Generate rasters for stressor and species range
  # Also change the CRS
  crs_proj <- 'epsg:4326'
  stressor_rast <- rast(here(stressor_tif_path)) %>% 
  terra::project(crs_proj)
  # call helper function to make raster from csv
  species_rast <- map_to_mol(species_range_df,
                             by = 'cell_id',
                             which = species_which,
                             ocean_mask = TRUE) %>% 
  terra::project(crs_proj)  
  
  # Make separate rasters that are mutually exclusive; for species stress and stressor
  stressor_intersect <- terra::mask(stressor_rast, species_rast)      # crop the stressor map to where the species range is
  product_rast <- stressor_intersect * species_rast                   # calculate species stress
  inverse_product_rast <- terra::mask(stressor_rast, product_rast, inverse=TRUE)
  
  ######### Actually make the plot ##########
  species_stress_df <- as.data.frame(x = product_rast, xy = TRUE) %>%
    rename_with(.cols = 3, ~ 'species_stress')
  stressor_df <- as.data.frame(x = inverse_product_rast, xy = TRUE) %>%
    rename_with(.cols = 3, ~ 'stress')
  species_stress_map <- ggplot() +
    geom_tile(data = species_stress_df, aes(x = x, y = y, fill = species_stress)) +
    scale_fill_gradient(low = 'white', high = 'red4') +
    new_scale_fill() +
    geom_tile(data = stressor_df, aes(x = x, y = y, fill = stress)) +
    scale_fill_gradient(low = 'white', high = 'blue4') +
    theme_void()
  
  return(species_stress_map)
}



#######################################################
################## User Interface #####################
#######################################################

ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/4ee2c5c2ed.js"), 
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "fish.css")
  # ),
  theme=shinytheme("slate"),
 # theme = "ocean.css",
  navbarPage("Relative Impacts of Stressors on Commercially Viable Fish",
             tabPanel("Info", fluid=TRUE, icon=icon("fish"),
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
                        
                        mainPanel (textOutput("info"), textOutput("species_info_text"), textOutput("selected_var1"), imageOutput("image"), textOutput("citation"))
                #
                      )
                    ),
             tabPanel("Summary Table", fluid=TRUE, icon = icon("table"),
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
                         mainPanel(textOutput("chart_title"), DTOutput('table'))
                         )
                      ),
             tabPanel("Plotting", fluid=TRUE, icon = icon("chart-column"), 
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
                                                    choices = unique(fish_info_map$stressor)),
                                        selectInput(inputId = "pick_species4",       #need unique inputIds per widget
                                                           label = "Choose Species:",
                                                           choices = unique(fish_info_map$species)),
                                        
                        ),
                        
                        mainPanel (textOutput("OUTPUT"), plotOutput('species_stress_map'))
                        
                        
                      )
                      )
             
  )
)

server <- function(input, output) {
  
##################### info panel ########################
  #reactive fxn for stressor info text
  stressor_info_reactive <- reactive({
    stressor_info %>% 
      filter(stressor %in% input$pick_stressor1) %>% 
      select(exp)
  })

  # #reactive fxn for highest vuln score for a species
  # most_impacted_stressor_reactive <- reactive({
  #   fish_info %>%
  #     filter(species %in% input$pick_species1) %>%
  #     select(stressor,vuln) %>%
  #     arrange(desc(vuln)) %>%
  #     slice(1) %>%
  #     select(stressor) %>%
  #     mutate(stressor = str_replace(stressor, pattern = "_", replacement = " "))
  #     #what to do when there is a tie????????????
  # })

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
  
  ### Seth's attempt
  most_impacted_stressor_reactive <- reactive({
    fish_info %>%
      filter(species %in% input$pick_species1) %>%
      select(stressor,vuln) %>%
      arrange(desc(vuln)) %>% 
      filter(vuln == max(vuln)) %>% 
      select(stressor) %>% 
      mutate(stressor = str_replace(stressor, pattern = "_", replacement = " ")) %>% 
      as.list()
  })
      

  
  #reactive fxn for IUCN status
  iucn_reactive<- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
      select(iucn_status)
  })
  
  iucn_meaning_reactive<-reactive({
    iucn_meaning %>%
      filter(iucn_status %in% iucn_reactive()) %>% 
      select(info)
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
      select(scientific_name_cap) %>% 
      toString() %>% 
      str_trim()
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
    paste("This dataset examines the risk of impact 
          of different environmental stressors on different marine species by intersecting 
          spatial distributions according to each species' vulnerability to a given stressor.
          This analysis specifically explore the vulnerability of high commercial value
          marine fish species to different environmental stressors. You can learn more 
          about the species and how the stressors were defined by changing the inputs 
          in the panel on the left.")
  })

  # #output that creates text with species info
  # #replaced input$pick_species1 with reactive function
  # output$species_info_text<-renderText({
  #   paste(sn_reactive(),", also known as ", cm_reactive(), ", has an IUCN status of ",
  #         iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", sn_reactive(), " is most vulnerable to",
  #         most_impacted_stressor_reactive(), ". This means that if the species was exposed to the same intensity of
  #         all stressors tested, then ", most_impacted_stressor_reactive() , " will have the greatest impact.", sep="")
  # })
  
  ### Seth's attempt for multiple top stressors
  output$species_info_text<-renderText({
    most_impacted_stressor_list <- most_impacted_stressor_reactive()
    if (length(most_impacted_stressor_list$stressor) == 1) {
      paste(sn_reactive(),", also known as ", cm_reactive(), ", has an IUCN status of ",
          iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", sn_reactive(), " is most vulnerable to ",
          most_impacted_stressor_list$stressor[1], ". This means that if the species was exposed to the same intensity of
          all stressors tested, then ", most_impacted_stressor_list$stressor[1], " will have the greatest impact.", sep="")
    }
    if (length(most_impacted_stressor_list$stressor) == 2) {
      paste(sn_reactive(),", also known as ", cm_reactive(), ", has an IUCN status of ",
            iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", sn_reactive(), " is most vulnerable to ",
            most_impacted_stressor_list$stressor[1], " and ", most_impacted_stressor_list$stressor[2], ". This means that if the species was exposed to the same intensity of
          all stressors tested, then ", most_impacted_stressor_list$stressor[1], " and ", most_impacted_stressor_list$stressor[2], " will have the greatest impact.", sep="")
    }
  })
  
  #output that creates text with stressor info
  output$selected_var1<-renderText({
    paste("In this dataset,", stressor_clean_reactive(), " is calculated according to the following:", stressor_info_reactive())
    })
  
  #reactive to produce file path of images
  #this works
  pic_reactive <- reactive({
    iucn_info %>% 
      filter(scientific_name_lower %in% input$pick_species1) %>% 
      select(scientific_name_lower) %>% 
      toString() %>% 
      str_replace_all(pattern = " ", replacement = "_") 
  })
  
  pic_file<-reactive({
    paste("www/", pic_reactive(),".jpg", sep="") %>% 
    as.character()
  })
  
  #output for picture showing
  output$image<- renderImage({
    list(src = pic_file(),
         #src = "www/brevoortia_patronus.jpg", #how I know this should work
         width = "60%",
         height = 350,
         style="display: block; margin-left: auto; margin-right: auto;"
         )
  }, deleteFile = F)
  
  output$citation<-renderText({
    paste("Data collected fom: O’Hara, C., Frazier, M., Valle, M., Butt, N., Kaschner, K., Klein, C.,
          & Halpern, B. *Cumulative human impacts on global marine fauna highlight risk to
          fragile functional diversity of marine ecosystems* [Unpublished manuscript]")
  })

##################### plotting panel ####################### 
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
    ggplot(data = fish_info_reactive(), aes(x = reorder(stressor, -vuln), y=vuln)) +
      geom_col(aes(fill=factor(vuln))) + theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_colour_brewer(palette = "Blues")+
      scale_fill_brewer(palette = "Blues")+
      xlab("Stressor")+
      ylab("Vulnerability Score")+ 
      guides(fill=guide_legend(title="Vulnerability Score"))
  )
  
####################### summary table panel #####################
  #chart title
  output$chart_title<-renderText({
    paste("Stressors with the Greatest Impact on", input$pick_species2)
  })
  
  #data for the table
  table_data <- fish_info %>% 
    select(species, stressor, vuln)
  
  table_data_1<- fish_info %>% 
    select(stressor, vuln)
  
  #reactive function for the table inputs
  table_reactive <- reactive({
    table_data %>% 
      filter(species %in% input$pick_species2) %>% 
      arrange(desc(vuln)) %>% 
      select(stressor, vuln)
  })

  #output that creates the table
  output$table = renderDT({
    datatable(table_reactive()) %>% 
      DT::formatStyle(columns = names(table_data_1), color="lightgray") #column headers, show all rows at once
  }) 

  ######################### map panel #########################
  map_stress_reactive <- reactive({
    input$pick_stressor4
  })
  map_species_reactive <- reactive({
    input$pick_species4
  })
  
  ####### Process data for map #######
  
  output$species_stress_map = renderPlot({
    
    map_stress_range(species_name = map_species_reactive(), stressor_name = map_stress_reactive())
  
  })
  
  
  
  
  
  ### Extract stressor and species specific rasters
  
      # make map of stressor that excludes species stress
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)