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
library(dplyr)

# for mapping
library(terra)
library(sf)
library(mapproj)
library(maptools)
library(ggnewscale)
library(rnaturalearth)



################# Setting paths

data_path <- 'fish_data_app/data'

source(here(data_path, 'helper_fxns.R'))


################# Front End Data Processing

##### fish info

fish_info<-read_csv(here("fish_data_app/data", "fish_info.csv")) %>% 
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
         stressor!="wildlife_strike") %>% 
  mutate(stressor = str_replace_all(stressor, pattern = "_", replacement = " "))
region_info<-read_csv(here("fish_data_app/data/spatial", "meow_rgns.csv"))
iucn_info<-read_csv(here("fish_data_app/data", "IUCN_data.csv")) %>% 
  janitor::clean_names()
stressor_info<-read_csv(here("fish_data_app/data", "stressor_info.csv")) %>% 
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
         stressor!="wildlife_strike")  %>% 
  mutate(stressor = str_replace_all(stressor, pattern = "_", replacement = " "))
iucn_meaning<-read_csv(here("fish_data_app/data", "iucn_meaning.csv"))
#merge fish info with iucn info in order to connect common name to scientific name
fish_info<-left_join(fish_info, iucn_info, by=c('species'='scientific_name_lower'))


##### regions table #####
bycatch_df <- rast(here(data_path, 'stressor_maps', 'bycatch_benthic_2017.tif')) %>%
  map_to_df()
eu_df <- rast(here(data_path, 'stressor_maps', 'nutrient_2020.tif')) %>%
  map_to_df()
lp_df <- rast(here(data_path, 'stressor_maps', 'light_2018.tif')) %>%
  map_to_df()
mhw_df <- rast(here(data_path, 'stressor_maps', 'sst_extremes_2020.tif')) %>%
  map_to_df()
oceana_df <- rast(here(data_path, 'stressor_maps', 'ocean_acidification_2020.tif')) %>%
  map_to_df()
plasticp_df <- rast(here(data_path, 'stressor_maps', 'microplastics_2015.tif')) %>%
  map_to_df()
# sst_df <- rast(here(data_path, 'sst_rise_maps', 'spp_max_temp.tif')) %>%
#   map_to_df()
uv_df <- rast(here(data_path, 'stressor_maps', 'uv_radiation_2020.tif')) %>%
  map_to_df()
regions_df <- rast(here(data_path, 'spatial', 'meow_rgns_mol.tif')) %>%
  map_to_df()
meow<-read_csv(here(data_path, 'spatial', 'meow_rgns.csv')) %>% as.data.frame() %>% dplyr::select(rlm_code, realm, eco_code_x)
#merge dfs
df_list <- list(bycatch_df, eu_df, lp_df, mhw_df, oceana_df, plasticp_df, uv_df, regions_df)
merge<-df_list %>% reduce(full_join, by='cell_id') %>% drop_na()
merge2<-dplyr::full_join(merge, meow, by = c("meow_rgns_mol"="eco_code_x")) %>%
  group_by(realm) %>%
  summarize(across(.cols=bycatch_benthic_2017:uv_radiation_2020, .fns=mean))



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

################ regions ##################
# items are region name, globe rotate T/F, longitude lower, longitude upper, latitude lower, latitude upper
world <- list(name = 'World', rotate = F, lon_low = -180, lon_up = 180, lat_low = -89.94, lat_up = 90)
pacific_nw <- list(name = 'Northwest Pacific', rotate = T, lon_low = -80, lon_up = 10, lat_low = 0, lat_up = 65)
pacific_ne <- list(name = 'Northeast Pacific', rotate = T, lon_low = -10, lon_up = 80, lat_low = 0, lat_up = 65)
pacific_sw <- list(name = 'Southwest Pacific', rotate = T, lon_low = -80, lon_up = 10, lat_low = -65, lat_up = 0)
pacific_se <- list(name = 'Southeast Pacific', rotate = T, lon_low = -10, lon_up = 80, lat_low = -65, lat_up = 0)
indian <- list(name = 'Indian', rotate = F, lon_low = 10, lon_up = 120, lat_low = -70, lat_up = 0)
atlantic_n <- list(name = 'North Atlantic', rotate = F, lon_low = -40, lon_up = 20, lat_low = 0, lat_up = 65)
atlantic_s <- list(name = 'South Atlantic', rotate = F, lon_low = -30, lon_up = 40, lat_low = -65, lat_up = 0)

regions_list <- list(world = world,
                     pacific_nw = pacific_nw, 
                     pacific_ne = pacific_ne, 
                     pacific_sw = pacific_sw,
                     pacific_se = pacific_se,
                     indian = indian,
                     atlantic_n = atlantic_n,
                     atlantic_s = atlantic_s
)

################ for base layer ##############
land_df <- as.data.frame(x = land_rast, xy = TRUE) %>%
  rename_with(.cols = 3, ~ 'land')
land_rotated_df <- as.data.frame(x = land_rast_rotated, xy = TRUE) %>%
  rename_with(.cols = 3, ~ 'land')

land_sf_plot <- ggplot() +
  geom_tile(data = land_df, aes(x = x, y = y, fill = land, )) + 
  scale_fill_gradientn(breaks = c(0,1),
                       colors = c('seashell1', 'seashell1'),
                       guide = 'none') +
  new_scale_fill()

land_sf_plot_rotated <- ggplot() +
  geom_tile(data = land_rotated_df, aes(x = x, y = y, fill = land)) + 
  scale_fill_gradientn(breaks = c(0,1),
                       colors = c('seashell1', 'seashell1'),
                       guide = 'none') +
  new_scale_fill()


####################################################################
##################### Define mapping function ######################
####################################################################

map_stress_range <- function(species_name, stressor_name, region_name) {
  
  stressor_choice <- c(stressor_name)
  species_choice <- species_name
  region_choice <- region_name
  
  # initialize and set region variables
  extracted_region <- regions_list[[region_choice]]
  rotate_globe <- extracted_region$rotate
  long_lower <- extracted_region$lon_low
  long_upper <- extracted_region$lon_up
  lat_lower <- extracted_region$lat_low
  lat_upper <- extracted_region$lat_up
  
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
  product_rast <- stressor_intersect * species_rast              # calculate species stress
  inverse_product_rast <- terra::mask(stressor_rast, product_rast, inverse=TRUE)
  
  ##### Rotate raster data if necessary for selected region
  if (rotate_globe == T) {
    product_rast <- product_rast %>% terra::rotate()
    ext(product_rast) <- c(-180, 180, -89.9401853248781, 90)
    inverse_product_rast <- inverse_product_rast %>% terra::rotate()
    ext(inverse_product_rast) <- c(-180, 180, -89.9401853248781, 90)
  }
  
  ##### make dataframes out of the rasters
  species_stress_df <- as.data.frame(x = product_rast, xy = TRUE) %>%
    rename_with(.cols = 3, ~ 'species_stress')
  stressor_df <- as.data.frame(x = inverse_product_rast, xy = TRUE) %>%
    rename_with(.cols = 3, ~ 'stress')
  
  # ggplot() + geom_tile(data = stressor_df, aes(x = x, y = y, fill = stress))
  
  ######### Actually make the plot ##########
  # two cases for whether the inverted globe is needed
  if (rotate_globe == T) {
    species_stress_map <- land_sf_plot_rotated +
      geom_tile(data = species_stress_df, aes(x = x, y = y, fill = species_stress)) +
      scale_fill_gradient(low = 'white', high = 'red4') +
      new_scale_fill() +
      geom_tile(data = stressor_df, aes(x = x, y = y, fill = stress)) +
      scale_fill_gradient(low = 'white', high = 'deepskyblue4') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      theme_minimal()
  } else {
    species_stress_map <- land_sf_plot +
      geom_tile(data = species_stress_df, aes(x = x, y = y, fill = species_stress)) +
      scale_fill_gradient(low = 'white', high = 'red4') +
      new_scale_fill() +
      geom_tile(data = stressor_df, aes(x = x, y = y, fill = stress)) +
      scale_fill_gradient(low = 'white', high = 'deepskyblue4') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      theme_void()
  }
  
  species_stress_map_final <- species_stress_map +
    coord_sf(xlim = c(long_lower, long_upper), ylim = c(lat_lower, lat_upper), expand = F)
  
  return(species_stress_map_final)
}



#######################################################
################## User Interface #####################
#######################################################

ui <- fluidPage(
  tags$script(src = "https://kit.fontawesome.com/4ee2c5c2ed.js"), 
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "fish.css")
  # ),
  theme=shinytheme("superhero"),
  navbarPage("Relative Impacts of Stressors on Commercially Viable Fish",
             tabPanel("Info", fluid=TRUE, icon=icon("fish"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          titlePanel(h5("Change these selections to learn about different species and stressors:")),
                          #Select species
                          selectInput(inputId = "pick_species1",
                                                label = "Choose species:",
                                                #choices = unique(fish_info$species), 
                                                choices = c("Gulf menhaden"="brevoortia patronus",
                                                  "Milkfish"="chanos chanos",
                                                  "Atlantic herring"="clupea harengus",
                                                  "Japanese anchovy"="engraulis japonicus",
                                                  "Peruvian anchoveta"="engraulis ringens",
                                                  "Atlantic cod"="gadus morhua",
                                                  "Skipjack tuna"="katsuwonus pelamis",
                                                  "Capelin"="mallotus villosus",
                                                  "Rainbow trout"="oncorhynchus mykiss",
                                                  "Atlantic salmon"="salmo salar",
                                                  "European pilchard"="sardina pilchardus",
                                                  "Indian oil sardine"="sardinella longiceps",
                                                  "Chub mackerel"="scomber japonicus",
                                                  "Atlantic mackerel"="scomber scombrus",
                                                  "Yellowfin tuna"="thunnus albacares",
                                                  "Largehead hairtail"="trichiurus lepturus"),
                                                selected = "oncorhynchus mykiss"),
                          #Select stressor
                          selectInput(inputId = "pick_stressor1",
                                                label = "Choose stressor:",
                                                choices = unique(stressor_info$stressor), 
                                                selected="biomass_removal")
                                        ),
                        
                        mainPanel(h3(strong("Background")),
                                  uiOutput("info"),
                          tabsetPanel(
                            tabPanel(      
                                  h5(strong(uiOutput("fish_subheading"))),
                                  uiOutput("spp_info_text"), #this text doesn't work
                                  imageOutput("image")),
                            tabPanel(
                                  h5(strong(textOutput("stressor_subheading"))),
                                  textOutput("selected_var1")),
                            tabPanel(
                                  h5(strong("Data Sources")),
                                  uiOutput("citation"),
                                  uiOutput("iucn_learn"))
                          )
                        )
                      )
                    ),
             tabPanel("Ranked Stressors", fluid=TRUE, icon = icon("table"),
                      #icon=icon("", lib = "font-awesome"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          #select species
                          radioButtons(inputId = "pick_species2",
                                             label = "Choose species:",
                                             #choices = unique(fish_info$species)),
                                      choices = c("Gulf menhaden"="brevoortia patronus",
                                                   "Milkfish"="chanos chanos",
                                                   "Atlantic herring"="clupea harengus",
                                                   "Japanese anchovy"="engraulis japonicus",
                                                   "Peruvian anchoveta"="engraulis ringens",
                                                   "Atlantic cod"="gadus morhua",
                                                   "Skipjack tuna"="katsuwonus pelamis",
                                                   "Capelin"="mallotus villosus",
                                                   "Rainbow trout"="oncorhynchus mykiss",
                                                   "Atlantic salmon"="salmo salar",
                                                   "European pilchard"="sardina pilchardus",
                                                   "Indian oil sardine"="sardinella longiceps",
                                                   "Chub mackerel"="scomber japonicus",
                                                   "Atlantic mackerel"="scomber scombrus",
                                                   "Yellowfin tuna"="thunnus albacares",
                                                   "Largehead hairtail"="trichiurus lepturus")),
                          #select stressor
                          # checkboxGroupInput(inputId = "pick_stressor2",
                          #                    label = "Choose stressor",
                          #                    choices = unique(fish_info$stressor))#,
                          #select region
                          #checkboxGroupInput(inputId = "pick_region",
                                            # label = "Choose region:",
                                            # choices = unique(region_info$realm))
                         
                           ),
                         mainPanel(uiOutput("chart_title"), DTOutput('table'))
                         )
                      ),
             tabPanel("Vulnerability Chart", fluid=TRUE, icon = icon("chart-column"), 
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput(inputId = "pick_species3",
                                                  label = "Choose species:",
                                                  #choices = unique(fish_info$species), 
                                                  choices = c("Gulf menhaden"="brevoortia patronus",
                                                              "Milkfish"="chanos chanos",
                                                              "Atlantic herring"="clupea harengus",
                                                              "Japanese anchovy"="engraulis japonicus",
                                                              "Peruvian anchoveta"="engraulis ringens",
                                                              "Atlantic cod"="gadus morhua",
                                                              "Skipjack tuna"="katsuwonus pelamis",
                                                              "Capelin"="mallotus villosus",
                                                              "Rainbow trout"="oncorhynchus mykiss",
                                                              "Atlantic salmon"="salmo salar",
                                                              "European pilchard"="sardina pilchardus",
                                                              "Indian oil sardine"="sardinella longiceps",
                                                              "Chub mackerel"="scomber japonicus",
                                                              "Atlantic mackerel"="scomber scombrus",
                                                              "Yellowfin tuna"="thunnus albacares",
                                                              "Largehead hairtail"="trichiurus lepturus"
                                                              ),
                                                  selected = fish_info$species[2]),
                                     checkboxGroupInput(inputId = "pick_stressor3",
                                                        label = "Choose stressor:",
                                                        choices = unique(fish_info$stressor), 
                                                        selected = c(fish_info$stressor[1], fish_info$stressor[5], fish_info$stressor[8]))
                        ),
                        
                        mainPanel(uiOutput("plot_title"), plotOutput('fish_info_plot'))
                        
                        
                      )
                      ),
             tabPanel("Stressor by Realm", fluid=TRUE, icon=icon("location-dot"),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          radioButtons(inputId = "pick_realm",       #need unique inputIds per widget
                                                           label = "Choose Realm:",
                                                           choices = unique(merge2$realm)
                                                 ),

                        ),

                        mainPanel(
                          tabsetPanel(
                            tabPanel(
                                  h5(textOutput("stress_realm_title")), 
                                  DTOutput('realm_table')),
                          tabPanel(h5("Map of Realms"),
                                  imageOutput("realm_pic"),
                                  textOutput('realm_citation')
                                  )))


                      )
                      ),
             tabPanel("Mapping Vulnerability", fluid=TRUE, icon=icon("globe-americas"), 
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput(inputId = "pick_species4",       #need unique inputIds per widget
                                      label = "Choose species:",
                                      #choices = unique(fish_info_map$species)
                                      choices = c("Gulf menhaden"="brevoortia patronus",
                                                  "Milkfish"="chanos chanos",
                                                  "Atlantic herring"="clupea harengus",
                                                  "Japanese anchovy"="engraulis japonicus",
                                                  "Peruvian anchoveta"="engraulis ringens",
                                                  "Atlantic cod"="gadus morhua",
                                                  "Skipjack tuna"="katsuwonus pelamis",
                                                  "Capelin"="mallotus villosus",
                                                  "Rainbow trout"="oncorhynchus mykiss",
                                                  "Atlantic salmon"="salmo salar",
                                                  "European pilchard"="sardina pilchardus",
                                                  "Indian oil sardine"="sardinella longiceps",
                                                  "Chub mackerel"="scomber japonicus",
                                                  "Atlantic mackerel"="scomber scombrus",
                                                  "Yellowfin tuna"="thunnus albacares",
                                                  "Largehead hairtail"="trichiurus lepturus"),
                                      selected = 'Rainbow trout'
                                      ),
                          selectInput(inputId = "pick_stressor4",
                                      label = "Choose stressor:",
                                      #choices = unique(fish_info_map$stressor),
                                      choices = c("bycatch"="bycatch",
                                                  "eutrophication nutrient pollution"="eutrophication_nutrient_pollution",
                                                  "light pollution"="light_pollution",
                                                  "marine heat waves"="marine_heat_waves",
                                                  "ocean acdification"="ocean_acidification",
                                                  "plastic pollution microplastic"="plastic_pollution_microplastic",
                                                  "sst rise"="sst_rise",
                                                  "UV radiation"="uv_radiation"),
                                      selected = 'ocean_acidification'),
                          selectInput(inputId = "pick_region",
                                      label = "Choose region:",
                                      choices = c("World"="world",
                                                  "Northwest Pacific"="pacific_nw",
                                                  "Northeast Pacific"="pacific_ne",
                                                  "Southwest Pacific"="pacific_sw",
                                                  "Southeast Pacific"="pacific_se",
                                                  "Indian"="indian",
                                                  "North Atlantic"="atlantic_n",
                                                  "South Atlantic"="atlantic_s"),
                                      selected = 'World'),
                          
                        ),
                        
                        mainPanel(uiOutput('map_title'), plotOutput('species_stress_map'))
                        
                        
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
  
  cm_plot_reactive_info<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species1) %>% 
      select(common_name) %>% 
      unique()
  })
  
  #upper case scientific name reactive for plot title
  sci_name_reactive_info<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species1) %>% 
      select(scientific_name_cap) %>% 
      unique() %>% 
      toString() 
  })
  
  #output that makes a reactive heading telling us which fish we are learning about
  output$fish_subheading<-renderUI(HTML(paste("Learn more about", cm_plot_reactive_info()," (",
                                          em(sci_name_reactive_info()), ")"), sep=""))
  
  #output that makes a reactive heading telling us more about how stressors are defined
  output$stressor_subheading<-renderText(paste("Defining", input$pick_stressor1), sep="")
  
  #output with basic info about data that doesn't change
  wiki_url <- a("this website", href="https://en.m.wikipedia.org/wiki/List_of_commercially_important_fish_species")
  output$info<-renderUI({
    tagList("This dataset examines the risk of impact 
          of different environmental stressors on different marine species by intersecting 
          spatial distributions according to each species' vulnerability to a given stressor
          from 2015 to 2020. This analysis specifically explores the vulnerability of high commercial value
          marine fish species to different environmental stressors. Fish species
          of high comercial value were defined as marine species that are harvested
          in excess of 500,000 tons annually according to", wiki_url,". You can learn more 
          about the species and how the stressors were defined by changing the inputs 
          in the panel on the left.")
  })
  
  # #output that creates text with species info
  # #replaced input$pick_species1 with reactive function
  # output$spp_info_text<-renderText({
  #   paste(sn_reactive(),", also known as ", cm_reactive(), ", has an IUCN status of ",
  #         iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", sn_reactive(), " is most vulnerable to ",
  #         most_impacted_stressor_reactive(), ". This means that if the species was exposed to the same intensity of
  #         all stressors tested, then ", most_impacted_stressor_reactive() , " will have the greatest impact.", sep="")
  # })
  
  ### Seth's attempt for multiple top stressors
  output$spp_info_text<-renderUI({
    most_impacted_stressor_list <- most_impacted_stressor_reactive()
    if (length(most_impacted_stressor_list$stressor) == 1) {
      HTML(paste(em(sn_reactive()),", also known as ", cm_reactive(), ", has an IUCN status of ",
          iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", 
          em(sn_reactive()), " is most vulnerable to ", most_impacted_stressor_list$stressor[1], 
          ". This means that if the species was exposed to the same intensity of
          all stressors tested, then ", most_impacted_stressor_list$stressor[1], 
          " will have the greatest impact.", sep=""))
    }
    #if (length(most_impacted_stressor_list$stressor) == 2) {
    else {
      HTML(paste(em(sn_reactive()),", also known as ", cm_reactive(), ", has an IUCN status of ",
            iucn_reactive(),". ",iucn_meaning_reactive()," Of the stressors tested, ", 
            em(sn_reactive()), " is most vulnerable to ", most_impacted_stressor_list$stressor[1], 
            " and ", most_impacted_stressor_list$stressor[2], ". This means that if the 
            species was exposed to the same intensity of all stressors tested, then ", 
            most_impacted_stressor_list$stressor[1], " and ", most_impacted_stressor_list$stressor[2], 
            " will have the greatest impact.", sep=""))
    }
  })
  
  #output that creates text with stressor info
  output$selected_var1<-renderText({
    paste("In this dataset,", stressor_clean_reactive(), " is calculated according to the following:", stressor_info_reactive())
    })
  
  #reactive to produce file path of images
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
  
  output$citation<-renderUI(HTML(paste("Data collected fom: O’Hara, C., Frazier, M., Valle, M., Butt, N., Kaschner, K., Klein, C.,
          & Halpern, B.", em("Cumulative human impacts on global marine fauna highlight risk to
          fragile functional diversity of marine ecosystems "), " [Unpublished manuscript]")
  ))
  
  IUCN_url <- a("IUCN Homepage", href="https://www.iucnredlist.org/")
  output$iucn_learn <- renderUI({
    tagList("Learn more about IUCN statuses here:", IUCN_url)
  })

##################### plotting panel ####################### 
  #common name reactive for plot title
  cm_plot_reactive<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species3) %>% 
      select(common_name) %>% 
      unique()
  })
  
  #upper case scientific name reactive for plot title
  sci_name_reactive<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species3) %>% 
      select(scientific_name_cap) %>% 
      unique() %>% 
      toString() 
    })
  
  #output that makes a reactive plot title
  output$plot_title<-renderUI(HTML(paste("Vulnerability of ", cm_plot_reactive()," (",
                                         em(sci_name_reactive()), ") to Stressors"), sep=""))
 
  #reactive fxn for plot
  fish_info_reactive <- reactive({
    fish_info %>%
      filter(species %in% input$pick_species3) %>%
      filter(stressor %in% input$pick_stressor3)
  })

  #output that creates plot
  output$fish_info_plot <- renderPlot(
    ggplot(data = fish_info_reactive(), aes(x = reorder(stressor, -vuln), y=vuln)) +
      geom_col(aes(fill=factor(vuln))) + 
      theme_light()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.background = element_rect(colour = "#6D9EC1",
                                           size = 2, linetype = "solid"), 
            axis.title=element_text(size=14), 
            axis.text=element_text(size=12),
            legend.position = "none")+
      scale_colour_brewer(palette = "Blues")+
      scale_fill_brewer(palette = "Blues")+
      xlab("Stressor")+
      ylab("Vulnerability Score")+
      geom_text(aes(label = vuln), vjust = -0.2)+
      guides(fill=guide_legend(title="Vulnerability Score")),
    height = 550
  )
  
####################### summary table panel #####################
  cm_plot_reactive_table<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species2) %>% 
      select(common_name) %>% 
      unique()
  })
  
  #upper case scientific name reactive for plot title
  sci_name_reactive_table<- reactive({
    fish_info %>% 
      filter(species %in% input$pick_species2) %>% 
      select(scientific_name_cap) %>% 
      unique() %>% 
      toString() 
  })
  
  #output that makes a reactive chart title
  output$chart_title<-renderUI(HTML(paste("Stressors that ", cm_plot_reactive_table()," (",
                                         em(sci_name_reactive_table()), ") is Vulnerable to Ranked"), sep=""))
  
  # #chart title
  # output$chart_title<-renderText({
  #   paste("Stressors with the Greatest Impact on", input$pick_species2)
  # })
  
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
    datatable(table_reactive(), style = "bootstrap") %>% 
      DT::formatStyle(columns = names(table_data_1), color="lightgray") #column headers, show all rows at once
  }) 
  ######################### realm stressor panel #########################

  #output that makes a reactive chart title
  output$stress_realm_title<-renderText(paste("Average Vulnerability Score Per 
                                              Stressor in the ",input$pick_realm, sep=""))
  #dataframe containing correct column names
  column_names<-c("stressor", "vulnerability")
  
  #reactive function for the table inputs
  table_reactive2 <- reactive({
    merge2 %>%
      filter(realm %in% input$pick_realm) %>% 
      select(-realm) %>% 
      #dplyr::rename("bycatch_benthic_2017"="bycatch") %>% 
      pivot_longer(cols=bycatch_benthic_2017:uv_radiation_2020,names_to = "stressor", 
                   values_to = "vulnerability") %>% 
      arrange(desc(vulnerability))%>%                   # Using dplyr functions
      mutate(stressor = recode(stressor, bycatch_benthic_2017 = 'bycatch', 
                             ocean_acidification_2020 = 'ocean acidification', 
                             uv_radiation_2020 = 'UV radiation',
                             sst_extremes_2020 ='marine heat waves',
                             light_2018='light pollution',
                             microplastics_2015='microplastic pollution',
                             nutrient_2020 ='eutrophication')) %>% 
      dplyr::mutate_if(is.numeric,
                       round,
                       digits = 4)
  })
  
  #output for picture showing
  output$realm_pic<- renderImage({
    list( src = "www/realms.png", #how I know this should work
         width = "80%",
         height = 380,
         style="display: block; margin-left: auto; margin-right: auto;"
    )
  }, deleteFile = F)
  
  output$realm_citation<-renderText(paste(
  "This map shows the areas that each realm encompasses and taken from: 
  Spalding, M., Fox, H., Allen, G., Davidson, N., Ferdaña, Z., Finlayson, M., 
  Halpern, B., Jorge, M., Lombana, A., Lourie, S., Martin, K., McManus, E., 
  Molnar, J., Recchia, C., & Robertson, J. (2007). Marine Ecoregions of the World: 
    A Bioregionalization of Coastal and Shelf Areas. BioScience, 57, 573–583. 
  https://doi.org/10.1641/B570707"))

  #output that creates the table
  output$realm_table = renderDT({
    datatable(table_reactive2(), style = "bootstrap") %>%
      DT::formatStyle(columns = names(column_names), color="lightgray") #column headers, show all rows at once
  })
  ######################### map panel #########################
  map_stress_reactive <- reactive({
    input$pick_stressor4
  })
  map_species_reactive <- reactive({
    input$pick_species4
  })
  map_region_reactive <- reactive({
    input$pick_region
  })
  
  output$map_title<-renderUI(HTML(paste("Global stress vulnerability of ", input$pick_stressor4, " to ", em(input$pick_species4), sep = "")))
  
  ####### Process data for map #######
  
  output$species_stress_map = renderPlot({
    
    map_stress_range(species_name = map_species_reactive(), stressor_name = map_stress_reactive(), region_name = map_region_reactive())
  
  })
  
  
  
  
  
  ### Extract stressor and species specific rasters
  
      # make map of stressor that excludes species stress
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)