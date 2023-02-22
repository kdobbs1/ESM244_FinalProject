### These require tidyverse and terra packages to be loaded.

get_mol_rast <- function() {
  ### This function creates a cell ID raster, based on the ocean area raster.
  ### For the species maps, the cell_id column corresponds to the values in
  ### this raster.
  rast_base <- terra::rast(here::here('spatial/ocean_area_mol.tif')) %>%
    terra::setValues(1:terra::ncell(.))
  return(rast_base)
}

map_to_mol <- function(df, by = 'cell_id', which, xfm = NULL, ocean_mask = TRUE) {
  ### This function takes a dataframe of cell_id and some data you wish to map
  ### (e.g., a species range), and generates a raster.
  
  ### Error checking:
  if(!by %in% names(df)) stop('Dataframe needs a valid column for "by" (e.g., cell_id)!')
  if(!which %in% names(df)) stop('Dataframe needs a valid column for "which" (e.g., n_spp)!')
  if(any(is.na(df[[by]]))) stop('Dataframe contains NA values for "by"!')
  if(length(df[[by]]) != length(unique(df[[by]]))) stop('Dataframe contains duplicate observations of ', by, '!')
  
  ### Instead of raster::subs (which is pretty slow), just make a 
  ### vector of all the new values by joining the dataframe to a
  ### new dataframe with complete cell_id column, then replace all 
  ### raster values at once, very fast!
  out_rast <- get_mol_rast()
  df1 <- data.frame(cell_id = 1:ncell(out_rast)) %>%
    dt_join(df, by = 'cell_id', type = 'left')
  values(out_rast) <- df1[[which]]
  
  ### this keeps the layer name from the base rast... swap with "which"
  names(out_rast) <- which
  
  if(!is.null(xfm)) {
    if(class(xfm) != 'function') stop('xfm argument must be a function!')
    out_rast <- xfm(out_rast)
  }
  
  if(ocean_mask) {
    out_rast <- out_rast %>%
      terra::mask(terra::rast(here::here('spatial/ocean_area_mol.tif')))
  }
  return(out_rast)
}


map_to_df  <- function(r, cname = NULL) {
  ### This function takes a raster and converts it to a dataframe of cell_id and
  ### raster values.  Use the cname argument to assign a specific column name, to
  ### override the default.  If you pass a multi-layer SpatRaster and use the cname
  ### argument, you should give it a vector of names, one for each layer
  
  ### load base raster
  base_r <- get_mol_rast()
  
  ### Error checking:
  if(!'SpatRaster' %in% class(r)) stop('Input must be a raster!')
  if(crs(r) != crs(base_r)) stop('Coordinate ref system for raster does not match base!')
  if(!is.null(cname) & length(cname) != nlyr(r)) {
    stop('You have provided a multi-layer SpatRaster but the wrong number of column 
         names.  cname argument should have one name for each layer!')
  }

  ### Create dataframe
  if(is.null(cname)) cname <- names(r)
  df <- data.frame(cell_id = 1:ncell(r),
                   values(r)) %>%
    setNames(c('cell_id', cname))
  

  return(df)
}

dt_join <- function (df1, df2, by, type, allow.cartesian = FALSE)  {
  ### this is a fast join using data.table package but with the syntax of
  ### dplyr joins e.g., left_join is dt_join() with type = 'left'
  a <- case_when(type == "left" ~ c(FALSE, TRUE, FALSE), type == 
                   "full" ~ c(TRUE, TRUE, TRUE), type == "inner" ~ c(FALSE, 
                                                                     FALSE, FALSE))
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2, all = a[1], all.x = a[2], all.y = a[3], 
                   allow.cartesian = allow.cartesian)
  return(as.data.frame(dt_full))
}

library(tidyverse)
library(terra)
library(here)

cell_id_rast <- get_mol_rast()
plot(cell_id_rast)

ocean_a_rast <- terra::rast(here('spatial/ocean_area_mol.tif'))
plot(ocean_a_rast)

### load a couple of species maps as a csv of cell IDs - sourced from AquaMaps or IUCN
spp_maps <- list.files(here('species_ranges'), full.names = TRUE)

am_spp_maps <- spp_maps[str_detect(basename(spp_maps), '^am_')]
  ### ^^^ this looks for filenames that start with "am_" to indicate AquaMaps

ex_am_spp_map <- read_csv(am_spp_maps[1]) %>%
  mutate(presence = ifelse(prob > 0.5, 1, NA))
### note "prob" column - you might create a "presence" column that indicates
### presence if prob > 0.5 (or some other threshold?) and absence otherwise.

iucn_spp_maps <- spp_maps[str_detect(basename(spp_maps), '^iucn_')]
### ^^^ this looks for filenames that start with "iucn_" to indicate IUCN maps
ex_iucn_spp_map <- read_csv(iucn_spp_maps[2]) %>%
  filter(presence != 5) %>%
  mutate(presence = 1)
### note "presence" column - these are IUCN codes for different time scales of
### presence.  You should probably drop presence == 5 (extinct) and then
### convert any remaining presence values to 1.


### turn the ocean area raster into a dataframe
ocean_a_df <- map_to_df(ocean_a_rast)

### map an AquaMaps species to the ocean area dataframe
am_spp_map_df <- ocean_a_df %>%
  left_join(ex_am_spp_map, by = 'cell_id')

### convert species dataframe to a raster
am_spp_map_r <- map_to_mol(am_spp_map_df, which = 'presence')
plot(am_spp_map_r, col = 'red')

### map an IUCN species to the ocean area dataframe
iucn_spp_map_df <- ocean_a_df %>%
  left_join(ex_iucn_spp_map, by = 'cell_id')

### convert to a raster
iucn_spp_map_r <- map_to_mol(iucn_spp_map_df, which = 'presence')
plot(iucn_spp_map_r, col = 'blue')

### create a dataframe of multiple stressors and species presence
str_rast_fs <- list.files(here('stressor_maps'), pattern = '.tif', full.names = TRUE)
str_rasts <- rast(str_rast_fs[1:4])
str_df <- map_to_df(str_rasts) %>%
  left_join(ex_iucn_spp_map, by = 'cell_id')
