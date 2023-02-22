library(tidyverse)
library(here)
library(oharac)

source('common_fxns.R')

spp <- assemble_spp_info_df()

fish_spp <- read_csv(here('fish_for_esm244', 'ESM244FishSpecies.csv')) %>%
  select(comname = 1, sciname = 2) %>%
  mutate(sciname = tolower(sciname))

### Gather info on fish species
fish_df <- spp %>%
  filter(species %in% fish_spp$sciname) %>%
  select(species, stressor, vuln = v_score, iucn_sid, src, id, map_f)

### Copy coral spp maps to local folder
spp_map_orig <- fish_df$map_f %>% unique()
spp_map_new  <- here('fish_for_esm244/species_ranges', basename(spp_map_orig))

spp_map_new_missing <- spp_map_new[!file.exists(spp_map_new)]
spp_map_orig_missing <- spp_map_orig[!file.exists(spp_map_new)]
if(length(spp_map_new_missing) > 1) {
  file.copy(from = spp_map_orig_missing, to = spp_map_new_missing)
}


### Gather stressor info
str_fs <- list.files(here('_data/stressors_mol'), full.names = TRUE)
str_fs_new <- here('fish_for_esm244/stressor_maps', basename(str_fs))
file.copy(from = str_fs, to = str_fs_new)

### gather SST rise stressor maps
sst_dir <- '/home/shares/ohi/spp_vuln/spp_vuln_mapping/stressors/max_temp'
sst_base_f <- sprintf('%s_spp_max_temp_%s.csv', fish_df$src, fish_df$id) %>% unique()
sst_fs_orig <- file.path(sst_dir, sst_base_f)
sst_fs_new  <- here('fish_for_esm244/sst_rise_maps', sst_base_f)

sst_new_missing <- sst_fs_new[!file.exists(sst_fs_new)]
sst_orig_missing <- sst_fs_orig[!file.exists(sst_fs_new)]
if(length(sst_new_missing) > 1) {
  file.copy(from = sst_orig_missing, to = sst_new_missing)
}

### gather fishing stressor maps
fishing_dir <- '/home/shares/ohi/spp_vuln/spp_vuln_mapping/stressors/fishing/4_rescaled_catch_by_spp_cell'
fishing_base_f <- sprintf('%s_spp_rescaled_catch_%s.csv', 
                          fish_df$src, 
                          str_replace_all(fish_df$species, ' +', '_')) %>% 
  unique()
fishing_fs_orig <- file.path(fishing_dir, fishing_base_f)
fishing_fs_orig <- fishing_fs_orig[file.exists(fishing_fs_orig)]
fishing_fs_new  <- here('fish_for_esm244/fishing_stressor_maps', fishing_base_f)

fishing_new_missing <- fishing_fs_new[!file.exists(fishing_fs_new)]
fishing_orig_missing <- fishing_fs_orig[!file.exists(fishing_fs_new)]
if(length(fishing_new_missing) > 1) {
  file.copy(from = fishing_orig_missing, to = fishing_new_missing)
}

### Clean up dataframe and write out
fish_df_new <- fish_df %>%
  mutate(map_f = file.path('species_ranges', basename(map_f)),
         vuln = round(vuln, 5)) %>%
  group_by(stressor) %>%
  filter(sum(vuln) > 0) %>% ### drop any stressors with no vulnerability
  ungroup() %>%
  select(-id)

write_csv(fish_df_new, here('fish_for_esm244/fish_info.csv'))

### gather spatial info
spat_info <- c('ocean_area_mol.tif', 'eez_mol.tif', 
               'rgn_names_ohi.csv', 'meow_rgns_mol.tif') %>%
  paste(collapse = '|')
spatial_fs <- list.files(here('_spatial'), 
                         full.names = TRUE, recursive = TRUE,
                         pattern = spat_info)
file.copy(spatial_fs, here('fish_for_esm244/spatial', basename(spatial_fs)))
meow_info <- foreign::read.dbf(here('_spatial/meow_rgns/meow_rgns.dbf')) %>%
  janitor::clean_names()

write_csv(meow_info, here('fish_for_esm244/spatial/meow_rgns.csv'))
