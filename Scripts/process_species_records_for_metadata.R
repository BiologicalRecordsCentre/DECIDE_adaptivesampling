

### Processing species records dataframes for faster loading in-app
library(data.table)
library(tidyverse)

# moths
dfm_full <- fread("Data/species_data/moth/DayFlyingMoths_EastNorths_no_duplicates.csv") %>% 
  mutate(thinned_id = paste(sp_n, com_n, lon, lat)) %>% 
  dplyr::select(sp_n, com_n, lon, lat, thinned_id) # select the important columns

# thin to only one record of a species in a cell
dfm_full <- dfm_full[!duplicated(dfm_full$thinned_id),] 
head(dfm_full)


# get the dataframe as one row per unique location
# with a list of species as a comma-separated list
mf <- dfm_full %>% 
  group_by(lon, lat) %>% 
  summarise(com_name = paste0(unique(com_n), collapse = ', '),
            latin_name = paste0(unique(sp_n), collapse = ', '))

# # save
# saveRDS(mf, file = 'Data/metadata/moth_records_by_100m.rds')

# testing
system.time(mf2 <- readRDS('Data/species_data/moth/moth_records_by_100m.rds'))

# get the list of unique species
mth_list <- dfm_full %>% 
  dplyr::select(com_n, sp_n) %>% 
  distinct() %>% 
  arrange(sp_n) %>% 
  mutate(taxa = 'Day flying moth')
mth_list

# butterfly
but_full <- fread("Data/species_data/butterfly/butterfly_EastNorths_no_duplicates.csv") %>% 
  mutate(thinned_id = paste(sp_n, com_n, lon, lat)) %>% 
  dplyr::select(sp_n, com_n, lon, lat, thinned_id)

# thin to only one record of a species in a cell
but_full <- but_full[!duplicated(but_full$thinned_id),] 
head(but_full)


# get the dataframe as one row per unique location
# with a list of species as a comma-separated list
bf <- but_full %>% 
  group_by(lon, lat) %>% 
  summarise(com_name = paste0(unique(com_n), collapse = ', '),
            latin_name = paste0(unique(sp_n), collapse = ', '))

# # save
# saveRDS(bf, file = 'Data/metadata/butterfly_records_by_100m.rds')

# testing
system.time(bf2 <- readRDS('Data/species_data/butterfly/butterfly_records_by_100m.rds'))


# get the list of unique species
but_list <- but_full %>% 
  dplyr::select(com_n, sp_n) %>% 
  distinct() %>% 
  arrange(sp_n) %>% 
  mutate(taxa = 'Butterfly')
but_list

spp_list <- rbind(mth_list, but_list)
write.csv(spp_list, file= 'Data/taxonomy/List_of_butterfly_and_moth_species.csv')
