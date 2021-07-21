rm(list = ls())

### creating static nudge layer with associated metadata
source('Scripts/modules/filter_distance.R')
library(data.table)
library(tidyverse)

# modal landcover
lcm <- raster(list.files(pattern = 'lcm', 'Data/environmental_data/', full.names = T))
lcm

# data frame to match the land cover map to the lcm names
lcm_names <- data.frame(index = 1:21,
                        lcm_value = c("broad_wood","conif_wood","arable","impr_grass","neutr_grass","calc_grass",
                                      "acid_grass","fen_marsh_swamp","heather","heath_grass","bog","inland_rock",
                                      "saltwater","freshwater","sup_lit_rock","sup_lit_sed","lit_rock","lit_sed",
                                      "saltmarsh","urban","suburban"))

## function to get the names of the habitat corresponding to the modal raster layer
find_lcm_features <- function(rast_obj, # raster of land cover classes or data frame with associated factor levels
                              name_df, # data frame with names corresponding to the 
                              location = c(-2.784492, 54.024851), # location of the cell of interest
                              crds_loc = 4326, # coords of the location
                              crds_obj = 27700, # coords of the raster 
                              buffer_distance = NULL){ # return land cover within a buffer distance from the cell of interest
  
  # first need to convert long lat to BNG
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = crds_loc) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = crds_obj) # transform to BNG
  if(!is.null(buffer_distance)) trans_loc <- st_buffer(trans_loc, buffer_distance) # create a buffer around the point if desired
  
  lcm_num <- raster::extract(x=rast_obj, y=trans_loc, method = 'simple') # extract the 
  
  return(name_df$lcm_value[name_df$index %in% lcm_num[[1]]])
  
}


find_lcm_features(rast_obj = lcm, # raster of land cover classes or data frame with associated factor levels
                  name_df = lcm_names, # data frame with names corresponding to the 
                  location = c(-2.784492, 54.024851),
                  crds_loc = 4326,
                  crds_obj = 27700,
                  buffer_distance = 200)


## get list of species that have been recorded at a location
find_species <- function(location,
                         records_df, # records to find species in
                         crds_loc = 4326, # coordinates of the location vector
                         crds_df = 27700, # coordinates of the data frame
                         buffer_distance = NULL,
                         rounding = -1, # rounding to match the transformed coordinates to the data frame. -1 is 100m resolution
                         name_col){ # the name of the column in records_df that you want returned
  
  # convert to data.frame
  records_df <- data.frame(records_df)
  
  # convert location to spatial and same coordinates as data
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = crds_loc) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = crds_df) # transform to BNG
  
  if(!is.null(buffer_distance)) {
    
    # create the buffer and get the extent
    buff_reg <- extent(st_buffer(trans_loc, buffer_distance)) # create a buffer around the point if desired
    
    # use the extent to crop the main data frame, return the species list
    cropped_records_df <- subset(records_df, lon >= buff_reg[1] &
                                   lon <=  buff_reg[2] &
                                   lat >= buff_reg[3] &
                                   lat <= buff_reg[4])[, name_col]
    
    return(unique(cropped_records_df))
    
  } else if(is.null(buffer_distance)) {
    
    # to get the name right
    colnames(trans_loc) <- 'geometry'
    st_geometry(trans_loc) <- 'geometry'
    
    # get lon and lat of the location separately
    loc <- trans_loc %>% 
      mutate(lon = round(unlist(map(trans_loc$geometry, 1)), rounding),
             lat = round(unlist(map(trans_loc$geometry, 2)), rounding))  %>% 
      as.data.frame() %>% 
      mutate(geometry = NULL)
    
    
    species_present <- (records_df[paste0(records_df$lon, records_df$lat) %in% paste0(loc$lon, loc$lat),])
    species_present <- unique(species_present[, name_col])
    
    
    return(species_present)
    
  }
  
}


# list of species recorded in the cell
taxa = c('butterfly', 'moth')

recs_out <- list()

for(i in taxa){
  
  if(i == 'moth') {
    dfm_full <- fread("Data/species_data/moth/DayFlyingMoths_EastNorths_no_duplicates.csv")
  } else if(i == 'butterfly'){
    dfm_full <- fread("Data/species_data/butterfly/butterfly_EastNorths_no_duplicates.csv")
  }
  
  
  find_species(location = c(-1.097367, 51.604963),
               records_df = dfm_full,
               crds_loc = 4326,
               crds_df = 27700,
               buffer_distance = NULL,
               rounding = -1,
               name_col = 'sp_n')
  
  
  # number of records within the same moving window as the smoothing
  source('Scripts/modules/smooth_recording.R')
  source('Scripts/modules/count_records.R')
  
  
  cr <- count_records(records_df = dfm_full,
                      template_raster = lcm,
                      weight_by_time = FALSE)
  plot(cr)
  
  sm_matrix = matrix(c(0,    1, 1, 1,    0,
                       1, 1, 1, 1, 1,
                       1, 1,    1, 1, 1,
                       1, 1, 1, 1, 1,
                       0,    1, 1, 1,    0), 
                     nrow = 5, ncol = 5)
  
  # r <- raster(ncols=36, nrows=18, xmn=0)
  # values(r) <- round(runif(ncell(r), max = 10))
  # unique(r)
  # plot(r)
  
  smoothed_effort <- focal(x = cr, 
                           w = sm_matrix,
                           fun = sum,
                           pad = TRUE,
                           padValue = NA,
                           na.rm = T,
                           NAonly = F)
  
  plot(smoothed_effort)
  
  recs_out[[i]] <- smoothed_effort
  names(recs_out[[i]]) <- i
  
  # writeRaster(smoothed_effort, filename = paste0('Data/metadata/',i,'_summed_records.grd'))
  
}

# stack taxa counts and lcm
metadata_so_far <- raster::stack(lcm, raster::stack(recs_out))
plot(metadata_so_far[[1]])
any(is.na(values(metadata_so_far[[1]])))


# species-level uncertainty

taxa = 'moth'
pseudoabs = 'PA_thinned_10000nAbs'


model_locs <- paste0('/data-s3/thoval/sdm_outputs/', taxa, '/combined_model_outputs/', pseudoabs)

names <- gsub(pattern = '_PA_thinned_10000nAbs_weightedmeanensemble.grd', replacement = '', 
              list.files(model_locs, 
                         pattern='_weightedmeanensemble.grd'))

# sdm outputs for each species
species_stack <- list()

# error outputs
error_out <- list()

for(i in 1:length(names)){
  
  print(names[i])
  
  # initiate model list within for loop so that it gets replaced when starting a new species
  # otherwise we might get some weird overlaps
  
  # mean predictions
  mp <- list.files(model_locs, 
                   pattern = paste0(names[i], "_", pseudoabs, "_weightedmeanensemble.grd"),
                   full.names = TRUE)
  
  mod_preds <- raster::stack(mp)
  names(mod_preds) <- paste0(names[i], '_mean_pred')
  
  # quantile range
  qr <- list.files(model_locs, 
                   pattern = paste0(names[i], "_", pseudoabs, "_rangeensemblequantiles.grd"),
                   full.names = TRUE)
  
  qrnge <- raster::stack(qr)
  names(qrnge) <- paste0(names[i], '_quantile_range')
  
  species_stack[[i]] <- raster::stack(mod_preds, qrnge)
  
}


# species richness
preds <- stack(lapply(species_stack, FUN = function(x) subset(x, grep(pattern = 'mean_pred',
                                                                      names(x)))))

## this doesn't work - R crashes. Too big?
pquant <- lapply(preds, FUN = function(x) tdigest::tquantile(tdigest::tdigest(na.omit(values(x))), probs = 0.8))


