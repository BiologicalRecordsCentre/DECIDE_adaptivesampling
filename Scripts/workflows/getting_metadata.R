rm(list = ls())

### creating static nudge layer with associated metadata
source('Scripts/modules/filter_distance.R')

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



# list of species recorded in the cell
dfm_full <- fread("Data/species_data/moth/DayFlyingMoths_EastNorths_no_duplicates.csv")
dfm_full <- fread("Data/species_data/butterfly/butterfly_EastNorths_no_duplicates.csv")

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
      mutate(lon = round(unlist(map(t$geometry, 1)), rounding),
             lat = round(unlist(map(t$geometry, 2)), rounding))  %>% 
      as.data.frame() %>% 
      mutate(geometry = NULL)
    
    subset(records_df, lon == loc$lon &
             lat == loc$lat)
    
    species_present <- (records_df[paste0(records_df$lon, records_df$lat) %in% paste0(loc$lon, loc$lat),])
    species_present <- unique(species_present[, name_col])
    
    
    return(species_present)
    
  }
  
}

find_species(location = c(-1.097367, 51.604963),
             records_df = dfm_full,
             crds_loc = 4326,
             crds_df = 27700,
             buffer_distance = NULL,
             rounding = -1,
             name_col = 'sp_n')

# number of records within the same moving window as the smoothing


# species-level uncertainty


# species richness


