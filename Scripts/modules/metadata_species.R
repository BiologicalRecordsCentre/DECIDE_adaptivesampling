
# get the names of species which have been recorded in an area

## get list of species that have been recorded at a location
metadata_species <- function(location,
                             records_df, # records to find species in
                             crds_loc = 4326, # coordinates of the location vector
                             crds_df = 27700, # coordinates of the data frame
                             buffer_distance = NULL,
                             rounding = 4, # rounding to match the transformed coordinates to the data frame. 4 is 100m resolution
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
                                   lat <= buff_reg[4])[, name_col] # could go inside subset...
    
    return(unique(cropped_records_df))
    
  } else if(is.null(buffer_distance)) {
    
    # function to floor the coordinates rather than round
    signif.floor <- function(x, n){
      pow <- floor( log10( abs(x) ) ) + 1 - n
      y <- floor(x / 10 ^ pow) * 10^pow
      # handle the x = 0 case
      y[x==0] <- 0
      y
    }
    
    # get the coordinates of the point
    loc <- data.frame(signif.floor(st_coordinates(trans_loc), rounding))
    
    # get the matching species
    species_present <- subset(records_df, lon == loc$X & lat == loc$Y)
    species_present <- unique(species_present[, name_col])
    
    return(species_present)
    
  }
  
}