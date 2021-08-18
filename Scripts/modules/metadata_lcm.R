
# get the values of the land cover map
# at a point or in a buffered region
# # modal landcover
# lcm <- raster(list.files(pattern = 'lcm', 'Data/metadata/', full.names = T))
# lcm



# data frame to match the land cover map to the lcm names
lcm_names <- data.frame(index = 1:21,
                        lcm_value = c("broad_wood","conif_wood","arable","impr_grass","neutr_grass","calc_grass",
                                      "acid_grass","fen_marsh_swamp","heather","heath_grass","bog","inland_rock",
                                      "saltwater","freshwater","sup_lit_rock","sup_lit_sed","lit_rock","lit_sed",
                                      "saltmarsh","urban","suburban"))

## function to get the names of the habitat corresponding to the modal raster layer
find_lcm_features <- function(rast_obj, # raster of land cover classes or data frame with associated factor levels
                              name_df = lcm_names, # data frame with names corresponding to the 
                              location, # location of the cell of interest
                              crds_loc = 4326, # coords of the location
                              crds_obj = 27700, # coords of the raster 
                              buffer_distance = 10000, # return land cover within a buffer distance from the cell of interest
                              rounding = 4){ # 4 for eastings northings (6 digits, 4th digit is the 100s scale)
  
  # first need to convert long lat to BNG
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = crds_loc) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = crds_obj) # transform to BNG
  if(!is.null(buffer_distance)) trans_loc <- rbind(st_coordinates(trans_loc), st_coordinates(st_buffer(trans_loc, buffer_distance))[,1:2]) # create a buffer around the point if desired
  
  # function to floor the coordinates rather than round
  signif.floor <- function(x, n){
    pow <- floor( log10( abs(x) ) ) + 1 - n
    y <- floor(x / 10 ^ pow) * 10^pow
    # handle the x = 0 case
    y[x==0] <- 0
    y
  }
  
  lcm_num <- raster::extract(x=rast_obj, y=unique(signif.floor(trans_loc, rounding)), method = 'simple') # extract the lcm values
  
  return(name_df$lcm_value[name_df$index %in% lcm_num])
  
}
