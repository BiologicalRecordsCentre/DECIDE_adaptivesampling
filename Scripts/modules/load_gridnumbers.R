
#  Find the grid numbers of interest to load
# 
# _Input_: Location; Distance; a grid object
# 
# _Output_: Accessible area layer with corresponding error metric
# 
# _Metadata_:
# 


load_gridnumbers <- function(location, # location in form long, lat
                          distance, # distance in metres
                          grid) { # must be an sf_object - can be different grid sizes
  
  
  # create buffer
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = 4326) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = 27700) # transform to BNG
  buffed <- st_buffer(trans_loc, distance) # create a buffer around the point
  
  # find intersection
  grid_num <- st_intersects(buffed, grid)[[1]]
  
  return(grid_num)
  
}