# **5. Accessible locations**
#   
#  Identify accessible features (footpaths, parks, etc) within the area of interest
# 
# _Input_: Spatial layer of accessible locations [one or all of footpaths, parks, etc]; point location; buffer
# 
# _Output_: Spatial subset of accessible locations with their metadata
# 
# _Metadata_: Buffer size, spatial layer used

# This could be generalised to take any prow, accesspoints or greenspace (or any combination)
# to make this transferable to other countries

# original directory "/data/notebooks/rstudio-adaptivesampling"

# "/data/notebooks/

# # load public rights of way file (only some of england for the moment)
# prow <- st_read('/data/notebooks/rstudio-constraintlayers/Data/raw_data/rowmaps_footpathbridleway/rowmaps_footpathbridleway/ALL_PATHS_MERGED_long.shp')
# prow
# 
# # load greenspaces
# greenspace <- st_read('/data/notebooks/rstudio-constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/GB_GreenspaceSite_BNG.shp')
# greenspace
# 
# # plot(st_geometry(grsp)) # this works but takes ages
# 
# # load access points
# accesspoints <- st_read('/data/notebooks/rstudio-constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/GB_AccessPoint_BNG.shp')
# accesspoints
# 
# location= c(-2.789108, 54.039093) # lancaster


#########       the function        ########

filter_accessible_locations <- function(location = c(-1.110557, 51.602436),
                                        distance = 1500,
                                        prow,
                                        greenspace,
                                        accesspoints){
  
  ####    Get buffer around location
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = 4326) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = 27700) # transform to BNG
  buffed <- st_buffer(trans_loc, distance) # create a buffer around the point
  
  
  ####    public rights of way
  # set the coordinates of the PROW dataset to BNG because I know that I set the coordinates in 'Process_foot_bridleway.R'
  st_crs(prow) <- 27700
  
  # get prow within distance from location
  prow_buf <- st_intersection(prow, buffed) # crop the sf object  
  
  
  ####    Greenspaces
  # get greenspaces and transform (not actually changing, just adding extra bits to match buffer)
  # grsp_geo <- st_transform((greenspace), crs = 27700)
  st_crs(greenspace) <- 27700 # I can just set the CRS given that now I have changed the file I wrote to disk in 'Process_greenspaces.R'
  
  # get greenspaces within distance from location
  grsp_buf <- st_intersection(greenspace, buffed) # crop the sf object  
  
  
  ####    Access points
  # get access points geometries
  # accp_geo <- st_transform((accesspoints), crs = 27700)
  st_crs(accesspoints) <- 27700 # I can just set the CRS given that now I have changed the file I wrote to disk in 'Process_greenspaces.R'
  
  # get access points within distance from location
  accp_buf <- st_intersection(accesspoints, buffed) # crop the sf object 
  
  
  ####    Plot the results (for testing)
  plot(prow_buf$geometry)
  plot(grsp_buf$geometry, col = rgb(0,255,0,max = 255, alpha = 100), add = T)
  plot(accp_buf$geometry, col = 'red', pch = 20, cex = 0.3, add = T)
  
  
  ####    Return the results
  return(list(prow = prow_buf, 
              greenspace = grsp_buf, 
              accesspoints = accp_buf))
  
}


system.time(filter_accessible_locations(location = c(-1.110557, 51.602436),
                                        distance = 2000,
                                        prow = prow,
                                        greenspace = greenspace,
                                        accesspoints = accesspoints))


