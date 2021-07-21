## Thin the DECIDE nudges using a grid of any size

# INPUT:

# decide_raster, # the original raster layer
# nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
# lon = 'lon', # the name of the longitude column in the data frame
# lat = 'lat',  # the name of the latitude column in the data frame
# crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
# buffer_distance, # the closest you want two points to be (km)
# plot = FALSE, # whether or not to plot output

# OUTPUT:

# data frame of nudges
# plot showing the original nudges and selected nudges (only is plot = TRUE)

nudge_thin <- function(decide_raster, # the original raster layer
                       nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
                       lon = 'lon', # the name of the longitude column in the data frame
                       lat = 'lat',  # the name of the latitude column in the data frame
                       crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
                       buffer_distance = 1, # the closest you want two points to be (km)
                       plot = FALSE) # whether or not to plot output
{
  
  # convert nudges to sf points object
  
  if(class(nudge_df)[1]=='data.frame'){
    
    spat_nudge <- st_as_sf(nudge_df, coords = c(lon, lat), crs = crs)
    
    # include this for plotting later so don't have to use an if() statement in the ggplot
    orig_nudges <- nudge_df
    
  } else if(class(nudge_df)[1]=='sf'){
    
    spat_nudge <- nudge_df
    
    # include this for plotting later so don't have to use an if() statement in the ggplot
    orig_nudges <- nudge_df %>%
      mutate(lon = unlist(map(nudge_df$geometry,1)),
             lat = unlist(map(nudge_df$geometry,2))) %>%
      as.data.frame()
    
    warning('! if plot looks wrong, check to make sure that coordinates in sf geometry column are in order c(lon, lat)')
    
  } else {
    
    stop('! Function only works with objects of class "sf" and "data.frame"')
    
  }
  
  # Thin the points using spThin and the distance specified
  # by the user
  
  # First spThin needs the points as lat and long
  spat_nudge_lat_lon <- st_transform(spat_nudge, crs = 4326)
  XY <- st_coordinates(spat_nudge_lat_lon)

  # Do the thinning
  sn_thin <- spThin::thin(loc.data = data.frame(sp = 'na',
                                               lon = XY[ , 'X'],
                                               lat = XY[ , 'Y']),
                         long.col = 'lon',
                         lat.col = 'lat',
                         spec.col = 'sp', 
                         thin.par = buffer_distance, 
                         reps = 1,
                         locs.thinned.list.return = TRUE,
                         write.files = FALSE,
                         write.log.file = FALSE)
  
  # Used the row numbers from the thinned dataset to subset
  # the original dataset (this is faster than transforming this new object)
  spat_nudge <- spat_nudge[as.numeric(row.names(sn_thin[[1]])), ]
  
  if(plot == TRUE){
    print('... Plotting...')
    ### fix plotting
    p <- ggplot() +
      geom_sf(data = conv_rast(decide_raster, crs), aes(fill = layer), col = NA) +
      geom_sf(data = spat_nudge, aes(col = 'Thinned nudges'), pch = 20, size = 3, pch = 1)  +
      geom_point(data = orig_nudges, aes(x=lon, y=lat, col = 'Original nudges'), pch = 20, size = 1.5) +
      theme_bw() +
      labs(y='Longtitude', x='Latitude') +
      scale_fill_continuous(type = 'viridis', name = 'Layer value') + 
      scale_colour_manual(name = '', values = c('red', 'yellow'))
    print(p)
    
    return(list(nudges = spat_nudge,
                plot = p))    
    
  } else {
    
    return(list(nudges = spat_nudge))
    
  }
  

  
}