## Thin the DECIDE nudges using a grid of any size

# INPUT:

# decide_raster, # the original raster layer
# nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
# lon = 'lon', # the name of the longitude column in the data frame
# lat = 'lat',  # the name of the latitude column in the data frame
# crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
# buffer_distance, # the resolution of the grid to sample from - on the scale of the raster
# sample_num, # number of nudges to sample per grid
# plot = FALSE, # whether or not to plot output
# square = TRUE) # use a square or hexagonal grid

# OUTPUT:

# data frame of nudges
# the grid, and
# plot showing the original nudges, selected nudges and grid used (optional)



nudge_thin <- function(decide_raster, # the original raster layer
                       nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
                       lon = 'lon', # the name of the longitude column in the data frame
                       lat = 'lat',  # the name of the latitude column in the data frame
                       crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
                       buffer_distance, # the resolution of the grid to sample from - on the scale of the raster
                       sample_num, # number of nudges to sample per grid
                       plot = FALSE, # whether or not to plot output
                       square = TRUE) # use a square or hexagonal grid
{
  
  # create a grid to sample from
  spat_grd <- st_make_grid(conv_rast(decide_raster, crs), cellsize = buffer_distance, square = square)
  
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
  
  
  
  # go through each grid cell, sample from all the points in the grid
  # return sample_num points per grid cell
  l_out <- lapply(1:length(spat_grd), FUN = function(x){
    
    # sample from a grid cell
    # only if grid contains a point
    if(dim(st_intersection(spat_nudge, spat_grd[x,]))[1]>0){
      
      # sample 'sample_num' points from the grid cell of interest
      # could also determine number of nudges based on the number of points in the cell
      # i.e. the proportion of points in that cell?
      sample_index <- sample(1:dim(st_intersection(spat_nudge, spat_grd[x,]))[1], 
                             size = ifelse(sample_num <= dim(st_intersection(spat_nudge, spat_grd[x,]))[1], # if the number of points asked for is less than the number in the cell then  
                                           sample_num, # return the number asked for
                                           dim(st_intersection(spat_nudge, spat_grd[x,]))[1])) # if more, then return all the points in the cell
      
      # return the sampled point within that grid
      return(st_intersection(spat_nudge, spat_grd[x,])[sample_index,])
    }
    
  })
  
  ### fix plotting
  p <- ggplot() +
    geom_sf(data = conv_rast(decide_raster, crs), aes(fill = layer), col = NA) +
    geom_sf(data = spat_grd, fill = NA, col = 'black') +
    geom_sf(data = do.call('rbind', l_out), aes(col = 'Thinned nudges'), pch = 20, size = 3, pch = 1)  +
    geom_point(data = orig_nudges, aes(x=lon, y=lat, col = 'Original nudges'), pch = 20, size = 1.5) +
    theme_bw() +
    labs(y='Longtitude', x='Latitude') +
    scale_fill_continuous(type = 'viridis', name = 'Layer value') + 
    scale_colour_manual(name = '', values = c('red', 'yellow'))
  
  if(plot == TRUE){
    print('... Plotting...')
    print(p)
  }
  
  return(list(nudges = do.call('rbind', l_out),
              grid = spat_grd,
              plot = p))
  
}