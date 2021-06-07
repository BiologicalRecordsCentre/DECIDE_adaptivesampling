## Filter nudges based on proximity to an accessible area

# INPUT:
# nudges_df, the sf dataframe that contains the nudges 
# access_layers, accessible areas in the same location and same coordinates - could add functionality to check that this is the case. A single sf layer or a list of sf layers 
# buffer, the buffer to create around each accessible are - on same scale as access_layers (so metres for us),
# crs = ifelse(class(access_layers)=='list', st_crs(access_layers[[1]]), st_crs(access_layers)),
# lon = 'lon', # the name of the longitude column in the data frame, only used if nudges_df is a dataframe
# lat = 'lat',# the name of the latitude column in the data frame, only used if nudges_df is a dataframe
# plot, return the plot or not. The plot is still stored regardless of True or false


# OUTPUT: 
# sf dataframe of nudges
# plot of locations chosen



nudge_accessible <- function(nudges_df,
                             access_layers,
                             buffer = 100,
                             crs = ifelse(class(access_layers)=='list', st_crs(access_layers[[1]])[[1]], st_crs(access_layers)),
                             lon = 'lon',
                             lat = 'lat',
                             plot = FALSE){
  
  require(tidyverse)
  require(sf)
  
  if(class(nudges_df)[1]=="data.frame") nudges_df <- st_as_sf(nudges_df, coords = c(lon, lat), crs = crs)
  
  # remove NULL objects from the list
  access_layers_sub <- access_layers[lengths(access_layers) != 0]
  
  ## This for else if combination could really be improved and probably sped up
  if(class(access_layers_sub)[1]=='list' & length(access_layers_sub)==1){ # sf object stored as a one item list
    
    access_layers_sub <- access_layers_sub[[1]]
    
    # if one layer, buffer it
    shapes <- st_union(st_buffer(access_layers_sub, buffer))
    
  } else if(class(access_layers_sub)[1]=='list' && length(access_layers_sub)>1){ # a list of multiple sf objects
    
    ## add error code to check the class of each item in list
    
    # get each layer buffered
    shapes_list <- lapply(access_layers_sub, FUN = function(x) (st_union(st_buffer(x, buffer))))
    
    # combine the first two list objects; st_union only accepts two objects
    ## !! make this BETTER!! So hacky!! !! ##
    shapes <- st_union(shapes_list[[1]], shapes_list[[2]])
    
    # use a for loop to combine the other ones if the length of shape > 2
    if(length(shapes_list)>2) for(i in 3:length(shapes_list)){ shapes <- st_union(shapes, shapes_list[[i]]) }
    
    
  } else if(class(access_layers_sub)[1]=='sf'){ # a single sf object
    
    # if one layer, buffer it
    shapes <- st_union(st_buffer(access_layers_sub, buffer))
    
  } else {
    
    stop('!! only works on lists of "sf" obects or single "sf" objects')
    
  }
  
  # find the points that fall within the buffered shapes
  int_ind <- st_within(nudges_df, shapes, sparse = FALSE)
  int_nudge <- nudges_df[int_ind,]
  
  if(dim(int_nudge)[1]==0){
    
    print('! No nudges within buffered region; returning original nudges object with no plot. Consider increasing buffer size')
    
    return(list(nudges = nudges_df,
                plot = NULL))
    
  }
  
  
  # # base plot
  # print(plot(st_geometry(shapes), border = 'blue'))
  # print(lapply(access_layers_sub, FUN = function(x) plot(st_geometry(x), col = 'green', border = 'green', add = T)))
  # print(plot(st_geometry(nudges_df), add = T, pch=20)) 
  # print(plot(st_geometry(int_nudge), col = 'red', add = T, pch = 20))
  
  # ggplot
  p <- ggplot() +
    geom_sf(data=shapes, aes(colour = 'Buffered accessible\nareas'), fill = NA) +
    theme_bw() 
  
  for(pls in 1:length(access_layers_sub)){
    
    if(dim(access_layers_sub[[pls]])[1]==0) next
    
    p <- p + 
      geom_sf(data=access_layers_sub[[pls]], aes(colour = 'Accessible areas', fill = 'Accessible areas'))
  }
  
  p <- p +
    geom_sf(data = int_nudge, aes(colour = 'Nudges within\nbuffered area'), cex = 2) + 
    geom_sf(data = nudges_df, aes(colour = 'Original nudges'), cex = 1) +
    scale_colour_manual(name = '', values = c('green3', 'green4', 
                                              'red', 'black')) +
    scale_fill_manual(values = 'green3') +
    guides(fill = FALSE)
  
  if(plot == T) print(p)
  
  return(list(nudges = int_nudge,
              plot = p))
  
}