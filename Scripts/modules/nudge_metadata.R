## Get metadata associated with the access for each nudge

## INPUT: 
# the thinned nudges
# access layers
# buffer - same as the one used for nudge_accessible
# metadata_col_names - names of the columns or objects that we want returned for each of the accessible layers
# if we add more layers or change any, this list will need to be updated


nudge_metadata <- function(nudges_df,
                           access,
                           buffer = 200,
                           metadata_col_names = c('ROW_TYPE', 'function_', 'Descrip',
                                                  'accessType', 'Name', 'fclass')) { # names need to be manually entered into here
  
  # remove NULL items in list and get each layer buffered
  shapes_list <- lapply(access[lengths(access) != 0], FUN = function(x) (st_buffer(x, buffer)))
  
  # find which shapes each point is in and return their metadata for each point
  out_nudge <- lapply(1:dim(nudges_df)[1], FUN = function(n) {  # go through each point in turn
    
    # for each point, go through the different access layers
    out_shape <- lapply(1:length(shapes_list), FUN = function(x){ 
      
      # does the point fall within any of the shape?
      if(sf::st_within(nudges_df[n,], shapes_list[[x]]) %>% lengths > 0){
        
        # get the items in the list that the point of interest falls within
        shps_within <- shapes_list[[x]][sf::st_within(nudges_df[n,], shapes_list[[x]], sparse = F),]
        
        # find the columns of interest and return it as a dataframe
        sw_df <- shps_within[,names(shps_within) %in% metadata_col_names] # %>% 
          
        if(dim(sw_df)[2]>2) sw_df <- sw_df[,2:3]; warning('!! dropping first column, check that the layer names make sense !!')
        
        sw_df <- sw_df %>% 
          mutate(lon = unlist(map(nudges_df[n,]$geometry,1)),
                 lat = unlist(map(nudges_df[n,]$geometry,2)),
                 geometry = NULL) %>%
          as.data.frame() %>% 
          distinct()
        
        colnames(sw_df) <- c('layer_name', 'lon', 'lat')
        
        return(sw_df)
        
      } else(return(NULL))
      
      
    })
    
    
    return(do.call(rbind, out_shape))
    
  })
  
  return(do.call(rbind, out_nudge))
  
}
