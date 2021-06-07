
## 

library(sf)

tn_df = thinned_nudges$nudges
access = final_acc_loc
buffer = 200


# remove NULL items in list and get each layer buffered
shapes_list <- lapply(access[lengths(access) != 0], FUN = function(x) ((st_buffer(x, buffer))))

out_nudge <- lapply(1:dim(tn_df)[1], FUN = function(n) { # I want to find which shapes each point is in
  
  out_shape <- lapply(1:length(shapes_list), FUN = function(x){
    
    # find the shapes that the point falls within
    if(sf::st_within(tn_df[n,], shapes_list[[x]]) %>% lengths > 0){# does the point fall within any of the shape?
      
      shps_within <- shapes_list[[x]][sf::st_within(tn_df[n,], shapes_list[[x]], sparse = F),]
      
      sw_df <- shps_within[,names(shps_within) %in% c('ROW_TYPE', 'function_', 'Descrip',
                                                      'accessType', 'Name', 'fclass')] %>% ## need to find all other names
        mutate(lon = unlist(map(tn_df[n,]$geometry,1)),
               lat = unlist(map(tn_df[n,]$geometry,2)),
               geometry = NULL) %>%
        as.data.frame()
      
      colnames(sw_df) <- c('layer_name', 'lon', 'lat')
      
      return(sw_df)
      
    } else(return(NULL))
    
    
    })
  
  
  return(do.call(rbind, out_shape))
  
})

do.call(rbind, out_nudge)

# # # if the point falls within buffered region store its index, otherwise skip to next loop
# # ifelse(any(int_ind), int_nudge <- tn_df[int_ind,], return(NULL))
# 
# # find the column containing the name
# cols <- shapes_list[[x]][,names(shapes_list[[x]]) %in% c('ROW_TYPE', 'function_')]
# 
# int_nudge[,names(int_nudge) %in% c('ROW_TYPE', 'function_')]
# 
# output <- cols %>%
#   mutate(lon = unlist(map(tn_df[1,]$geometry,1)),
#          lat = unlist(map(tn_df[1,]$geometry,2)),
#          geometry = NULL) %>%
#   as.data.frame() %>% 
#   rename(layer_name = ROW_TYPE)



# output: data frame with:


# shapes_list
# 
# do.call(c, lapply(1:dim(tn_df)[1], FUN = function(x) any(sf::st_within(tn_df[1,], shapes_list[[1]], sparse = FALSE))))
# shapes <- st_union(shapes_list[[1]], shapes_list[[2]])
