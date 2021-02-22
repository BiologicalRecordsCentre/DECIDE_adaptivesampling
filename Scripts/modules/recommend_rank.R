# Aggregate the uncertainty metric across all species and for each species separately; rank the cells highest to lowest
# 
# _Input_: Output of function 'recommend_metric()': rasterstack of one metric, a layer for each species; Method
# 
# _Output_: Rasterstack, the input metric and inverse rank of cells in area. Different for each method
# _Output_: Method = 'additive' : 2 rasters, original metric and inverse rank of cells metric 
# _Output_: Method = 'species_rank' :  2 rasters for each species, original metric and inverse rank of cells metric 
# 
# _Metadata_: ???


##' Need to work out how this will work with multiple models
##' As we are not interested in the model used per se, probably need a 
##' new function that averages the models using the AUC - just use rob's function
##' to do this

##' aggregate prediction, errors and ranks across all species
##' Include in the function a way to get the rank across all species when added together
##' but also get a rank that done for each species separately
##' _input_ prediction+error rasterstack of species (output of recommend_metric()); 
##' _input_ cont.: method for recommendation (only rank so far);
##' _input_ cont.: how to aggregate predictions (don't know if useful) - across species/species separately ---- HAVE A THINK ----
##' _outout_ method = 'additive' : 2 rasters, original metric and inverse rank of cells metric 
##' _outout_ method = 'species_rank' :  2 rasters for each species, original metric and inverse rank of cells metric 


recommend_rank <- function(predict_err_raster,
                           method = NULL){ # c('additive', 'species_rank'), one of these options
  
  # stop if method not specified
  if(!all(method %in% c('additive', 'species_rank'))){
    
    stop("'methods are limited to:'additive', 'species_rank'")
    
  }
  
  # if additive, sum across all species,
  # if species_rank retain the error for each species separately
  if(method == 'additive'){
    rast <- sum(predict_err_raster) # sum raster layer of predict_err_raster across all species
  } else if(method == 'species_rank') {
    rast <- predict_err_raster # keep the raster for each species separately
  }
  
  comb_df <- as.data.frame(rast, xy = T) # convert raster to data frame
  # head(comb_df)
  
  # to get the rank need to use 'quo()' argument to create a conditional 'group_by()' statement later
  if(method == 'additive'){
    quo_var <- quo() # if ranking across all species, don't need to 'group_by()' anything, so need an empty variable
  } else if(method == 'species_rank'){
    # if want the rank for each species separately, need to 'group_by' species
    # to be able to do this, need to change the data frame created from 'as.data.frame' into long format
    
    # convert into long format to get rank in same way as above - would be easier with pivot_longer/pivot_wider
    comb_df <- melt(comb_df, id.vars = c('x', 'y'), 
                    variable.name = 'species',
                    value.name = 'layer')
    
    quo_var <- quo(species) # save 'species' as the grouping variable using the 'quo()' function
  }
  
  # now, need to create the ranking
  # it is the 'inverse_ranking' to make plotting easier later so don't have to mess with colour scale
  rank_df <- comb_df %>%
    group_by(!!quo_var) %>% # this will group_by() species if the method == 'species_rank'; empty if method == 'additive'
    arrange(layer) %>% # sort by layer, increasing
    mutate(inverse_rank = seq(1, length(layer)), # create a rank, highest rank, highest prob+err
           inverse_rank = ifelse(is.na(layer), NA, inverse_rank)) %>% # get rid of NAs so left with original dimension raster
    rename(error_metric = layer)
  
  ##'  If the method=='species_rank' want to have it in wide format to convert to a raster stack for each species
  ##'  Could do this without an lapply() statement if could use pivot_wider.
  ##'  Without pivot_wider, need to convert each subset of data frame separately and store as list
  if(method == 'species_rank'){
    
    spp <- unique(rank_df$species) %>% sort()
    
    rank_df <- lapply(spp, ranked_df = rank_df, FUN = function(species, ranked_df){
      # subset the dataframe by species
      sub_df <- ranked_df[ranked_df$species == species,]
      
      # convert to raster
      sp_rast <- rasterFromXYZ(sub_df[,!names(sub_df) %in% c("species")]) # remove the species name from the layer
      
      # return raster
      return(sp_rast)
    })
    
    names(rank_df) <- spp
    
    return(rank_df)
  } else ( # for all species together, create the raster and return it
    
    # convert the back to raster
    return(rasterFromXYZ(rank_df)))
  
}


# oi <- recommend_rank(predict_err_raster = additive_metric,
#                method = 'additive')
