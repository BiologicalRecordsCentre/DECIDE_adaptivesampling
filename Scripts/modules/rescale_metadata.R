
# function to rescale the metadata between 0-9 for use in the NLG

rescale_metadata <- function(metadata_loc_df, # the metadata of the central location returned from the metadata_model_info$central_loc function - in fact, can be any data frame as long as it has the same name as a layer in 'region_uncert_rast'. First two columns MUST be lon, lat and be on same coordinates system as the raster
                             metadata_column = 'mean_uncertainty', # the column you want to rescale - must be same name as the name in the region_uncert_rast
                             region_uncert_rast = butt_meta, # an already cropped or uncropped rasterstack to use as background data
                             region_loc = c(-2.784492, 54.024851), ## point around which the original decide score region was extracted, if already cropped, set to NULL
                             distance = 5000, ## distance for the original Decide score area
                             min_val = 0, # minimum value for rescaling 
                             max_val = 9, # maximum value for rescaling
                             rounding = 0){ # number of decimal places to return the value at
  
  require(scales)
  
  # sort out metadata df
  mdat <- metadata_loc_df[,c(1, 2, which(colnames(metadata_loc_df)==all_of(metadata_column)))] # find the column name that matches "metadata_column"
  colnames(mdat) <- c('x', 'y', metadata_column)
  mdat$layer_level <- 'metadata_loc'
  
  # crop the background uncertainty layer to the original DECIDE region
  if(!is.null(region_loc)){
    
    suppressWarnings(cropped_background <- filter_distance(region_uncert_rast, 
                                                           location = region_loc,
                                                           distance = distance) %>% 
                       as.data.frame(xy=T) %>% 
                       mutate(layer_level = 'background') %>% 
                       na.omit %>% 
                       dplyr::select(x,y,all_of(metadata_column),layer_level))
    
    # check to make sure central loc is within cropped raster extent
    if(!(mdat$x < max(cropped_background$x) & mdat$x > min(cropped_background$x) & 
         mdat$y < max(cropped_background$y) & mdat$y > min(cropped_background$y))) {
      warning("!! Location of interest does not fall within background extent, are you sure that's what you want?")}
    
    
  } else if(is.null(region_loc)){
    
    print("Converting raster to dataframe. If raster is not already cropped, this could take a while.")
    
    cropped_background <- region_uncert_rast %>% 
      as.data.frame(xy=T) %>% 
      mutate(layer_level = 'background') %>% 
      na.omit %>% 
      dplyr::select(x,y,all_of(metadata_column),layer_level)
    
  }
  
  
  # bind the two together
  comb_df <- rbind(mdat, cropped_background)
  
  # rescale between 0-9
  comb_df$rescaled_values <- round(scales::rescale(comb_df[,metadata_column], to = c(min_val,max_val)), digits = rounding)
  
  return(comb_df$rescaled_values[comb_df$layer_level=='metadata_loc'])
  
}
