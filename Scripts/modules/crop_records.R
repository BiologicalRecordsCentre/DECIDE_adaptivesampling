## Crop a data frame to the extent of a raster
# coords must be the same
# Needs to be an object can get raster::extent() from


crop_records <- function(cropped_raster, # a raster cropped to the extent of interest 
                         records_df){
  
  # find the extent of the cropped area
  cropped_extent <- raster::extent(cropped_raster)
  
  # subset the records
  cropped_records_df <- subset(records_df, lon >= cropped_extent[1] &
                                 lon <=  cropped_extent[2] &
                                 lat >= cropped_extent[3] &
                                 lat <= cropped_extent[4])
  
  return(cropped_records_df)
  
}