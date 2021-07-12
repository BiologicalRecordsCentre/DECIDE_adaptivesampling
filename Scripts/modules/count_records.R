# Count_records
# Summarise the number of records or time since last record per cell of a raster
# Returns a raster of the same size and resolution as the input raster
# the records need to be in the same coordinate system as the raster used.
# the records must have a lon, lat and date (in yyyymmdd format)
# The function does two things. it either counts the number of records in a cell
# if weight_by_time = FALSE.
# if weight_by_time = TRUE, then it calculates the number of years since the last record in a cell was made.
# and returns 1/number of years.
# could be easily altered to calculate the number of days since last record.
# In either case of weight_by_time, bigger numbers are bad.

### notes

# This might not work if the data and the raster are at different resolutions. 
# Slow on large rasters. Might be possible to do this in package sf rather than relying on raster. 
# Only possible if the coords of the data frame and raster are the same.


count_records <- function(records_df, # sf object or data frame/data table of record counts
                          template_raster, # raster that you want the counts to be aggregated by
                          Coords = 27700, 
                          weight_by_time = TRUE)
{
  
  # Get the counts of records per cell and store as data frame
  if(class(records_df)[1] == 'sf'){
    
    record_counts <- records_df %>% 
      mutate(lon = unlist(map(records_df$geometry, 1)),
             lat = unlist(map(records_df$geometry, 2)))  %>% 
      as.data.frame() %>% 
      mutate(geometry = NULL)
    
  } else if(class(records_df)[1] == 'data.table'|class(records_df)[1] == 'data.frame'){
    
    record_counts <- records_df
    
  } else { stop('!! records_df must be class sf or data.frame') }
  
  if(weight_by_time){
    
    record_weights <- record_counts %>% 
      group_by(lon, lat) %>% 
      dplyr::summarise(last_rec = max(year), # what is the most recent year?
                       last_date = ymd(max(date)), # what is the most recent date?
                       yrs_since_last_rec = (year(Sys.Date())-last_rec), # number of years since the last record in a grid cell
                       days_since_last_rec = as.numeric(Sys.Date()) - as.numeric(last_date), # number of days since the last record in a grid cell
                       score = 1/yrs_since_last_rec) %>% # get the big values small and vice-versa, so that larger numbers are 'bad' and smaller numbers are 'good', so that it matches the number of records layer that's also outputted by this function
      ungroup()
    
  } else if(!weight_by_time){
    
    record_weights <- record_counts
    
  } else {stop('Weight by time mst be TRUE/FALSE')}
  
  # convert to a spatial points data frame to match with raster
  xy <- record_weights[,c("lon","lat")]
  spdf.recs <- SpatialPointsDataFrame(coords = xy, data = record_weights,
                                             proj4string = CRS(paste0("+init=epsg:", Coords))) 
  
  ### create counts raster to store number of records in each cell ###
  # this is used to make an 'effort' layer
  n_recs <- template_raster
  
  # make a raster of zeroes for input
  n_recs[!is.na(n_recs)] <- 0
  
  if(weight_by_time){
    
    # get cell numbers for the spdf.recs data frame
    cells  <- (cellFromXY(n_recs,spdf.recs))
    
    # fill those cells with the score, have done some tests and 98% sure they are in the same order but ideally would need a match statement here
    n_recs[cells] <- spdf.recs$score
    
  } else if(!weight_by_time){
    
    # get the cell index for each point and make a table:
    counts  <- table(cellFromXY(n_recs,spdf.recs))
    
    # fill in the raster with the counts from the cell index:
    n_recs[as.numeric(names(counts))] <- counts
    
  }
  
  # mask cells falling outside of raster values
  n_recs <- mask(n_recs, template_raster)
  
  return(n_recs)
  
}

