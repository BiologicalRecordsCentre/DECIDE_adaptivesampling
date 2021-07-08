rm(list = ls())

library(tidyverse)
library(doParallel)
library(foreach)
library(raster)
library(viridis)
library(lubridate)
library(data.table)
library(sf)

source("Scripts/modules/filter_distance.R")

taxa = 'moth'
pseudoabs = 'PA_thinned_10000nAbs'

if(taxa == 'butterfly'){
  
  dfm_full <- fread("Data/species_data/butterfly/butterfly_EastNorths_no_duplicates.csv")
  
} else if(taxa == 'moth'){
  
  # read in records data
  dfm_full <- fread("Data/species_data/moth/DayFlyingMoths_EastNorths_no_duplicates.csv")
  
}


model_locs <- paste0('/data-s3/thoval/sdm_outputs/', taxa, '/combined_model_outputs/', pseudoabs)

names <- gsub(pattern = '_PA_thinned_10000nAbs_weightedmeanensemble.grd', replacement = '', 
              list.files(model_locs, 
                         pattern='_weightedmeanensemble.grd'))
# names



######     Get models
# sdm outputs for each species
species_stack <- list()

# error outputs
error_out <- list()

for(i in 1:length(names)){
  
  print(names[i])
  
  # initiate model list within for loop so that it gets replaced when starting a new species
  # otherwise we might get some weird overlaps
  
  # quantile range
  qr <- list.files(model_locs, 
                   pattern = paste0(names[i], "_", pseudoabs, "_rangeensemblequantiles.grd"),
                   full.names = TRUE)
  
  qrnge <- raster::stack(qr)
  names(qrnge) <- paste0(names[i], '_quantile_range')
  
  species_stack[[i]] <- raster::stack(qrnge)
  
}



##### Get mean

registerDoParallel(7)

# out_cropped <- list()
system.time(
  out_cropped <- foreach(s = 1:length(species_stack)) %dopar% {
    
    print(s)
    
    sp <- species_stack[[s]]
    
    # get the error
    m_quant_av <- subset(sp, grep(pattern = 'quantile_range',
                                  names(sp)))
    
    out_rasts <- list(m_quant_av)
    names(out_rasts) <- c('quantile_var')
    
    return(out_rasts)
    
  }
)

registerDoSEQ()

names(out_cropped) <- names


# get the cropped variation
var <- stack(lapply(1:length(out_cropped), FUN = function(x) out_cropped[[x]]$quantile_var))
names(var) <- names(out_cropped)

decide <- mean(var)


######     Sort out species records

dfm <- dfm_full %>% 
  dplyr::select(lon, lat, date, year = yr, species = sp_n, common_name=com_n)

####    Function
# not sure if this will work with raster and sf object with different resolutions
count_records <- function(records_df, # sf object of record counts
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
    
  } else if(class(records_df)[1] == 'data.table'){
    
    record_counts <- records_df
    
  } else{ stop('!! records_df must be class sf or data.frame') }
  
  if(weight_by_time){
    
    # # get the years ranked    
    # yr_rnk <- data.frame(yr = unique(record_counts$year), rnk = rank(unique(record_counts$year)))
    # 
    # # bind to records data.frame
    # record_counts$rnk <- yr_rnk$rnk[match(record_counts$year, yr_rnk$yr)]
    
    record_weights <- record_counts %>% 
      group_by(lon, lat) %>% 
      dplyr::summarise(last_rec = max(year), # what is the most recent year?
                       last_date = ymd(max(date)), # what is the most recent date?
                       yrs_since_last_rec = (year(Sys.Date())-last_rec), # number of years since the last record in a grid cell
                       days_since_last_rec = as.numeric(Sys.Date()) - as.numeric(last_date), # number of days since the last record in a grid cell
                       score = 1/yrs_since_last_rec) %>% # get the big values small and vice-versa, so that larger numbers are 'bad' and smaller numbers are 'good', so that it matches the number of records layer that's also outputted by this function
      ungroup()
    
    # convert to a spatial points data frame to match with raster
    xy <- record_weights[,c("lon","lat")]
    spdf.recs_weight <- SpatialPointsDataFrame(coords = xy, data = record_weights,
                                               proj4string = CRS(paste0("+init=epsg:", Coords))) 
    
    ### create counts raster to store number of records in each cell ###
    # this is used to make an 'effort' layer
    n_recs <- template_raster
    
    # make a raster of zeroes for input
    n_recs[!is.na(n_recs)] <- 0
    
    # get cell numbers for the spdf.recs data frame
    cells  <- (cellFromXY(n_recs,spdf.recs_weight))
    
    # fill those cells with the score, have done some tests and 98% sure they are in the same order but ideally would need a match statement here
    n_recs[cells] <- spdf.recs_weight$score
    
  } else if(!weight_by_time){
    
    # convert to a spatial points data frame to match with raster
    xy <- record_counts[,c("lon","lat")]
    spdf.recs <- SpatialPointsDataFrame(coords = xy, data = record_counts,
                                        proj4string = CRS(paste0("+init=epsg:", Coords))) 
    
    ### create counts raster to store number of records in each cell ###
    # this is used to make an 'effort' layer
    n_recs <- template_raster
    
    # make a raster of zeroes for input
    n_recs[!is.na(n_recs)] <- 0
    
    # get the cell index for each point and make a table:
    counts  <- table(cellFromXY(n_recs,spdf.recs))
    
    # fill in the raster with the counts from the cell index:
    n_recs[as.numeric(names(counts))] <- counts
    
  } else {stop('Weight by time mst be TRUE/FALSE')}
  
  return(n_recs)
  
}

# get weighting layer - years since last record
rec_counts_time <- count_records(records_df = dfm, 
                                 template_raster = decide,
                                 weight_by_time = TRUE)


#####    Decide weighted by time since last record
# Function

smooth_recording <- function(weighted_layer, # layer to be weighted
                             effort_raster, 
                             recording_impact = 10) # the larger the value the less impact it has
{
  
  smoothed_effort <- focal(x = effort_raster, 
                           w = matrix(c(0,    0.09, 0.11, 0.09,    0,
                                        0.09, 0.21, 0.33, 0.21, 0.09,
                                        0.11, 0.33,    1, 0.33, 0.11,
                                        0.09, 0.21, 0.33, 0.21, 0.09,
                                        0,    0.09, 0.11, 0.09,    0), 
                                      nrow = 5, ncol = 5),
                           pad = TRUE,
                           padValue = 0,
                           NAonly = T)
  
  # Convert recording to weighting
  weighting <- smoothed_effort
  weighting <- 1/(1+(weighting*recording_impact)) # Outputs a layer of 1s where there are no records and progressively goes to 0 where there are more records
  
  # set minimum value to original layer
  M <- minValue(weighted_layer)
  
  # for now, just multiplying the weighted_layer by the weighting
  adjusted_score <- weighted_layer * weighting
  adjusted_score[adjusted_score < M] <- M # change the lowest records to be equal to the original decide score minimum
  
  
  
  return(list(weighting_layer = weighting,
              weighted_score = adjusted_score))
  
}


## decide weighted by time
decide_time <- smooth_recording(weighted_layer = decide,
                                effort_raster = rec_counts_time,
                                recording_impact = 5)
par(mfrow = c(1,2))
plot(decide)
plot(decide_time$weighted_score)
par(mfrow = c(1,1))

# download map GB
uk_map <- st_as_sf(getData("GADM", country = "GBR", level = 1, path='Data/'))
uk_map <- st_transform(uk_map, 27700)

# remove nrothern ireland
gb_map <- uk_map[uk_map$NAME_1 != 'Northern Ireland',]

# check
plot(st_geometry(gb_map))

# convert to spatial for use in raster::mask()
gb_mask <- as_Spatial(gb_map)

# crop
decide_time_GB <- raster::mask(decide_time$weighted_score, gb_mask[1])
plot(decide_time_GB)

writeRaster(decide_time_GB, filename = paste0('outputs/', taxa, '_DECIDE_Score_variation_weighted_by_time_since.grd'),
            format = 'raster', overwrite = TRUE)

w <- filter_distance(decide_time$weighted_score,
                     location = location,
                     distance = 5000,
                     method = 'buffer')
library(viridis)
plot(w, col = viridis(50))
