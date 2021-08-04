
### getting metadata outline
rm(list = ls())


library(tidyverse)


# number of records within the same moving window as the smoothing
source('Scripts/modules/metadata_lcm.R')
source('Scripts/modules/metadata_species.R')
source('Scripts/modules/metadata_model_info.R')
source('Scripts/modules/rescale_metadata.R')
source('Scripts/modules/crop_records.R')


## Get the modal landcover for a location and surrounding locations
lcm <- raster(list.files(pattern = 'lcm', 'Data/metadata/', full.names = T))
# lcm


system.time(l <- find_lcm_features(rast_obj = lcm, # raster of land cover classes or data frame with associated factor levels
                                   # name_df = lcm_names, # data frame with names corresponding to the raster layer; has a default
                                   location = c(-2.784492, 54.024851),
                                   crds_loc = 4326,
                                   crds_obj = 27700,
                                   buffer_distance = 200))

l


# get the species in a location
# moths
system.time(dfm_full <- readRDS('Data/metadata/moth_records_by_100m.rds'))

system.time(metadata_species(location = c(-2.784492, 54.024851),
                             records_df = dfm_full, # records to find species in
                             crds_loc = 4326, # coordinates of the location vector
                             crds_df = 27700, # coordinates of the data frame
                             buffer_distance = 1000,
                             rounding = -2, # rounding to match the transformed coordinates to the data frame. -2 is 100m resolution
                             name_col = 'com_name'))



# butterfly
system.time(but_full <- readRDS('Data/metadata/butterfly_records_by_100m.rds'))

system.time(ms <- metadata_species(location = c(-2.784492, 54.024851),
                                   records_df = but_full, # records to find species in
                                   crds_loc = 4326, # coordinates of the location vector
                                   crds_df = 27700, # coordinates of the data frame
                                   buffer_distance = 1000,
                                   rounding = -2, # rounding to match the transformed coordinates to the data frame. -2 is 100m resolution
                                   name_col = 'com_name'))
ms


# get species richness, uncertainty and number of records
mths_meta <- raster::stack(paste0('Data/metadata/moth_recs_spprich_uncert_GB.grd'))
mths_meta

system.time(mm <- metadata_model_info(rast_obj = mths_meta,
                                      location = c(-2.784492, 54.024851),
                                      crds_loc = 4326, # coords of the location
                                      crds_rast = 27700,
                                      buffer_distance = 1000,
                                      rounding = -2))
mm

# % difference between central point uncertainty and surrounding cells
# this is just an example of what we could do with the NLG
mm$central_loc$mean_uncertainty/mean(mm$buffered_area$mean_uncertainty)*100

butt_meta <- raster::stack(paste0('Data/metadata/butterfly_recs_spprich_uncertSD_GB.grd'))
butt_meta

system.time(mb <- metadata_model_info(rast_obj = butt_meta,
                                      location = c(-2.784410, 54.02486),
                                      crds_loc = 4326, # coords of the location
                                      crds_rast = 27700,
                                      buffer_distance = NULL,
                                      rounding = -2))
mb


mb$central_loc$mean_uncertainty/mean(mb$buffered_area$mean_uncertainty)*100



##  crop the metadata raster to DECIDE score region
## scale uncertainty relative to all values
source('Scripts/modules/filter_distance.R')

rescale_metadata(metadata_loc_df = mb$central_loc, # the metadata of the central location returned from the metadata_model_info$central_loc function - in fact, can be any data frame as long as it has the same name as a layer in 'region_uncert_rast'
                 metadata_column = 'mean_uncertainty', # the column you want to rescale - must be same name as the name in the region_uncert_rast
                 region_uncert_rast = mths_meta, # an already cropped or uncropped rasterstack to use as background data
                 region_loc = c(-2.784492, 54.024851), ## point around which the original decide score region was extracted, if already cropped, set to NULL
                 distance = 5000, ## distance for the original Decide score area
                 min_val = 0, # minimum value for rescaling 
                 max_val = 9, # maximum value for rescaling
                 rounding = 0) # how many decimal places to give the returned value



