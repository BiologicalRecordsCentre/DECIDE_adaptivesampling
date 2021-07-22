rm(list = ls())

### creating static nudge layer with associated metadata
source('Scripts/modules/filter_distance.R')
library(data.table)
library(tidyverse)


# number of records within the same moving window as the smoothing
source('Scripts/modules/smooth_recording.R')
source('Scripts/modules/count_records.R')
source('Scripts/modules/metadata_lcm.R')

# modal landcover
lcm <- raster(list.files(pattern = 'lcm', 'Data/metadata/', full.names = T))
# lcm


find_lcm_features(rast_obj = lcm, # raster of land cover classes or data frame with associated factor levels
                  # name_df, # data frame with names corresponding to the 
                  location = c(-2.784492, 54.024851),
                  crds_loc = 4326,
                  crds_obj = 27700,
                  buffer_distance = 200)


# list of species recorded in the cell
taxa = c('moth', 'butterfly')
pseudoabs = 'PA_thinned_10000nAbs'

recs_out <- list()

for(tax in taxa){
  
  print(tax)
  
  if(tax == 'moth') {
    dfm_full <- fread("Data/species_data/moth/DayFlyingMoths_EastNorths_no_duplicates.csv")
  } else if(tax == 'butterfly'){
    dfm_full <- fread("Data/species_data/butterfly/butterfly_EastNorths_no_duplicates.csv")
  }
  
  
  # metadata_species(location = c(-1.097367, 51.604963),
  #              records_df = dfm_full,
  #              crds_loc = 4326,
  #              crds_df = 27700,
  #              buffer_distance = NULL,
  #              rounding = -1,
  #              name_col = 'sp_n')
  
  
  print('getting number of records')
  number_records <- count_records(records_df = dfm_full,
                                  template_raster = lcm,
                                  weight_by_time = FALSE)
  
  # # no longer want to do a moving window summary
  # sm_matrix = matrix(c(0,    1, 1, 1,    0,
  #                      1, 1, 1, 1, 1,
  #                      1, 1,    1, 1, 1,
  #                      1, 1, 1, 1, 1,
  #                      0,    1, 1, 1,    0), 
  #                    nrow = 5, ncol = 5)
  # 
  # # r <- raster(ncols=36, nrows=18, xmn=0)
  # # values(r) <- round(runif(ncell(r), max = 10))
  # # unique(r)
  # # plot(r)
  # 
  # smoothed_effort <- focal(x = cr, 
  #                          w = sm_matrix,
  #                          fun = sum,
  #                          pad = TRUE,
  #                          padValue = NA,
  #                          na.rm = T,
  #                          NAonly = F)
  # 
  # plot(smoothed_effort)
  # 
  # recs_out[[i]] <- smoothed_effort
  # names(recs_out[[i]]) <- i
  
  # # save the counds in each cell
  # writeRaster(cr, filename = paste0('Data/metadata/',tax,'_summed_records.grd'),
  #             overwrite = TRUE)
  
  
  
  ### species-level uncertainty and probability of presence
  model_locs <- paste0('/data-s3/thoval/sdm_outputs/', tax, '/combined_model_outputs/', pseudoabs)
  
  names <- gsub(pattern = '_PA_thinned_10000nAbs_weightedmeanensemble.grd', replacement = '', 
                list.files(model_locs, 
                           pattern='_weightedmeanensemble.grd'))
  
  # sdm outputs for each species
  species_stack <- list()
  
  # error outputs
  error_out <- list()
  
  print('loading rasters from object store')
  
  for(i in 1:length(names)){
    
    print(names[i])
    
    # initiate model list within for loop so that it gets replaced when starting a new species
    # otherwise we might get some weird overlaps
    
    # mean predictions
    mp <- list.files(model_locs, 
                     pattern = paste0(names[i], "_", pseudoabs, "_weightedmeanensemble.grd"),
                     full.names = TRUE)
    
    mod_preds <- raster::stack(mp)
    names(mod_preds) <- paste0(names[i], '_mean_pred')
    
    # quantile range
    qr <- list.files(model_locs, 
                     pattern = paste0(names[i], "_", pseudoabs, "_rangeensemblequantiles.grd"),
                     full.names = TRUE)
    
    qrnge <- raster::stack(qr)
    names(qrnge) <- paste0(names[i], '_quantile_range')
    
    species_stack[[i]] <- raster::stack(mod_preds, qrnge)
    
  }
  
  
  # species richness
  print('calculating species richness')
  spp_richness <- round(sum(stack(lapply(species_stack, FUN = function(x) subset(x, grep(pattern = 'mean_pred',
                                                                                         names(x)))))))
  
  # uncertainty score
  print('calculating mean uncertainty')
  uncertainty <- mean(stack(lapply(species_stack, FUN = function(x) subset(x, grep(pattern = 'quantile_range',
                                                                                   names(x))))))
  
  metadata <- raster::stack(spp_richness, uncertainty)
  
  ## mask
  ## crop to GB
  # download map GB
  uk_map <- st_as_sf(getData("GADM", country = "GBR", level = 1, path='Data/environmental_data'))
  uk_map <- st_transform(uk_map, 27700)
  
  # remove nrothern ireland
  gb_map <- uk_map[uk_map$NAME_1 != 'Northern Ireland',]
  
  # convert to spatial for use in raster::mask()
  gb_mask <- as_Spatial(gb_map)
  
  # crop to GB
  print('cropping to GB')
  metadata_GB <- raster::mask(metadata, gb_mask[1])
  
  # crop number of records to same extent as the species richness/uncertainty raster
  nrecs_cropped <- raster::crop(number_records, metadata_GB)
  
  # stack the records
  metadata_GB <- raster::stack(nrecs_cropped, metadata_GB)
  names(metadata_GB) <- c('number_records', 'spp_richness', 'mean_uncertainty')
  metadata_GB
  
  # write out
  print('writing to file')
  writeRaster(metadata_GB, filename = paste0('Data/metadata/', tax, '_recs_spprich_uncert_GB.grd'),
              format = 'raster')
  
  
}

mths_meta <- raster::stack(paste0('Data/metadata/moth_recs_spprich_uncert_GB.grd'))
mths_meta

butt_meta <- raster::stack(paste0('Data/metadata/butterfly_recs_spprich_uncert_GB.grd'))
butt_meta

plot(mths_meta)
plot(butt_meta)


