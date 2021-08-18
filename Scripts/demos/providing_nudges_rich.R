rm(list= ls())

## Nudges for Rich


## packages
library(tidyverse)
library(raster)
library(patchwork)
library(spThin)

## custom functions
source("Scripts/modules/filter_distance.R")
source("Scripts/modules/convert_raster.R")
source("Scripts/modules/metadata_accessible.R")
source("Scripts/modules/nudge_thin.R")
source("Scripts/modules/nudge_list.R")
source("Scripts/modules/nudge_accessible.R")
source("Scripts/modules/load_gridnumbers.R")


## load the decide score raster into R and crop it to size
mgb <- raster('/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Data/species_data/decide_scores/Actually_fixed_coasts_butterfly_PA_thinned_10000nAbs_decide_score_var_only_mean_time_since.grd')


## location and distance (same as whatever it is for user)
location = c(-1.110557, 51.602436) # wallingford
location = c(-2.763206, 54.042394) # lancaster
location = c(1.674618, 52.324225) # southwold
location = c(1.7246736494050823, 52.39079202181886) # southwold
# location = c(1.721925851841557, 52.331772607553205) # southwold
# location = c(1.6389400352906682, 52.3130964236375) # southwold
location = c(1.6394552473338566, 52.31425079836214) # southwold
location = c(1.3874346942977713, 52.01489483811975) # 


distance = 5000

# crop to size
wall_m <- filter_distance(mgb,
                          location=location,
                          distance=distance,
                          method = 'buffer')


## get a list of nudge locations
# probably best to leave these as they are for now
nudges <- nudge_list(wall_m,
                     n=1000,
                     # prop = 0.01,
                     cutoff_value = 0.75, 
                     cutoff = TRUE,
                     weight = TRUE,
                     weighting_column = 'decide_score',
                     weight_inflation = 50)
head(nudges) ## produces a data frame of nudges with their decide scores
dim(nudges)

## LOAD THE ACCESS LAYERS

# load a UK grid - this is the same process as in the other scripts
grid <- st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/UK_grids/uk_grid_10km.shp')
st_crs(grid) <- 27700

# find grid numbers that location + buffered zone covers
grid_numbers <- load_gridnumbers(location = location,
                                 distance = distance,
                                 grid = grid)

# shape data
# point to shape locations - need to change to wherever these are located on the tool
{# england
  prow_loc <- ("/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/rowmaps_footpathbridleway/rowmaps_footpathbridleway/gridded_data_10km/")
  grnspc_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_greenspace_data_10km/"
  accspnt_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_accesspoint_data_10km/"
  access_land_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/CRoW_Act_2000_-_Access_Layer_(England)-shp/gridded_data_10km/"
  nat_trust_loc <- '/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/national_trust/gridded_data_10km/'
  lond_path_loc <- '/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/greater-london-latest-free/london_gridded_data_10km/'
}

# extract shapes for location of interest
system.time(
  acc_loc <- lapply(c(1:length(grid_numbers)), FUN = function(n){
    
    
    ####    ENGLAND    ####
    # prow .shp
    prow_files <- list.files(prow_loc,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(prow_files) != 0) {
      prow <- sf::st_read(prow_files, quiet = TRUE)
      st_crs(prow) <- 27700
    } else { prow <- NULL }
    
    # greenspaces .shp
    grnspc_files <- list.files(grnspc_loc,
                               full.names = T,
                               pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(grnspc_files) != 0){
      grnspc <- sf::st_read(grnspc_files, quiet = TRUE)
      st_crs(grnspc) <- 27700
    } else { grnspc <- NULL }
    
    # access points .shp
    accs_files <- list.files(accspnt_loc,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(accs_files) != 0){
      accspnt <- sf::st_read(accs_files, quiet = TRUE)
      st_crs(accspnt) <- 27700
    } else{ accspnt <- NULL }
    
    # access land .shp
    accslnd_files <- list.files(access_land_loc,
                                full.names = T,
                                pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(accslnd_files) != 0){
      accslnd <- sf::st_read(accslnd_files, quiet = TRUE)
      st_crs(accslnd) <- 27700
    } else { accslnd <- NULL }
    
    # national trust .shp
    national_trust_files <- list.files(nat_trust_loc,
                                       full.names = T,
                                       pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(national_trust_files) != 0){
      national_trust <- sf::st_read(national_trust_files, quiet = TRUE)
      st_crs(national_trust) <- 27700
    } else { national_trust <- NULL }
    
    # london shapes .shp
    london_files <- list.files(lond_path_loc,
                               full.names = T,
                               pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(london_files) != 0){
      london_paths <- sf::st_read(london_files, quiet = TRUE)
      st_crs(london_paths) <- 27700
    } else { london_paths <- NULL }
    
    return(list(prow, grnspc, accspnt, accslnd, national_trust, london_paths)) ## england

  })
)

all_outs <- do.call(Map, c(rbind, acc_loc))

# crop shapes to exact region of interest
final_acc_loc <- lapply(all_outs, filter_distance,
                        location = location,
                        distance = distance,
                        method = 'buffer')


## Find nudges within certain distance of accessible areas
accessible_nudges <- nudge_accessible(nudges_df = nudges,
                                      access_layers = final_acc_loc,
                                      buffer = 200,
                                      crs = 27700,
                                      lon = 'lon',
                                      lat = 'lat',
                                      plot = FALSE)
accessible_nudges$nudges


## Thin the nudges to only return those at least X apart
thinned_nudges <- nudge_thin(decide_raster = wall_m,
                             nudge_df = accessible_nudges$nudges,
                             lon = 'lon',
                             lat = 'lat',
                             crs = 27700, 
                             buffer_distance = 0.5, # distance in kilometres (not metres)
                             plot = FALSE)
thinned_nudges$nudges # sf data frame of nudges and their decide scores


## easy way of converting it to a data frame 
ndf <- thinned_nudges$nudges %>% as.data.frame() %>% 
cbind(st_coordinates(thinned_nudges$nudges)) %>% 
  mutate(geometry = NULL) %>% 
  arrange(-decide_score)
dim(ndf)

ggplot() +
  geom_tile(data=as.data.frame(wall_m,xy=T), aes(x=x,y=y,fill=layer), alpha=0.7) +
  viridis::scale_fill_viridis(na.value = NA) #+
# geom_point(data=ndf,aes(x=X, y=Y), colour = 'red', cex = 0.5)

# # testing
# bu <- raster::stack('Data/metadata/butterfly_recs_spprich_uncertSD_GB.grd')
# bu2 <- filter_distance(bu[[2]],
#                        location=location,
#                        distance=distance,
#                        method = 'buffer')

ogd <- projectRaster(bu2, crs="+proj=longlat +datum=WGS84") %>% as.data.frame(xy=T)

ggmap(gmap) +
  geom_tile(data=ogd, aes(x=x,y=y,fill=spp_richness), alpha=0.7) +
  viridis::scale_fill_viridis(na.value = NA) #+
# geom_point(data=ndf,aes(x=X, y=Y), colour = 'red', cex = 0.5)




register_google(key = "_____")

gmap <- get_map(location = location, zoom = 12)

## original decide

ogd_p <- ggmap(gmap) +
  geom_tile(data = ogd, aes(x=x,y=y, fill = layer), alpha = 0.7) +
  coord_quickmap() +
  scale_fill_gradient(high = 'red', low = '#ffffcc', na.value = NA, name = 'DECIDE score') +
  labs(x= '', y = '')

## First lot of nudge metadata - the access layer the nudge is close to
# for all of them
metadata <- metadata_accessible(nudges_df = thinned_nudges$nudges,
                                access = final_acc_loc,
                                buffer = 200)
metadata

# for one of them
metadata_single <- metadata_accessible(nudges_df = thinned_nudges$nudges[3,],
                                       access = final_acc_loc,
                                       buffer = 200)
metadata_single
