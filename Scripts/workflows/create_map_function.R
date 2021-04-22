########        Function for providing map for Richard       ########

####    Inputs

###' Location
###' Distance


#####    Currently only works for Wallingford (because of file transfer issues).

## input variables
location = c(-1.110557, 51.602436) # wallingford
# location = c(-3.271289, 55.905648) # edinburgh
# location = c(-2.626592, 55.268517) # scottish/english borders
# location = c(-3.626351, 55.083935) # Dumfries
# location = c(-3.403110, 52.098200) # Powys Wales
# location = c(-0.373791, 51.359252) # claremont park london region
# location = c(-2.785436, 54.012698) # lancaster
# location = c(-2.730696, 54.026759) # quernmore
distance = 5000


#### Start of overall wrapper function


## setup
# load packages
require(raster) ## try to replace with terra
require(tidyverse)
require(viridis)
require(sf)
require(parallel)

# source functions
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/load_gridnumbers.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/filter_distance.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_rank.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_metric.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_agg_rank.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/extract_metric.R")


# # for rich
# source("Data/example_for_rich/functions/load_gridnumbers.R")
# source("Data/example_for_rich/functions/filter_distance.R")
# source("Data/example_for_rich/functions/recommend_rank.R")
# source("Data/example_for_rich/functions/recommend_metric.R")
# source("Data/example_for_rich/functions/recommend_agg_rank.R")
# source("Data/example_for_rich/functions/extract_metric.R")


# load a UK grid - currently 10km
grid <- st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/UK_grids/uk_grid_10km.shp')

# # for rich
# grid <- st_read('Data/example_for_rich/grids/uk_grid_10km.shp')

st_crs(grid) <- 27700

# find grid numbers that location + buffered zone covers
grid_numbers <- load_gridnumbers(location = location,
                                 distance = distance,
                                 grid = grid)


## species data
# this is currently coded for Rich - will need to see how/where data are stored
# before I can code this section up properly

# species names
names <- unique(gsub(pattern = "rf_SDMs_|_meanpred.grd|_meanpred.gri", 
                     replacement = '',
                     x = list.files('Data/example_for_rich/rf_models/', pattern = 'meanpred')))
names

# predicted probability distributions
preds <- raster::stack(list.files('Data/example_for_rich/rf_models/', 
                                  pattern = 'meanpred.grd',
                                  full.names = TRUE))
names(preds) <- names

# variation layers
var <- raster::stack(list.files('Data/example_for_rich/rf_models/', 
                                pattern = 'quantilerange.grd',
                                full.names = TRUE))
names(var) <- names


# crop species data
crop_pred <- filter_distance(obj = preds,
                             location = location,
                             distance = distance,
                             method = 'buffer')

crop_var <- filter_distance(obj = var,
                            location = location,
                            distance = distance,
                            method = 'buffer')


# create the decide score
additive_score <- recommend_metric(prediction_raster = crop_pred,
                                   error_raster = crop_var,
                                   method = 'additive')$additive

# Get the metric aggregated across all specyes
aggregate_score <- recommend_rank(predict_err_raster = additive_score,
                                  method = 'additive')$error_metric

# covert raster to an sf object for use with later functions
aggregate_score_converted <- conv_rast(raster = aggregate_score,
                                       coord = 27700) # British National Grid

## shape data
# point to shape locations
# england
prow_loc <- ("/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/rowmaps_footpathbridleway/rowmaps_footpathbridleway/gridded_data_10km/")
grnspc_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_greenspace_data_10km/"
accspnt_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_accesspoint_data_10km/"
access_land_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/CRoW_Act_2000_-_Access_Layer_(England)-shp/gridded_data_10km/"
nat_trust_loc <- '/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/national_trust/gridded_data_10km/'
lond_path_loc <- '/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/greater-london-latest-free/london_gridded_data_10km/'


# scotland
core_paths <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/core_paths/gridded_data_10km/"
pub_acc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/public_access_wiat/gridded_data_10km/"
cairn <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/cairngorms/gridded_data_10km/"
tross <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/lochlomond_tross/gridded_data_10km/"
cons_sites <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/local_nature_conservation_sites/gridded_data_10km/"
nat_sites <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/local_nature_reserves/gridded_data_10km/"
wild <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/Scotland/wildland_scotland/gridded_data_10km/"


# # for rich
# prow_loc <- 'Data/example_for_rich/grids/prow_grid/'
# grnspc_loc <- 'Data/example_for_rich/grids/greenspace_grid'
# accspnt_loc <- 'Data/example_for_rich/grids/accesspoint_grid'
# access_land_loc <- 'Data/example_for_rich/grids/accessland_grid'


# load the accessible areas
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
    
    
    ####    SCOTLAND    ####
    
    # core paths
    cp_files <- list.files(core_paths,
                           full.names = T,
                           pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(cp_files) != 0){
      c_paths <- sf::st_read(cp_files, quiet = TRUE)
      st_crs(c_paths) <- 27700
    } else { c_paths <- NULL }
    
    
    # public access
    # paths
    pacc_path_files <- list.files(pub_acc,
                                  full.names = T,
                                  pattern = paste0('paths_gridnumber_', grid_numbers[n], '.shp'))
    
    if(length(pacc_path_files) != 0){
      pacc_paths <- sf::st_read(pacc_path_files, quiet = TRUE)
      st_crs(pacc_paths) <- 27700
    } else { pacc_paths <- NULL }
    
    # woods
    pacc_wood_files <- list.files(pub_acc,
                                  full.names = T,
                                  pattern = paste0('wood_gridnumber_', grid_numbers[n], '.shp'))
    
    if(length(pacc_wood_files) != 0){
      pacc_wood <- sf::st_read(pacc_wood_files, quiet = TRUE)
      st_crs(pacc_wood) <- 27700
    } else { pacc_wood <- NULL }
    
    
    ## national parks
    # cairngorm
    cairn_files <- list.files(cairn,
                              full.names = T,
                              pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(cairn_files) != 0){
      cairn_shp <- sf::st_read(cairn_files, quiet = TRUE)
      st_crs(cairn_shp) <- 27700
    } else { cairn_shp <- NULL }
    
    
    # trossacks
    tross_files <- list.files(tross,
                              full.names = T,
                              pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(tross_files) != 0){
      tross_shp <- sf::st_read(tross_files, quiet = TRUE)
      st_crs(tross_shp) <- 27700
    } else { tross_shp <- NULL }
    
    
    # conservation sites
    cons_files <- list.files(cons_sites,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(cons_files) != 0){
      cons_shp <- sf::st_read(cons_files, quiet = TRUE)
      st_crs(cons_shp) <- 27700
    } else { cons_shp <- NULL }
    
    
    # natural conservation sites
    nat_files <- list.files(nat_sites,
                            full.names = T,
                            pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(nat_files) != 0){
      nat_shp <- sf::st_read(nat_files, quiet = TRUE)
      st_crs(nat_shp) <- 27700
    } else { nat_shp <- NULL }
    
    
    # wildland sites
    wild_files <- list.files(wild,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    if(length(wild_files) != 0){
      wild_shp <- sf::st_read(wild_files, quiet = TRUE)
      st_crs(wild_shp) <- 27700
    } else { wild_shp <- NULL }
    
    return(list(prow, grnspc, accspnt, accslnd, national_trust, london_paths, ## england
                c_paths, pacc_paths, pacc_wood, cairn_shp, tross_shp, cons_shp, nat_shp, wild_shp)) ## scotland
    
  })
)

all_outs <- do.call(Map, c(rbind, acc_loc))

# all_outs <- all_outs[lengths(all_outs) != 0]

# crop shapes to exact region of interest
final_acc_loc <- lapply(all_outs, filter_distance,
                        location = location,
                        distance = distance,
                        method = 'buffer')

## overlay species and shape data
# extract metric into shapes
system.time(
  access_metrics <- mclapply(X = final_acc_loc, 
                             FUN = extract_metric, 
                             mc.cores = 6,
                             metric = aggregate_score_converted))

## plot the final image
# This is conditional on which layers are available
# as, for some regions for example, there isn't any access land 


## try and do an automated plot regardless of how many layers there are

## first part of the plot will ALLWAYS be the same
base_plot <- ggplot() +
  geom_sf(data = aggregate_score_converted, aes(fill = error_metric), alpha = 0.5, colour = 'white', lwd = 0) +
  xlab('') + ylab('') +
  coord_sf(datum = sf::st_crs(27700)) +
  scale_fill_viridis(option = 'D',  na.value = "transparent",
                     name = 'DECIDE Score') +
  scale_colour_viridis(option = 'D',  na.value = "transparent",
                       name = 'DECIDE Score') +
  theme_bw() +
  theme(text = element_text(size = 15))

for(pl in 1:length(access_metrics)){
  
  print(pl)
  
  if (is.null(access_metrics[[pl]])) {
    
    next
    
  } else if (unique(st_geometry_type(access_metrics[[pl]])) == 'POLYGON' || 
             unique(st_geometry_type(access_metrics[[pl]])) == 'MULTIPOLYGON') {
    
    base_plot <- base_plot +
      geom_sf(data = access_metrics[[pl]], aes(fill = error_metric, colour = error_metric), show.legend = F, size = 0.8) +
      coord_sf(datum = sf::st_crs(27700))
    
  } else if (any(unique(st_geometry_type(access_metrics[[pl]])) == 'LINESTRING') |
             any(unique(st_geometry_type(access_metrics[[pl]])) == 'MULTILINESTRING')) {
    
    base_plot <- base_plot +
      geom_sf(data = access_metrics[[pl]], aes(colour = error_metric), show.legend = F, size = 0.8) +
      coord_sf(datum = sf::st_crs(27700))
    
  } else {
    
    print("Different object type than one that's been coded for here")
    
  }
}

base_plot


# {
#   base_plot <- ggplot() +
#     geom_sf(data = aggregate_score_converted, aes(fill = error_metric), alpha = 0.5, colour = 'white', lwd = 0) +
#     xlab('') + ylab('') +
#     coord_sf(datum = sf::st_crs(27700)) +
#     scale_fill_viridis(option = 'D',  na.value = "transparent",
#                        name = 'DECIDE Score') +
#     scale_colour_viridis(option = 'D',  na.value = "transparent",
#                          name = 'DECIDE Score') +
#     theme_bw() +
#     theme(text = element_text(size = 15))
#   
#   # footpaths
#   if (!is.null(access_metrics[[1]])) {
#     base_plot <- base_plot +
#       geom_sf(data = access_metrics[[1]], aes(col = error_metric), show.legend = F, size = 0.8) +
#       coord_sf(datum = sf::st_crs(27700))
#   }
#   
#   # greenspaces
#   if (!is.null(access_metrics[[2]])) {
#     base_plot <- base_plot +
#       geom_sf(data = access_metrics[[2]], aes(fill = error_metric)) +
#       coord_sf(datum = sf::st_crs(27700))
#   }
#   
#   # Open access areas
#   if (!is.null(access_metrics[[4]])) {
#     base_plot <- base_plot +
#       geom_sf(data = access_metrics[[4]], aes(fill = error_metric)) +
#       coord_sf(datum = sf::st_crs(27700))
#   }
#   
#   base_plot
# }

# ggsave(base_plot, filename = 'outputs/Score_example_wallingford_5k.tif',
#        device = 'tiff', dpi = 300)

