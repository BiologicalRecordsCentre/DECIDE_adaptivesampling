########        Function for providing map for Richard       ########

####    Inputs

###' Location
###' Distance


#####    Currently only works for Wallingford (because of file transfer issues).

## input variables
location = c(-1.110557, 51.602436) # wallingford
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
source("Data/example_for_rich/functions/load_gridnumbers.R")
source("Data/example_for_rich/functions/filter_distance.R")
source("Data/example_for_rich/functions/recommend_rank.R")
source("Data/example_for_rich/functions/recommend_metric.R")
source("Data/example_for_rich/functions/recommend_agg_rank.R")
source("Data/example_for_rich/functions/extract_metric.R")

# load a UK grid - currently 10km
grid <- st_read('Data/example_for_rich/grids/uk_grid_10km.shp')
st_crs(grid) <- 27700

# find grid numbers that location + buffered zone covers
grid_numbers <- load_gridnumbers(location = location,
                                 distance = distance,
                                 grid = grid)


## species data
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
prow_loc <- 'Data/example_for_rich/grids/prow_grid/'
grnspc_loc <- 'Data/example_for_rich/grids/greenspace_grid'
accspnt_loc <- 'Data/example_for_rich/grids/accesspoint_grid'
access_land_loc <- 'Data/example_for_rich/grids/accessland_grid'


# load the accessible areas
system.time(
  acc_loc <- lapply(c(1:length(grid_numbers)), FUN = function(n){
    
    # prow .shp
    prow_files <- list.files(prow_loc,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    prow <- sf::st_read(prow_files, quiet = TRUE)
    st_crs(prow) <- 27700
    
    # greenspaces .shp
    grnspc_files <- list.files(grnspc_loc,
                               full.names = T,
                               pattern = paste0('_', grid_numbers[n], '.shp'))
    
    grnspc <- sf::st_read(grnspc_files, quiet = TRUE)
    st_crs(grnspc) <- 27700
    
    # access points .shp
    accs_files <- list.files(accspnt_loc,
                             full.names = T,
                             pattern = paste0('_', grid_numbers[n], '.shp'))
    
    accspnt <- sf::st_read(accs_files, quiet = TRUE)
    st_crs(accspnt) <- 27700
    
    # access land .shp
    accslnd_files <- list.files(access_land_loc,
                                full.names = T,
                                pattern = paste0('_', grid_numbers[n], '.shp'))
    
    accslnd <- sf::st_read(accslnd_files, quiet = TRUE)
    st_crs(accslnd) <- 27700
    
    return(list(prow, grnspc, accspnt, accslnd))
    
  })
)

all_outs <- do.call(Map, c(rbind, acc_loc))

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


{
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
  
  # footpaths
  if (!is.null(access_metrics[[1]])) {
    base_plot <- base_plot +
      geom_sf(data = access_metrics[[1]], aes(col = error_metric), show.legend = F, size = 0.8) +
      coord_sf(datum = sf::st_crs(27700))
  }
  
  # greenspaces
  if (!is.null(access_metrics[[2]])) {
    base_plot <- base_plot +
      geom_sf(data = access_metrics[[2]], aes(fill = error_metric)) +
      coord_sf(datum = sf::st_crs(27700))
  }
  
  # Open access areas
  if (!is.null(access_metrics[[4]])) {
    base_plot <- base_plot +
      geom_sf(data = access_metrics[[4]], aes(fill = error_metric)) +
      coord_sf(datum = sf::st_crs(27700))
  }
  
  base_plot
}

# ggsave(base_plot, filename = 'outputs/Score_example_wallingford_5k.tif',
#        device = 'tiff', dpi = 300)
