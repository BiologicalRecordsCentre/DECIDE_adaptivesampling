

### dummy nudges
library(raster)
library(sf)
library(tidyverse)
library(rgeos)

source("Scripts/modules/filter_distance.R")
source("Scripts/modules/convert_raster.R")
source("Scripts/modules/load_gridnumbers.R")

mgb <- raster('Data/species_data/decide_scores/moth_weighted_prob_pres_GB_decide_score.grd')
mgb

# set location 
# location = c(-2.730696, 54.026759) # quernmore
location = c(-1.110557, 51.602436) # wallingford
# location = c(-1.117329, 53.947566) # york

# distances
distance = 5000

# crop
wall_m <- filter_distance(mgb,
                          location=location,
                          distance=distance,
                          method = 'buffer') 

# get a list of potential nudge locations
nudges <- nudge_list(wall_m,
                     prop = 0.1, 
                     cutoff_value = 0.9, 
                     cutoff = FALSE,
                     weight = TRUE,
                     weight_inflation = 50)
head(nudges)

# choose a subset of nudges on length 'n'
nudge_subset <- nudge_select(nudge_df = nudges, 
                             n = 50,
                             weight = TRUE,
                             weighting_column = 'dec_score',
                             weight_inflation = 10)
head(nudge_subset)


#####     Thinning the nudges     #####

thinned_points <- nudge_thin(decide_raster = wall_m,
                             nudge_df = nudge_subset,
                             lon = 'lon',
                             lat = 'lat',
                             crs = 27700, 
                             buffer_distance = 1000,
                             sample_num = 1)

thinned_points$plot


## function to choose points close to accessible features 

# first load the accessible areas

# load a UK grid - currently 10km
grid <- st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/UK_grids/uk_grid_10km.shp')
st_crs(grid) <- 27700

# find grid numbers that location + buffered zone covers
grid_numbers <- load_gridnumbers(location = location,
                                 distance = distance,
                                 grid = grid)

# shape data
# point to shape locations - move data to a common directory
{# england
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

# crop shapes to exact region of interest
final_acc_loc <- lapply(all_outs, filter_distance,
                        location = location,
                        distance = distance,
                        method = 'buffer')


t_acc <- final_acc_loc[[4]]

t_acc


### function
nudge_accessible <- function(nudges_df,
                             access_layers,
                             buffer = 100,
                             plot = FALSE){
  
  require(tidyverse)
  require(sf)
  
  # remove NULL objects from the list
  access_layers_sub <- access_layers[lengths(access_layers) != 0]
  
  if(class(access_layers_sub)[1]=='list'){
    
    ## add error code to check the class of each item in list
    
    # get each layer buffered
    shapes_list <- lapply(access_layers_sub, FUN = function(x) (st_union(st_buffer(x, buffer))))
    
    # combine the first two list objects; st_union only accepts two objects
    ## !! make this BETTER!! So hacky!! !! ##
    shapes <- st_union(shapes_list[[1]], shapes_list[[2]])
    
    # use a for loop to combine the other ones
    for(i in 3:length(shapes_list)){ shapes <- st_union(shapes, shapes_list[[i]]) }
    
  } else if(class(access_layers_sub)[1]=='sf'){
    
    shapes <- st_union(st_buffer(access_layers_sub, buffer))
    
  } else {
    
    stop('!! only works on lists of "sf" obects or single "sf" objects')
    
  }
  
  # find the points that fall within the buffered shapes
  int_ind <- st_within(nudges_df, shapes, sparse = FALSE)
  int_nudge <- nudges_df[int_ind,]
  
  if(dim(int_nudge)[1]==0){
    
    print('! No nudges within buffered region; returning NULL oobject. Consider increasing buffer size')
    
    return(NULL)
    
  }
  
  
  # # base plot
  # print(plot(st_geometry(shapes), border = 'blue'))
  # print(lapply(access_layers_sub, FUN = function(x) plot(st_geometry(x), col = 'green', border = 'green', add = T)))
  # print(plot(st_geometry(nudges_df), add = T, pch=20)) 
  # print(plot(st_geometry(int_nudge), col = 'red', add = T, pch = 20))
  
  # ggplot
  p <- ggplot() +
    geom_sf(data=shapes, aes(colour = 'Buffered accessible\nareas'), fill = NA) +
    theme_bw() 
  
  for(pls in 1:length(access_layers_sub)){
    p <- p + 
      geom_sf(data=access_layers_sub[[pls]], aes(colour = 'Accessible areas', fill = 'Accessible areas'))
  }
  
  p <- p +
    geom_sf(data = int_nudge, aes(colour = 'Nudges within\nbuffered area'), cex = 2) + 
    geom_sf(data = nudges_df, aes(colour = 'Original nudges'), cex = 1) +
    scale_colour_manual(name = '', values = c('green3', 'green4', 
                                              'red', 'black')) +
    scale_fill_manual(values = 'green3') +
    guides(fill = FALSE)
  
  if(plot == T) print(p)
  
  return(list(nudges = int_nudge,
              plot = p))
  
}

final_nudges <- nudge_accessible(nudges_df = thinned_points$nudges,
                                 access_layers = final_acc_loc,
                                 buffer = 300,
                                 plot = TRUE) 

final_nudges$plot
final_nudges$nudges
