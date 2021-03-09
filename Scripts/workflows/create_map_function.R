
########        Function for providing map        ########

####    Inputs

###' Location
###' Distance
###' Objects (?)
###' etc.... fill in later

## input variables
location = c(-1.110557, 51.602436) # wallingford
location = c(-2.775565, 54.041027) # lancaster
distance = 5000
grid = st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/UK_grids/uk_grid_25km.shp')



#### Start of overall wrapper function


## setup
# load packages
require(raster) ## try to replace with terra
require(tidyverse)
require(viridis)
require(sf)

# source functions
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/load_gridnumbers.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/filter_distance.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_rank.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_metric.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/recommend_agg_rank.R")
source("/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Scripts/modules/extract_metric.R")

# find grid numbers
grid_numbers <- load_gridnumbers(location = location,
                                distance = distance,
                                grid = grid)

## species data
# load species data

# crop species data

# create metric

# aggregate metric

## shape data
# point to shape locations
prow_loc <- ("/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/rowmaps_footpathbridleway/rowmaps_footpathbridleway/gridded_data")
grnspc_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_greenspace_data/"
accspnt_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/OS_greenspaces/OS Open Greenspace (ESRI Shape File) GB/data/gridded_accesspoint_data/"
access_land_loc <- "/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/CRoW_Act_2000_-_Access_Layer_(England)-shp/gridded_data/"

# load grid
uk_grid <- st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/UK_grids/uk_grid_25km.shp')
st_crs(uk_grid) <- 27700

# find grid of interest
grid_nums <- load_gridnums(location, distance, uk_grid)

# load accessible areas
system.time(
  acc_loc <- lapply(c(1:length(grid_nums)), FUN = function(n){
    
    # prow
    prow_files <- list.files(prow_loc,
                             full.names = T,
                             pattern = paste0(grid_nums[n], '.shp'))

    prow <- sf::st_read(prow_files, quiet = TRUE)
    st_crs(prow) <- 27700
    
    # greenspaces .shp
    grnspc_files <- list.files(grnspc_loc,
                               full.names = T,
                               pattern = paste0(grid_nums[n], '.shp'))

    grnspc <- sf::st_read(grnspc_files, quiet = TRUE)
    st_crs(grnspc) <- 27700
    
    # access points .shp
    accs_files <- list.files(accspnt_loc,
                             full.names = T,
                             pattern = paste0(grid_nums[n], '.shp'))

    accs <- sf::st_read(accs_files, quiet = TRUE)
    st_crs(accs) <- 27700
    
    # access land .shp
    accslnd_files <- list.files(access_land_loc,
                             full.names = T,
                             pattern = paste0(grid_nums[n], '.shp'))

    accs <- sf::st_read(accslnd_files, quiet = TRUE)
    st_crs(accs) <- 27700
    
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
