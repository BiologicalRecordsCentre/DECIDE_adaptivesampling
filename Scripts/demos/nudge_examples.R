

### dummy nudges
library(raster)
library(sf)
library(tidyverse)
library(rgeos)

source("Scripts/modules/filter_distance.R")
source("Scripts/modules/convert_raster.R")

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

# get a list of potential nudge locations above 0.9th quantile
nudges <- nudges_list(wall_m,
                      prop = 0.1, 
                      cutoff_value = 0.9, 
                      cutoff = FALSE,
                      weight = TRUE,
                      weight_inflation = 50)
head(nudges)

# choose a subset of nudges on length 'n'
nudge_subset <- nudge_select(nudge_df = nudges, 
                             n=50,
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


