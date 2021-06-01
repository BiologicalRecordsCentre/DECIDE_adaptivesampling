

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


# create nudge list
list_nudges <- function(decide_rast,
                        prop = 0.1, # proportion of total number of cells to suggest as nudges
                        cutoff = FALSE, # whether or not to cut off all of the values below the cutoff_value
                        cutoff_value = 0.9, # anywhere between 0-1; quantile to select nudges from
                        weight = TRUE, # whether or not to weight the nudges returned by decide score
                        weight_inflation = 10) # how much to inflate the decide score weighting - will change how many 'bad' places will get selected
{
  
  require(raster)
  require(tidyverse)
  
  decide_df <- as.data.frame(decide_rast,
                             xy = T)
  names(decide_df) <- c('lon', 'lat', 'dec_score')
  
  # quantile
  qs <- quantile(decide_df$dec_score, probs = cutoff_value, na.rm = T)
  
  # get values above cutoff
  # currently only works for one cutoff value
  decide_df <- decide_df %>% 
    mutate(above_cutoff = ifelse(dec_score > qs, 1,0)) %>% 
    na.omit()
  
  # if cutoff, only display nudges > cutoff
  # if cutoff=F, keep values below cutoff
  if(cutoff == T){
    
    high_decide <- decide_df[decide_df$above_cutoff==1,]
    
  } else if(cutoff == F){ 
    
    high_decide <- decide_df
    
  }
  
  if(weight == TRUE) {
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = dim(high_decide)[1]*prop, 
                        prob = high_decide$dec_score^weight_inflation)
    
  } else if(weight == FALSE){
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = dim(high_decide)[1]*prop)
    
  }
  
  nudges <- high_decide[nudge_ind,]
  return(nudges)
  
}

nudges <- list_nudges(wall_m,
                      prop = 0.1, 
                      cutoff_value = 0.9, 
                      cutoff = FALSE,
                      weight = TRUE,
                      weight_inflation = 50)
head(nudges)

## function to return a random number of nudges length N
# from the proportion of total number of decide scroes function above
nudge_select <- function(nudge_df,
                         n = 15,
                         weight = TRUE,
                         weight_inflation = 50) {
  
  nudge_df <- nudge_df[nudge_df$above_cutoff == 1,]
  print('!   Only selecting nudges from above the cutoff')
  
  if(weight == TRUE){
    
    nudge_inds <- sample(1:dim(nudge_df)[1], 
                         n,
                         prob = nudge_df$dec_score^weight_inflation)
    
  } else if(weight == FALSE){
    
    nudge_inds <- sample(1:dim(nudge_df)[1], 
                         n)
    
    
  } else{ stop('!!!   Weights must either be TRUE or FALSE, more options will be implemented later   !!!')}
  
  nudge_outs <- nudge_df[nudge_inds,]
  
  return(nudge_outs)
  
}

nudge_subset <- nudge_select(nudge_df = nudges, n=50)
head(nudge_subset)


#####     Thinning the nudges     #####
## the spatial approach


## function to thin all the nudges based on a grid
thin_nudges <- function(decide_raster, # the original raster layer
                        nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
                        lon = 'lon', # the name of the longitude column in the data frame
                        lat = 'lat',  # the name of the latitude column in the data frame
                        crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
                        buffer_distance, # the resolution of the grid to sample from - on the scale of the raster
                        sample_num, # number of nudges to sample per grid
                        plot = FALSE, # whether or not to plot output
                        square = TRUE) # use a square or hexagonal grid
  {
  
  # create a grid to sample from
  spat_grd <- st_make_grid(conv_rast(decide_raster, crs), cellsize = buffer_distance, square = square)
  
  # convert nudges to sf points object
  spat_nudge <- st_as_sf(nudge_df, coords = c(lon, lat), crs = crs)
  
  # go through each grid cell, sample from all the points in the grid
  # return sample_num points per grid cell
  l_out <- lapply(1:length(spat_grd), FUN = function(x){
    
    # sample from a grid cell
    # only if grid contains a point
    if(dim(st_intersection(spat_nudge, spat_grd[x,]))[1]>0){
      
      # sample 'sample_num' points from the grid cell of interest
      # could also determine number of nudges based on the number of points in the cell
      # i.e. the proportion of points in that cell?
      sample_index <- sample(1:dim(st_intersection(spat_nudge, spat_grd[x,]))[1], 
                             size = ifelse(sample_num <= dim(st_intersection(spat_nudge, spat_grd[x,]))[1], # if the number of points asked for is less than the number in the cell then  
                                           sample_num, # return the number asked for
                                           dim(st_intersection(spat_nudge, spat_grd[x,]))[1])) # if more, then return all the points in the cell
      
      # return the sampled point within that grid
      return(st_intersection(spat_nudge, spat_grd[x,])[sample_index,])
    }
    
  })
  
  ### fix plotting
  p <- ggplot() +
    geom_sf(data = conv_rast(decide_raster, crs), aes(fill = layer), col = NA) +
    geom_sf(data = spat_grd, fill = NA, col = 'black') +
    geom_sf(data = do.call('rbind', l_out), aes(col = 'Thinned points'), pch = 20, size = 3, pch = 1)  +
    geom_point(data = nudge_df, aes(x=lon, y=lat, col = 'Original points'), pch = 20, size = 1.5) +
    theme_bw() +
    labs(y='Longtitude', x='Latitude') +
    scale_fill_continuous(type = 'viridis', name = 'Layer value') + 
    scale_colour_manual(name = '', values = c('red', 'yellow'))
  
  if(plot == TRUE){
    print(p)
  }
  
  return(list(nudges = do.call('rbind', l_out),
              grid = spat_grd,
              plot = p))
  
}


thinned_points <- thin_nudges(decide_raster = wall_m,
                              nudge_df = nudge_subset,
                              lon = 'lon',
                              lat = 'lat',
                              crs = 27700, 
                              buffer_distance = 1000,
                              sample_num = 1)

thinned_points$plot


## function to choose points close to accessible features 



####    Plotting
# convert raster
wall_m_sf <- conv_rast(wall_m, 27700)


# plot
p <- ggplot() +
  geom_sf(data = wall_m_sf, aes(fill=layer), col = NA) 

p <- p +  geom_point(data = nudges, aes(x=lon, y=lat, colour = 'no_cutoff'), cex = 0.7)

p <- p + geom_point(data = nudge_subset, aes(x=lon, y=lat, colour = 'cutoff'), cex = 0.7) +
  scale_colour_manual(values = c('green', 'red', 'orange')) +
  scale_fill_continuous(type = 'viridis') +
  theme_bw() +
  labs(x=NULL, y=NULL)

p +  geom_point(data = df_p, aes(x=lon, y=lat, colour = 'thinned'), cex = 0.7) 
