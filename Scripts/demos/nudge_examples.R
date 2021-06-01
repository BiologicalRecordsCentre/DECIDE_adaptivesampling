

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
### ---   new function
list_nudges <- function(decide_rast,
                        prop = 0.1, # proportion of total number of cells to suggest as nudges
                        cutoff = FALSE, # whether or not to cut off all of the values below the cutoff_value
                        cutoff_value = 0.9, # anywhere between 0-1; quantile to select nudges from
                        weight = TRUE, # whether or not to weight the nudges returned by decide score - not a very powerful effect - how to increase?
                        weight_inflation = 10) # how much to inflate the decide score weighting
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

# function to thin nudges
thin_nudges <- function(decide_raster, # the original raster layer
                        nudge_df, # the data frame of nudges to thin, can be in any form as long as it has a 'lon' and 'lat' column
                        lon = 'lon', # the name of the longitude column in the data frame
                        lat = 'lat',  # the name of the latitude column in the data frame
                        crs = st_crs(decide_raster), # the coordinates system of the base raster - used in making the grid
                        buffer_distance, # the resolution of the grid to sample from - on the scale of the raster
                        sample_num,
                        plot = FALSE,
                        square = TRUE){
  
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
  
  
  p <- ggplot() +
    geom_sf(data = conv_rast(decide_raster, crs), aes(fill = layer, col = layer)) +
    geom_sf(data = spat_grd, fill = NA, col = 'yellow') +
    geom_point(data = nudge_df, aes(x=lon, y=lat), pch = 20, col = 'red') +
    geom_sf(data = do.call('rbind', l_out), pch = 1, fill = NA, col = 'black')+
    theme_bw() +
    labs(x='Longtitude', y='Latitude') +
    scale_fill_continuous(type = 'viridis', name = 'layer value') +
    scale_colour_continuous(type = 'viridis', name = 'layer value')
  
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





for(i in 1:length(grd)[1]){
  
  t <-  st_intersection(nudge_spat, grd[i,])
  if(dim(t)[1]>0){
    print(i)
  }
  
}

ggplot() +
  geom_sf(data = conv_rast(wall_m, 27700), aes(fill = layer, col = layer)) +
  geom_sf(data = nudge_spat, pch = 20, col = 'red') +
  geom_sf(data = grd, fill = NA, col = 'yellow')

plot((conv_rast(wall_m, 27700)))
plot(st_geometry(nudge_spat), add = T)
plot(st_geometry(grd), add = T)


df <- nudge_subset %>% mutate(k = 1,
                              point_id = paste(lon, lat, sep='_'))


t <- df %>% 
  full_join(df, by = "k") %>% 
  filter(paste(lon.x, lat.x) != paste(lon.y,lat.y)) %>% 
  mutate(distance = sqrt((lon.x - lon.y)^2 + (lat.x - lat.y)^2),
         close = ifelse(distance <= buffer_distance, 0, 1)) %>% 
  group_by(lon.x,lat.x) %>% 
  summarise(keep = ifelse(any(close == 0), 0, 1)) %>% 
  ungroup() %>% 
  filter(keep == 1)


ggplot() +
  geom_point(data=df, aes(x=lon,y=lat)) +
  geom_point(data=t, aes(x=lon.x,y=lat.x), colour = 'red')



t2 <- df %>% 
  full_join(df, by = "k") %>% 
  filter(paste(lon.x, lat.x) != paste(lon.y,lat.y)) %>% 
  mutate(distance = sqrt((lon.x - lon.y)^2 + (lat.x - lat.y)^2),
         close = ifelse(distance <= buffer_distance, 0, 1))

for(i in 1:length(t2$point_id.x)){
  
  dfi <- t2[t2$point_id.x == t2$point_id.x[i],]
  
  if(any(dfi$distance <= buffer_distance)){
    
    df_within_distance <- dfi[dfi$distance <= buffer_distance,]
    sample_ind <- sample(1:dim(df_within_distance)[1], 1)
    
    sub_sample <- df_within_distance[sample_ind,] %>% 
      select(lon = lon.x,
             lat = lat.x,
             dec_score = dec_score.x)
    
  } else {
    
    sub_sample <- data.frame(lon = unique(lon.x),
                             lat = unique(lat.x),
                             dec_score = unique(dec_score.x))
  }
  
  return(sub_sample)
  
}


buffer_distance=4000

### i give up here

d <- lapply(1:length(unique(t2$point_id.x)), FUN = function(x){
  
  dfi <- t2[t2$point_id.x == unique(t2$point_id.x)[x],]
  
  if(any(dfi$distance <= buffer_distance)){
    
    df_within_distance <- dfi[dfi$distance <= buffer_distance,]
    sample_ind <- sample(1:dim(df_within_distance)[1], 1)
    
    sub_sample <- df_within_distance[sample_ind,] %>% 
      select(lon = lon.y,
             lat = lat.y,
             dec_score = dec_score.x)
    
  } else {
    
    sub_sample <- data.frame(lon = unique(dfi$lon.x),
                             lat = unique(dfi$lat.x),
                             dec_score = unique(dfi$dec_score.x))
  }
  
  return(sub_sample)
  
  
})

dd <- do.call(rbind,d)

ggplot() +
  geom_point(data=df, aes(x=lon,y=lat)) +
  geom_point(data=dd, aes(x=lon,y=lat), colour = 'red')




t %>% group_by(value) %>% 
  summarise(keep = unique(close)) %>% 
  separate(value, into = c('lon', 'lat'))


points_matrix <- as.matrix(dist(cbind(df$lon, df$lat)))
points_matrix[points_matrix <= buffer_distance] <- NA
points_matrix[lower.tri(points_matrix, diag=TRUE)] <- NA
points_matrix
v <- colSums(points_matrix, na.rm=TRUE) > 0

df_p <- df[v,]


df2 <- df %>% 
  full_join(df, by = "k") %>% 
  mutate(dist = sqrt((lon.x - lon.y)^2 + (lat.x - lat.y)^2)) %>%
  select(-k)

head(df2)

points_matrix <- rgeos::gWithinDistance(data.frame(df$lon, df$lat), dist = 1000, byid = TRUE)
points_matrix[lower.tri(points_matrix, diag=TRUE)] <- NA
points_matrix
#    1     2     3     4     5
# 1 NA FALSE FALSE FALSE FALSE
# 2 NA    NA FALSE FALSE FALSE
# 3 NA    NA    NA FALSE FALSE
# 4 NA    NA    NA    NA  TRUE
# 5 NA    NA    NA    NA    NA

colSums(points_matrix, na.rm=TRUE) == 0
#    1     2     3     4     5 
# TRUE  TRUE  TRUE  TRUE FALSE 
v <- colSums(points_matrix, na.rm=TRUE) == 0
points[v, ]


########################         END OF TESTING STILL FAILING             ############################


## function to remove all the points within X distance of other points


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
