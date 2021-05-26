

### dummy nudges
library(raster)
library(sf)

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

## cutoff 
### ---   FUNCTIONALISE
cutoffs = c(0.9) # anywhere between 0-1

wall_m_df <- as.data.frame(wall_m,
                           xy = T)
names(wall_m_df) <- c('lon', 'lat', 'dec_score')

# quantile
qs <- quantile(wall_m_df$dec_score, probs = cutoffs, na.rm = T)
qs

# get values above cutoff
wmdf <- wall_m_df %>% 
  mutate(keep = ifelse(dec_score > qs, 1,0)) %>% 
  na.omit()
head(wmdf)

### ---   end of function


# create nudge list
### ---   new function
nudge_list <- function(decide_rast,
                       prop = 0.1,
                       cutoff_value = 0.9, # anywhere between 0-1; quantile to select nudges from
                       cutoff = FALSE, # whether or not to cut off all of the values below the cutoff_value
                       weight = TRUE, # whether or not to weight the nudges returned by decide score
                       scale_weights = TRUE)
{
  
  decide_df <- as.data.frame(decide_rast,
                             xy = T)
  names(decide_df) <- c('lon', 'lat', 'dec_score')
  
  # quantile
  qs <- quantile(decide_df$dec_score, probs = cutoff_value, na.rm = T)
  
  # get values above cutoff
  # currently only works for one cutoff value
  decide_df <- decide_df %>% 
    mutate(keep = ifelse(dec_score > qs, 1,0)) %>% 
    na.omit()
  
  # if cutoff, only display nudges > cutoff
  # if cutoff=F, keep values below cutoff
  if(cutoff == T){
    
    high_decide <- decide_df[decide_df$keep==1,]
    
  } else if(cutoff == F){ 
    
    high_decide <- decide_df
    
  }
  
  if(weight == TRUE) {
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = dim(high_decide)[1]*prop, 
                        prob= ifelse(rep(scale_weights==TRUE, length(high_decide$dec_score)), # ifelse returns vector same length as 'test'
                                     (high_decide$dec_score-min(high_decide$dec_score))/(max(high_decide$dec_score)-min(high_decide$dec_score)), 
                                     high_decide$dec_score))
    
  } else if(weight == FALSE){
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = dim(high_decide)[1]*prop)
    
  }
  
  nudges <- high_dec[nudge_ind,]
  return(nudges)
  
}

nudges <- nudge_list(wall_m)
head(nudges)

# convert raster
wall_m_sf <- conv_rast(wall_m, 27700)


# plot
p <- ggplot() +
  geom_sf(data = wall_m_sf, aes(fill=layer), col = NA) 

p <- p +  geom_point(data = nudges, aes(x=lon, y=lat), colour = 'pink', cex = 0.7)

p
