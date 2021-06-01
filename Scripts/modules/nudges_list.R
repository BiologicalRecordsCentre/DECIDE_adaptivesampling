# Get nudges from raster layer of decide scores based on the proportion of 
# the total number of cells that you want the nudge 'shortlist' to start with

# INPUT:
# prop # proportion of total number of cells to suggest as nudges
# cutoff # whether or not to cut off all of the values below the cutoff_value
# cutoff_value # anywhere between 0-1; quantile to select nudges from
# weight = TRUE # whether or not to weight the nudges returned by decide score
# weight_inflation # how much to inflate the decide score weighting - will change how many 'bad' places will get selected


nudges_list <- function(decide_rast,
                        prop = 0.1, # proportion of total number of cells to suggest as nudges
                        cutoff = FALSE, # whether or not to cut off all of the values below the cutoff_value
                        cutoff_value = 0.9, # anywhere between 0-1; this is the quantile to select nudges from (i.e. above this quantile)
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
  if(cutoff == TRUE){
    
    high_decide <- decide_df[decide_df$above_cutoff==1,]
    
  } else if(cutoff == FALSE){ 
    
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