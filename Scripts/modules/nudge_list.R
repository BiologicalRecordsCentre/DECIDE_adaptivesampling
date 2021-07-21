# Get nudges from raster layer of decide scores based on the proportion of 
# the total number of cells that you want the nudge 'shortlist' to start with

# INPUT:
# prop # proportion of total number of cells to suggest as nudges
# cutoff # whether or not to cut off all of the values below the cutoff_value
# cutoff_value # anywhere between 0-1; quantile to select nudges from
# weight = TRUE # whether or not to weight the nudges returned by decide score
# weight_inflation # how much to inflate the decide score weighting - will change how many 'bad' places will get selected


nudge_list <- function(decide_rast,
                       n = NULL, # number of nudges to select. If NULL function chooses a proportion instead
                       prop = 0.1, # proportion of total number of cells to suggest as nudges
                       cutoff = FALSE, # whether or not to cut off all of the values below the cutoff_value
                       cutoff_value = 0.9, # anywhere between 0-1; this is the quantile to select nudges from (i.e. above this quantile)
                       weight = TRUE, # whether or not to weight the nudges returned by decide score
                       weighting_column = 'weights', # the name of the column to hold the weights (which is the z in the raster layer)
                       weight_inflation = 10) # how much to inflate the decide score weighting - will change how many 'bad' places will get selected
{
  
  require(tdigest)  
  require(raster)
  require(tidyverse)
  
  if(!is.character(weighting_column)) stop("'weighting_column' must be a character")
  
  decide_df <- as.data.frame(decide_rast,
                             xy = T)
  names(decide_df) <- c('lon', 'lat', weighting_column)
  
  # quantile
  qs <- tquantile(tdigest(decide_df[, weighting_column]), probs = cutoff_value) #, na.rm = T)
  
  # get values above cutoff
  # currently only works for one cutoff value
  decide_df$above_cutoff <- ifelse(decide_df[, weighting_column] >= qs, 1,0)
  
  # remove NAs
  decide_df <- na.omit(decide_df)
  
  # if cutoff, only display nudges > cutoff
  # if cutoff=F, keep values below cutoff
  if(cutoff == TRUE){
    
    high_decide <- decide_df[decide_df$above_cutoff==1,]
    print('!   Only selecting nudges from above the cutoff')
    
  } else if(cutoff == FALSE){ 
    
    high_decide <- decide_df
    print('!   Selecting nudges from above and below cutoff')
    
  }
  
  if(weight == TRUE) {
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = ifelse(is.null(n), dim(high_decide)[1]*prop, n), 
                        prob = high_decide[, weighting_column]^weight_inflation)
    
  } else if(weight == FALSE){
    
    nudge_ind <- sample(1:dim(high_decide)[1], 
                        size = ifelse(is.null(n), dim(high_decide)[1]*prop, n))
    
  }
  
  nudges <- high_decide[nudge_ind,]
  return(nudges)
  
}
