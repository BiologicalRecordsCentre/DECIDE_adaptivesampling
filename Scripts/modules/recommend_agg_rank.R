# Get aggregated rank across species
# only works on recommend_aggregate output with method = 'species_rank'
# 
# _Input_: Output of function recommend_rank() with method = 'species_rank'
#
# _Output_: Summed rank of cells across all species
# 
# would it be useful to be able to do functions other than 'sum' to compare across species?


##' Need to work out how this will work with multiple models

recommend_agg_rank <- function(rast) {
  
  rst_nms <- names(rast)
  
  ranks <- lapply(rst_nms, FUN = function(x){rast[[x]][[2]]}) # needs to be the second layer in the raster because the first is the metric
  
  return(calc(stack(ranks), fun = sum)) # could it be useful to use different functions here? I.e. standard deviation? does the standard deviation of the rank make any sense?
  
}



## Might be useful to have a function to stack the error metric/species rank output from the recommend_rank() function 
## Although probably won't be useful - can't think of a case currently.

stack_metric_rank <- function(rast,
                              var) { # var = metric, rank
  
  num <- ifelse(var == 'metric', 1, 
                ifelse(var == 'rank', 2, 
                       stop("'var must be one of 'metric' or 'rank''")))
  
  rst_nms <- names(rast)
  
  ranks <- stack(lapply(rst_nms, FUN = function(x){rast[[x]][[num]]})) # needs to be the second layer in the raster because the first is the metric
  
  names(ranks) <- rst_nms
  
  return(ranks) # could it be useful to use different functions here? I.e. standard deviation? does the standard deviation of the rank make any sense?
  
}