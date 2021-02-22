# Combine the predictions and errors to generate a 'metric' of uncertainty
# 
# _Input_: Rasterstack predictions for each species; Rasterstack error for each species
# 
# _Output_: A list with entries for each method chosen
# 
# _Metadata_: ???


##' A function to combine the predictions and errors
##' So far, only have two metrics: additive and multiply
##' Need to add more (better) metrics to these in the future
##' Maybe need to only return one metric, rather than the possibility of multiple
##' Need to work out how this will work with multiple models

recommend_metric <- function(prediction_raster, 
                             error_raster, 
                             method = c('multiply', 'additive')){
  
  if(!all(method %in% c('multiply', 'additive'))){
    
    stop("'methods are limited to:'multiply', 'additive'")
    
  }
  
  # multiplication metric
  if('multiply' %in% method) {
    mult_err <- prediction_raster*error_raster
  } else {
    mult_err <- NULL
  }
  
  # additive metric
  if('additive' %in% method) {
    sum_err <- prediction_raster+error_raster
  } else {
    sum_err <- NULL
  }
  
  return(list(multiply = mult_err,
              additive = sum_err))
  
}
