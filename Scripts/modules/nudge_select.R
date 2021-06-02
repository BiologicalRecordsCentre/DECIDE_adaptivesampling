## Return a random number of nudges length N
# from the output of list_function

# INPUT:
# nudge_df, output of list_nudges(),
# n, number of nudges to return
# weight, whether to weight the values returned based on the weighting column
# weight_values, the column name containing the weights
# weight_inflation, How much to inflate the weighting layer by

nudge_select <- function(nudge_df,
                         n = 15,
                         cutoff = TRUE,
                         weight = TRUE,
                         weighting_column,
                         weight_inflation = 10) {
  
  if(cutoff == TRUE){
    
    nudge_df_cut <- nudge_df[nudge_df$above_cutoff == 1,]
    print('!   Only selecting nudges from above the cutoff')
    
  } else if(cutoff == FALSE) {
    
    nudge_df_cut <- nudge_df
    print('!   Selecting nudges from above and below cutoff')
    
  }
  
  
  if(weight == TRUE){
    
    nudge_inds <- sample(1:dim(nudge_df_cut)[1], 
                         n,
                         prob = nudge_df_cut[, weighting_column]^weight_inflation)
    
  } else if(weight == FALSE){
    
    nudge_inds <- sample(1:dim(nudge_df_cut)[1], 
                         n)
    
    
  } else{ stop('!!!   Weights must either be TRUE or FALSE, more options will be implemented later   !!!')}
  
  nudge_outs <- nudge_df_cut[nudge_inds,]
  
  return(nudge_outs)
  
}
