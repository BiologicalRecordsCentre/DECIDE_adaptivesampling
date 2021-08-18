## Get metadata associated with the access for each nudge

## INPUT: 
# the thinned nudges
# access layers
# buffer - same as the one used for nudge_accessible
# metadata_col_names - names of the columns or objects that we want returned for each of the accessible layers
# if we add more layers or change any, this list will need to be updated

metadata_accessible <- function(nudges_df,
                                access,
                                buffer = 200,
                                metadata_col_names = c('ROW_TYPE', 'function_', 'Descrip',
                                                       'accessType', 'Name', 'fclass')) { # names need to be manually entered into here
  
  # remove NULL items in list
  access <- access[lengths(access) != 0]  
  
  # change access layer name to 'open access' for Rich
  for(x in 1:length(access)){ if(any(colnames(access[[x]]) %in% 'Descrip')) {access[[x]]$Descrip <- 'Access land' }}
  
  # Go throught each features layer in turn...
  l_feat <- lapply(access, function(x){
    
    # Which features are close to each nudge
    ids <- st_is_within_distance(nudges_df,
                                 x,
                                 dist = buffer,
                                 sparse = FALSE)  
    
    # Find the features
    # For each nudge, return the details for the matching features
    # Returns a concatenation of the feature types for each nudge
    feat_by_layer <- apply(ids, MARGIN = 1, access = x,
                           cols = metadata_col_names,
                           FUN = function(y, access, cols){
                             temp <- as.data.frame(
                               access[y, cols[cols %in% colnames(access)]]
                             )[,1] 
                             paste(temp, collapse = ',')
                           })
  })
  
  # Bring the data from the different layers together
  feats <- apply(do.call(cbind, l_feat),
                 MARGIN = 1,
                 FUN = function(x){
                   paste(x[x!=''], collapse = ',')
                 })
  
  # Add the XY data to this
  return(cbind(feats, st_coordinates(nudges_df)))
  
}
