
### extract values from a raster stack or raster from a single point or buffer
# if buffer, return both central and buffered points
# rast_obj <- raster::stack(paste0('Data/metadata/moth_recs_spprich_uncert_GB.grd'))


metadata_model_info <- function(rast_obj,  
                                location, # location of the cell of interest
                                crds_loc = 4326, # coords of the location
                                crds_rast = 27700,
                                buffer_distance = 200,
                                rounding = -2) # to match the resolution of the raster (-2 for 100m)
{ 
  
  # first need to convert long lat to BNG
  dat_sf <- st_sf(st_sfc(st_point(location)), crs = crds_loc) # load location points, convert to spatial lat/lon
  trans_loc <- st_transform(dat_sf, crs = crds_rast) # transform to BNG
  central_point <- round(st_coordinates(trans_loc), rounding) # get the coordinates of the central point
  
  if(!is.null(buffer_distance)) trans_loc <- st_buffer(trans_loc, buffer_distance) # create a buffer around the point if desired
  
  # get the coordinates associated with the location+buffer
  crds_trans_loc <- st_coordinates(trans_loc)
  
  # extract the values from each layer
  extracted_vals <- raster::extract(x=rast_obj, y=matrix(rbind(central_point, unique(round(crds_trans_loc[,c('X', 'Y')], rounding))), ncol = 2), 
                                    method = 'simple',
                                    df = TRUE)[,-1] # remove the id column
  
  # bind back to the original coordinates
  if(is.null(buffer_distance)) {
    
    # because bound the central point to the original call to the extract function
    # need to then get unique because the information for that single central 
    # point is duplicated twice
    main_loc <- unique(cbind(lon = unique(round(crds_trans_loc[,c('X', 'Y')], rounding))[1],
                             lat = unique(round(crds_trans_loc[,c('X', 'Y')], rounding))[2],
                             extracted_vals))
    
    buffered_area <- NULL
    
  } else if(!is.null(buffer_distance)) {
    
    buffered_area <- cbind(lon = unique(round(crds_trans_loc[,c('X', 'Y')], rounding))[,1],
                           lat = unique(round(crds_trans_loc[,c('X', 'Y')], rounding))[,2],
                           extracted_vals[-1,])
    
    main_loc <- cbind(central_point,extracted_vals[1,])
    
    
  }
  
  return(list(central_loc = main_loc, buffered_area = buffered_area))
  
  
}

