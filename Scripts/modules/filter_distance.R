#  Weight/mask the adaptive sampling surface based on distance to a specific location (i.e. the users current location)
# 
# _Input_: Sampling layer(s); Location; Method
# 
# _Output_: Masked/rescaled adaptive sampling layer
# 
# _Metadata_: Method used
# 
library(sf)
library(raster)

# # old raster for whole of UK
# load('./data/sdm_output/example_05_11/Tyria jacobaeae_lr.rdata')
# 
# p <- (out$Predictions)
# raster = p
# 
# # because raster is currently lat/long, convert it to BNG outside of function (for now)
# crs(raster) <- CRS("+proj=longlat")
# raster <- projectRaster(raster, crs = CRS("+init=epsg:27700"), method = 'ngb')
# plot(raster)

## new raster for only subset of UK but with BNG and at 100m resolution
# load("/data/notebooks/rstudio-constraintlayers/Data/raw_data/subset_species_ranfor_29_01_21//SDMs_Archiearis parthenias.rdata")
# ap_se <- all_mods$rf$quantile_range
# plot(ap_se)


## function start
filter_distance <- function(obj, 
                            location = c(-1.110557, 51.602436), # has to be c(long, lat) as input
                            method = c('buffer', 'travel'),
                            distance = 20000){ # distance willing to go in metres
  
  require(sf)
  require(raster)
  
  if(method == 'buffer'){
    
    # first need to convert long lat to BNG
    dat_sf <- st_sf(st_sfc(st_point(location)), crs = 4326) # load location points, convert to spatial lat/lon
    trans_loc <- st_transform(dat_sf, crs = 27700) # transform to BNG
    buffed <- st_buffer(trans_loc, distance) # create a buffer around the point
    
    # # show where the buffered zone is
    # par(mfrow = c(1,2))
    # plot(raster)
    # plot(buffed, add = T)
    
    
    if (is.null(obj)){ # stop if object is NULL and return NULL
      
      warning('!!  Cropping object is empty, returning NULL output')
      
      c_buf <- NULL
      
    } else if(class(obj)[1] != 'sf' & 
              class(obj)[1] != 'RasterLayer' & 
              class(obj)[1] != 'RasterStack') {
      
      stop(paste('!!  object must be of class "Raster*" or "sf". Current class is:', class(obj)))
      
    } else if(class(obj)[1] == 'RasterLayer' | class(obj)[1] == 'RasterStack'){ # extract the masked extent for raster
      
      c_buf <- crop(obj, buffed) # crop the raster - creates a square extent 
      masked_area <- mask(c_buf, buffed)
      # plot(masked_area) # then mask it to get only the area within the 'travel distance'
      # par(mfrow = c(1,1))
      
      return(masked_area) # return only the masked region x distance from the 'location'
      
    } else if (class(obj)[1] == 'sf' && 
               unique(st_geometry_type(obj)) == 'LINESTRING'){
      c_buf <- st_intersection(obj, 
                               buffed) # crop the sf layer
      
      return(c_buf) # return only the masked region x distance from the 'location'
      
    } else if(class(obj)[1] == 'sf' && 
              (unique(st_geometry_type(obj)) == 'POLYGON' | 
               unique(st_geometry_type(obj)) == 'MULTIPOLYGON')){
      
      c_buf <- st_intersection(sf::st_buffer(obj, dist = 0), # this is a hack to get around shapes that self-intersect. Works for some reason?!
                               buffed) # crop the sf layer
    } 
    
  }
  
}


# 
# ap_merge <- st_read('/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/raw_data/rowmaps_footpathbridleway/rowmaps_footpathbridleway/ALL_PATHS_MERGED_long.shp')
# st_crs(ap_merge) <- 27700

# filter_distance(obj = obj,
#                 method = 'buffer',
#                 distance = 5000)


