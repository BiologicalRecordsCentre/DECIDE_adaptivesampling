## convert raster to an sf object

conv_rast <- function(raster, # raster with a KNOWN COORDINATES SYSTEM
                      coord){ # the coordinates system!
  
  require(stars)
  
  stars_obj <- st_as_stars(raster)
  sf_obj <- st_as_sf(stars_obj, as_points = FALSE, merge = FALSE)
  st_crs(sf_obj) <- coord
  
  return(sf_obj)
  
}
