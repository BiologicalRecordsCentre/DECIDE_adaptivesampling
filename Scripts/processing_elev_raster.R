

# unzip(zipfile = '/data/notebooks/rstudio-vizz/data/elevation/Copernicus_Elevation.zip', 
#       exdir = '/data/notebooks/rstudio-vizz/data/elevation/')

library(raster)

#####    Elevation data

# just a simple walkthrough of the elevation data that's available
# the files are pretty big though so they will need to be cropped before using.

# the elevation data are found here:
list.files('/data/notebooks/rstudio-vizz/data/elevation/Copernicus_Elevation/')

# this contains a 100m raster for the UK combined
uk_100m <- raster::stack('/data/notebooks/rstudio-vizz/data/elevation/Copernicus_Elevation/elevation_UK.tif')
uk_100m


# there are also rasters available at 25m
el1 <- raster("/data/notebooks/rstudio-vizz/data/elevation/Copernicus_Elevation/eu_dem_v11_E30N30/eu_dem_v11_E30N30.TIF")
el2 <- raster("/data/notebooks/rstudio-vizz/data/elevation/Copernicus_Elevation/eu_dem_v11_E30N40/eu_dem_v11_E30N40.TIF")

# merge the rasters together
elev_100 <- merge(el1, el2)

# remove iceland
ext_proj <- extent(matrix(c(3000000,3000000, 3900000, 4500000), ncol = 2))
elev_100_crop <- crop(elev_100, ext_proj) 
plot(elev_100_crop)


# landcover layer for reprojecting to:
lcm <- raster("/data/notebooks/rstudio-setupconsthomas/DECIDE_constraintlayers/Data/environmental_data/100mRastOneLayer.grd")

elev_proj <- raster::projectRaster(elev_100_crop, lcm)
