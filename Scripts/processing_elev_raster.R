# reprojecting raster data for DECIDE WP1 because my own computer was being stupid
# not important for DECIDE WP2

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
elev_proj
plot(elev_proj)

## crop to GB
# download map GB
uk_map <- st_as_sf(getData("GADM", country = "GBR", level = 1, path='Data/environmental_data'))
uk_map <- st_transform(uk_map, 27700)

# remove nrothern ireland
gb_map <- uk_map[uk_map$NAME_1 != 'Northern Ireland',]

# check
plot(st_geometry(gb_map))

# convert to spatial for use in raster::mask()
gb_mask <- as_Spatial(gb_map)
gb_mask

# mask elevation
m_gb <- raster::mask(elev_proj, gb_mask[1])

plot(m_gb, xlim = c(330000, 360000), ylim = c(440000, 470000))

writeRaster(m_gb, filename = 'Data/environmental_data/elevation_map_coasts_fixed_GB_100m.tiff')

