## counts for susan
# number of days of sighting of any species for each grid cell
# first get number of days of sighting

##  create a 'counts' raster layer
dfm <- read.csv('/data/notebooks/rstudio-adaptsampthomas/DECIDE_adaptivesampling/Data/species_data/moth/DayFlyingMoths_East_Norths.csv')
head(dfm)

# mean predictions
mp <- list.files(paste0('/data-s3/thoval/sdm_outputs/', taxa, '/', model[m]), 
                 pattern = "_meanpred.grd",
                 full.names = TRUE)
mps <- stack(mp[1])


## get the effort layer 1km
km1_eff <- dfm %>%
  group_by(lon,lat) %>%
  summarise(effort = length(unique(date))) %>%
  arrange(-effort)
km1_eff

r_df <- as.data.frame(mps, xy=T)[,1:2]

r_df$effort <- km1_eff$effort[match(paste0(r_df$x, r_df$y), paste0(km1_eff$lon, km1_eff$lat))]

hist(r_df$effort)

rast_eff <- rasterFromXYZ(r_df)

hist(rast_eff$effort)
plot(rast_eff)
rast_eff


## get the effort layer 1km
km10_eff <- dfm %>%
  mutate(lon = round(lon, -2),
         lat = round(lat, -2)) %>% 
  group_by(lon,lat) %>%
  summarise(effort = length(unique(date))) %>%
  arrange(-effort)
km10_eff

# or just aggregate 1km raster
rast_eff1000m <- aggregate(rast_eff, fact = 10, FUN = sum)
plot(rast_eff1000m)
rast_eff1000m ## INCORRECT

mps
plot(mps)

mps10 <- aggregate(mps, fact = 10)
mps10
plot(mps10)

r_df10 <- as.data.frame(mps10, xy=T)[,1:2]

r_df10$effort <- km10_eff$effort[match(paste0(r_df10$x, r_df10$y), paste0(km10_eff$lon, km10_eff$lat))]

hist(r_df10$effort)

rast_eff <- rasterFromXYZ(r_df10)

plot(rast_eff)



km1_eff %>%
  ggplot() +
  geom_histogram(aes(x = effort)) +
  theme_classic()

xy_eff <- km1_eff[,c("lon","lat")]
spdf.moth_eff <- SpatialPointsDataFrame(coords = xy_eff, data = km1_eff,
                                    proj4string = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"))

moth_eff_1km <- species_stack[[1]][[1]] # get a single raster

# make a raster of zeroes for input
moth_eff_1km[!is.na(moth_eff_1km)] <- 0

# get the cell index for each point and make a table:
effort = table(cellFromXY(moth_eff_1km,spdf.moth_eff))
hist(effort)

# fill in the raster with the counts from the cell index:
moth_eff_1km[as.numeric(names(effort))] <- effort
plot(moth_eff_1km)

round(dfm$lon[1])