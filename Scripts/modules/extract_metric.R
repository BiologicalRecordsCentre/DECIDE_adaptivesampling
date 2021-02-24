
### Overlaying SDMs and shapes

library(stars)
library(sf)
library(lwgeom)
library(viridis)

# greenspace/prow files
# prow
plot(st_geometry(final_acc_loc[[1]]))

prw <- filter_distance(obj = final_acc_loc[[1]],
                       # location = location,
                       distance = 2000,
                       method = 'buffer')

# example sdm metric
plot(agg_rank$error_metric, col = bpy.colors(100))
plot(st_geometry(prw), add = T, col = 'red', pwd = 2)


## could get a grid at 100m to use to cut the prow...
## convert raster to an sf object
t <- st_as_stars(agg_rank$error_metric)
sm_gr <- st_as_sf(t, as_points = FALSE, merge = FALSE)
st_crs(sm_gr) <- 27700

plot(sm_gr)
plot(st_geometry(sm_gr))
plot(st_geometry(prw), add = T)

# get the cells that the PROW cross
prw_intersect <- apply(st_intersects(sm_gr, prw, sparse = FALSE), 1, any)

# plot the cells touching the PROW
plot(sm_gr[prw_intersect, ], key.pos = NULL)
plot(st_geometry(prw), add = T)
## don't overlap properly - not sure why...

# try with ggplot - looks nicer and more easily customisable
ggplot() +
  geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
  geom_sf(data = prw) +
  scale_fill_viridis(option = 'B') +
  coord_sf(datum = sf::st_crs(27700))

# use st_join to get the error metric attached to the 
# PROW file
t <- st_join(prw, sm_gr,
             largest = FALSE,
             join = st_intersects)

# test plot
ggplot() +
  # geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
  geom_sf(data = t, aes(colour = error_metric)) +
  # scale_fill_viridis(option = 'B') +
  scale_colour_viridis(option = 'B') +
  coord_sf(datum = sf::st_crs(27700))



#### try and crop the prow file

# first get the intersected files
er_int <- sm_gr[prw_intersect,]
plot(er_int)

# get bbox of one geometry
st_bbox(er_int[1,])

test_crop <- st_crop(prw, st_bbox(er_int[1,]))
plot(st_geometry(test_crop))

cropped_prw <- vector("list", length = length(sm_gr[[2]]))

# loop
for(i in 1:length(sm_gr[[2]])) {
  
  loop_crop <- st_crop(prw, st_bbox(sm_gr[i,]))
  cropped_prw[[i]] <- loop_crop
  
}

# that worked
plot(cropped_prw[[5]])

# combine all
crp_prw <- do.call('rbind', cropped_prw) # takes AGES
plot(st_geometry(crp_prw)) ## looks okay
crp_prw

# join error_metric and cropped prow
crp_prw_err <- st_join(crp_prw, sm_gr,
                       largest = FALSE,
                       join = st_within) ## st_within is the argument to use!

ggplot() +
  # geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
  geom_sf(data = crp_prw_err, aes(colour = error_metric)) +
  scale_fill_viridis(option = 'B') +
  scale_colour_viridis(option = 'B') +
  coord_sf(datum = sf::st_crs(27700)) +
  theme_bw()




##### Do this with accessible areas

# crop sf object to region of interest
accs <- filter_distance(obj = final_acc_loc[[2]],
                       # location = location,
                       distance = 2000,
                       method = 'buffer')
plot(st_geometry(accs), col = 'green') # all good

## convert raster uncertainty layer to an sf object (same as above)
stars_rast <- st_as_stars(agg_rank$error_metric)
sm_gr <- st_as_sf(stars_rast, as_points = FALSE, merge = FALSE)
st_crs(sm_gr) <- 27700




#######     Creating a function to do this     #######

extract_metric <- function(metric, shape_obj){
  
  print('#####     cropping     #####')
  # crop the shape object to the grid of the metric
  # maybe parallelize??
  cropped_prw <- lapply(c(1:length(metric[[2]])), FUN = function(x){
    loop_crop <- st_crop(shape_obj, st_bbox(metric[x,]))
    return(loop_crop)
  })
  
  # combine all
  print('#####     joining     #####')
  crp_prw <- do.call('rbind', cropped_prw) # takes a while
  
  # join error_metric and cropped prow
  crp_prw_err <- st_join(crp_prw, metric,
                         largest = FALSE,
                         join = st_within)
  
  return(crp_prw_err)
}

system.time(
  ex_t <- extract_metric(metric = sm_gr, shape_obj = prw)
) ## takes a while

cropped_prw <- vector("list", length = length(sm_gr[[2]]))

# loop
for(i in 1:length(sm_gr[[2]])) {
  
  loop_crop <- st_crop(prw, st_bbox(sm_gr[i,]))
  cropped_prw[[i]] <- loop_crop
  
}

# that worked
plot(cropped_prw[[5]])

# combine all
crp_prw <- do.call('rbind', cropped_prw) # takes AGES
plot(st_geometry(crp_prw)) ## looks okay
crp_prw

# join error_metric and cropped prow
crp_prw_err <- st_join(crp_prw, sm_gr,
                       largest = FALSE,
                       join = st_within) ## st_within is the argument to use!

