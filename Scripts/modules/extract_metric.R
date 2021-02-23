
### Overlaying SDMs and shapes

library(stars)
library(sf)
library(lwgeom)

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

ggplot() +
  geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
  geom_sf(data = prw) +
  scale_fill_viridis(direction = -1)





t <- raster::extract(agg_rank$error_metric, prw[1], 
                     along = TRUE, cellnumbers = TRUE)
dim(t)
dim(prw[1])

crs(agg_rank$error_metric) <- st_crs(st_geometry(prw))$proj4string
st_intersects(prw, )

