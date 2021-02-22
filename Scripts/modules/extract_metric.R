
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
plot(agg_rank$error_metric)
plot(st_geometry(prw), add = T)


## could get a grid at 100m to use to cut the prow...
t <- st_as_stars(agg_rank$error_metric)
sm_gr <- st_as_sf(t, as_points = FALSE, merge = FALSE)
st_crs(sm_gr) <- 27700

plot(sm_gr)
plot(st_geometry(sm_gr))
plot(st_geometry(prw), add = T)

test_split <- st_split(prw, st_combine(sm_gr))
dim(test_split)
dim(prw)
plot(test_split$geometry)
plot(test_split['ROW_TYPE'])
plot(test_split[1])
plot(prw[1])

plot(st_combine(sm_gr))

BB <- st_cast(sm_gr, "MULTILINESTRING", group_or_split=FALSE)
## Break LINESTRING A into segments by using:
## - st_intersection() to find points at which lines features intersect
## - st_buffer() to convert points to tiny polygons with some width
## - st_difference() to break line up into sections not overlapping tiny polygons
C <- st_difference(prw, st_buffer(st_intersection(prw,BB), dist=1e-12))
plot(C)

plot(sm_gr, col="grey")
plot(st_cast(C, "LINESTRING"), lwd=3, add=TRUE)

plot(agg_rank$error_metric)
plot(st_geometry(prw), add = T)
plot((sm_gr), border = 'red', add = T)

grid_intersect <- apply(st_intersects(uk_grid, uk_map, sparse = FALSE), 1, any)

transect = raster::extract(agg_rank$error_metric, prw, 
                           along = TRUE, cellnumbers = TRUE, df = F)
transect_df = purrr::map_dfr(transect, as_data_frame, .id = "ID")
transect_coords = xyFromCell(agg_rank$error_metric, transect_df$cell)




t <- raster::extract(agg_rank$error_metric, prw[1], 
                     along = TRUE, cellnumbers = TRUE)
dim(t)
dim(prw[1])

crs(agg_rank$error_metric) <- st_crs(st_geometry(prw))$proj4string
st_intersects(prw, )

