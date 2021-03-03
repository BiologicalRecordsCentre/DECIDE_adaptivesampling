

#  Combine the error metric layer with Accessible areas/PROW files
#  extract the error metric into the various other layers
# 
# _Input_: Error layer; Accessible area layer
# 
# _Output_: Accessible area layer with corresponding error metric
# 
# _Metadata_:
# 


### Overlaying SDMs and shapes

library(stars)
library(sf)
library(lwgeom)
library(viridis)


extract_metric <- function(metric, 
                           shape_obj){
  
  if(class(metric)[1] != 'sf' | class(shape_obj)[1] != 'sf'){
    stop('Both objects must be of class "sf". Is your metric a raster? Convert it using "stars" package.')
  }
  
  # first get the intersected boxes to reduce processing time
  intersects <- apply(st_intersects(metric, shape_obj, sparse = FALSE), 1, any)
  intersected_metric <- metric[intersects,]
  
  print('#####     cropping     #####')
  # crop the shape object to the grid of the metric
  # maybe parallelize??
  cropped <- lapply(c(1:length(intersected_metric[[2]])), FUN = function(x){
    loop_crop <- st_crop(shape_obj, st_bbox(intersected_metric[x,]))
    return(loop_crop)
  })
  
  # combine all
  print('#####     joining     #####')
  crp_comb <- do.call('rbind', cropped) # takes a while
  
  # join error_metric and cropped prow
  crp_err <- st_join(crp_comb, metric,
                     largest = FALSE,
                     join = st_within)
  
  return(crp_err)
}


## convert raster to an sf object
conv_rast <- function(raster, # raster with a KNOWN COORDINATES SYSTEM
                      coord){ # the coordinates system!
  
  require(stars)
  
  stars_obj <- st_as_stars(raster)
  sf_obj <- st_as_sf(stars_obj, as_points = FALSE, merge = FALSE)
  st_crs(sf_obj) <- coord
  
  return(sf_obj)
  
}


# ###############       Development/testing below
# 
# 
# #######    quick testing testing the function     #######
# 
# # convert raster to an sf object
# # convert to its own function?
# stars_rast <- st_as_stars(agg_rank$error_metric) # only interested in the error metric
# sm_gr <- st_as_sf(stars_rast, as_points = FALSE, merge = FALSE)
# st_crs(sm_gr) <- 27700
# 
# 
# 
# # crop PROW
# prw <- filter_distance(obj = final_acc_loc[[1]],
#                        # location = location,
#                        distance = 2000,
#                        method = 'buffer')
# 
# # crop accessible layers
# accs <- filter_distance(obj = final_acc_loc[[2]],
#                         # location = location,
#                         distance = 2000,
#                         method = 'buffer')
# 
# 
# ## This works with linestring object 
# system.time(
#   ex_prw <- extract_metric(metric = sm_gr, shape_obj = prw)
# ) ## takes a long time
# 
# system.time(
#   ex_access <- extract_metric(metric = sm_gr, shape_obj = accs)
# )
# 
# 
# comb_p <- ggplot() +
#   geom_sf(data = sm_gr, aes(fill = error_metric), 
#           show.legend = F, 
#           alpha = 0.4,
#           colour = 'white') +
#   geom_sf(data = ex_prw, aes(colour = error_metric)) +
#   geom_sf(data = ex_access, aes(fill = error_metric), show.legend = F) +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   theme_bw()
# comb_p
# 
# ggsave(plot = comb_p,
#        filename = 'outputs/error_metric_combplot.tiff',
#        height = 5, width = 6)
# 
# ##  combined plot
# library(patchwork)
# 
# p1 <- ggplot() +
#   geom_sf(data = sm_gr, aes(fill = error_metric), show.legend = F) +
#   # geom_sf(data = ex_prw, aes(colour = error_metric)) +
#   # geom_sf(data = ex_access, aes(fill = error_metric), show.legend = F) +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   ylim(c(187500, 191750)) +
#   xlim(c(459500, 464000)) +
#   theme_bw()
# 
# p2 <- ggplot() +
#   # geom_sf(data = sm_gr, aes(fill = error_metric), show.legend = F) +
#   geom_sf(data = ex_prw, aes(colour = error_metric), show.legend = F) +
#   # geom_sf(data = ex_access, aes(fill = error_metric), show.legend = F) +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   ylim(c(187500, 191750)) +
#   xlim(c(459500, 464000)) +
#   theme_bw()
# 
# p3 <- ggplot() +
#   # geom_sf(data = sm_gr, aes(fill = error_metric), show.legend = F) +
#   # geom_sf(data = ex_prw, aes(colour = error_metric)) +
#   geom_sf(data = ex_access, aes(fill = error_metric), show.legend = TRUE) +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   ylim(c(187500, 191750)) +
#   xlim(c(459500, 464000)) +
#   theme_bw()
# 
# p2save <- p1 + p2 + p3 + 
#   plot_layout(guides = 'collect')
# p2save
# 
# ggsave(plot = p2save,
#        filename = 'outputs/error_metric_egplot.tiff',
#        height = 5, width = 15)
# 
# 
# ## First get the error metric raster from the 'making_recommendations' workflow
# # need to convert this raster layer to an sf object because
# # it is easier to combine/manipulate two sf objects than to combine
# # raster and sf objects
# # can do this through the 'stars' package
# library(stars)
# 
# plot(agg_rank) ## use the aggregated sum prob+variation across all species
# 
# # convert raster to an sf object
# # convert to its own function?
# stars_rast <- st_as_stars(agg_rank$error_metric) # only interested in the error metric
# sm_gr <- st_as_sf(stars_rast, as_points = FALSE, merge = FALSE)
# st_crs(sm_gr) <- 27700
# 
# 
# ## Now get the PROW files from 'find_accessible_areas' workflow
# # prow files from the 'final_acc_loc' object
# plot(st_geometry(final_acc_loc[[1]])) ## prow
# 
# # crop to match area that we were interested from the raster
# # greenspace/prow files
# # prow
# plot(st_geometry(final_acc_loc[[1]]))
# 
# prw <- filter_distance(obj = final_acc_loc[[1]],
#                        # location = location,
#                        distance = 2000,
#                        method = 'buffer')
# 
# plot(sm_gr)
# plot(st_geometry(sm_gr))
# plot(st_geometry(prw), add = T)
# 
# # get the cells that the PROW cross
# prw_intersect <- apply(st_intersects(sm_gr, prw, sparse = FALSE), 1, any)
# 
# # plot the cells touching the PROW
# ggplot() +
#   geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
#   geom_sf(data = prw) +
#   scale_fill_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700))
# 
# # use st_join to get the error metric attached to the 
# # PROW file
# prw_errgrid <- st_join(prw, sm_gr,
#                        largest = FALSE,
#                        join = st_intersects)
# 
# # plot to try
# ggplot() +
#   # geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
#   geom_sf(data = prw_errgrid, aes(colour = error_metric)) +
#   # scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700))
# 
# ## this is good but there is only one error metric (not sure which...?)
# ## associated with each PROW segment - NOT what we want.
# 
# 
# ####    Try to crop the prow file with the error metric grid
# 
# # the error metric is an sf object with each square a different geometry 
# # polygon in each row
# sm_gr
# 
# # get bbox of one geometry - need this to do the cropping
# st_bbox(sm_gr[1,])
# 
# # crop the PROW that are in the first square in the sm_gr object
# test_crop <- st_crop(prw, st_bbox(sm_gr[1,]))
# plot(st_geometry(test_crop)) ## actually is nothing in the first square...
# 
# 
# ### to crop for all boxes use a for loop/lapply statement
# 
# # initiate list
# cropped_prw <- vector("list", length = length(sm_gr[[2]]))
# 
# # the loop
# for(i in 1:length(sm_gr[[2]])) {
#   
#   loop_crop <- st_crop(prw, st_bbox(sm_gr[i,]))
#   cropped_prw[[i]] <- loop_crop
#   
# }
# 
# # that worked
# plot(cropped_prw[[750]])
# 
# # combine all
# crp_prw <- do.call('rbind', cropped_prw) # takes AGES
# plot(st_geometry(crp_prw)) ## looks okay
# crp_prw
# length(unique(crp_prw$Name)) ## the name is duplicated because of the cropping
# 
# 
# # now that the lines are cropped to the grid and then joined together
# # we can then find the error metric value that is associated with
# # any particular segment of the PROW
# # join error_metric and cropped prow using the st_join() function
# # after testing - using the st_within join argument is definitely the best 
# crp_prw_err <- st_join(crp_prw, sm_gr,
#                        largest = FALSE,
#                        join = st_within) ## st_within is the argument to use!
# 
# # plot to see what these error metrics look like
# ggplot() +
#   # geom_sf(data = sm_gr[prw_intersect,], aes(fill = error_metric)) +
#   geom_sf(data = crp_prw_err, aes(colour = error_metric)) +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   theme_bw()
# ## preeeettttyyyyyy cool
# 
# 
# 
# ####   Aaaannnnddd thhheeeenn   ####
# 
# # Try this same thing with accessible areas
# 
# # crop sf object to region of interest
# ## final_acc_loc[[2]] is the accessible greenspaces
# accs <- filter_distance(obj = final_acc_loc[[2]],
#                         # location = location,
#                         distance = 2000,
#                         method = 'buffer')
# plot(st_geometry(accs), col = 'green') # all good
# 
# ## convert raster uncertainty layer to an sf object (same as above - just doing
# ## for consistency)
# stars_rast <- st_as_stars(agg_rank$error_metric)
# sm_gr <- st_as_sf(stars_rast, as_points = FALSE, merge = FALSE)
# st_crs(sm_gr) <- 27700
# 
# # get the cells that the accessible areas cross
# accs_intersect <- apply(st_intersects(sm_gr, accs, sparse = FALSE), 1, any)
# 
# # plot - yes kind of makes sense
# ggplot() +
#   geom_sf(data = sm_gr[accs_intersect,], aes(fill = error_metric)) +
#   geom_sf(data = accs) +
#   scale_fill_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700))
# 
# 
# 
# #### try and crop the accessible area file
# 
# # first get the intersected files
# # the loop and bining works much quicker with this
# er_accs_int <- sm_gr[accs_intersect,]
# plot(er_accs_int)
# 
# test_crop <- st_crop(accs, st_bbox(er_accs_int[1,]))
# plot(st_geometry(test_crop))
# 
# cropped_accs <- vector("list", length = length(er_accs_int[[2]]))
# 
# # loop
# system.time(for(i in 1:length(er_accs_int[[2]])) {
#   
#   loop_crop <- st_crop(accs, st_bbox(er_accs_int[i,]))
#   cropped_accs[[i]] <- loop_crop
#   
# })
# 
# 
# # combine all
# system.time(crp_accs <- do.call('rbind', cropped_accs)) # takes AGES
# plot(st_geometry(crp_accs)) ## looks okay
# crp_accs
# 
# # join error_metric and cropped prow
# crp_accs_err <- st_join(crp_accs, sm_gr,
#                         largest = FALSE,
#                         join = st_within) ## st_within is the argument to use!
# 
# ggplot() +
#   geom_sf(data = sm_gr, aes(fill = error_metric)) +
#   geom_sf(data = crp_accs_err, aes(fill = error_metric), colour = 'white') +
#   scale_fill_viridis(option = 'B') +
#   scale_colour_viridis(option = 'B') +
#   coord_sf(datum = sf::st_crs(27700)) +
#   theme_bw()
# 
# 
# 
# # example sdm metric
# plot(agg_rank$error_metric)
# plot(st_geometry(prw), add = T)
# 
# 
# 
