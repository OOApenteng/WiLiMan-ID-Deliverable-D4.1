
tif_map_world <- raster("5_Pg_2015_Da.tif") # for pigs and the user can the same link get the rest of the
#population densities as stated in the README or in the code. 

plot(tif_map_world)

tif_map_eu_LIM <- extent(-10, 40, 30, 70)

subset_raster <- raster(ext = tif_map_eu_LIM,
                        nrows = nrow(tif_map_world),
                        ncols = ncol(tif_map_world),
                        crs = projection(tif_map_world))
tif_map_eu <- crop(tif_map_world, subset_raster)
plot(tif_map_eu)
