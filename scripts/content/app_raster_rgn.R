


raster_rgn <- raster::raster(here::here("data", "spatial", "regions_raster.tif")) %>% 
  raster::as.data.frame(xy = T) %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = regions_raster)) +
  theme_bw() +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas_WCPO, fill = "transparent", color = "red") +
  geom_sf(data = small_coast, color = "black")


raster_rgn