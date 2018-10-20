library(raster)
library(fasterize)
library(sf)
library(tidyverse)
source(here::here("scripts", "st_rotate.R"))

rasterize_df <- function(x, r, fun = "sum"){
  x2 <- x %>%
    select(lon, lat, hours) %>% 
    mutate(lon = ifelse(lon < 0, lon + 180, lon - 180) + 180) %>% 
    as.matrix()
  
  rasterize(x = x2[,1:2],
            y = r,
            field = x2[,3],
            fun = fun) %>% 
    return()
}

extract_raster <- function(r, year){
  as.data.frame(r, xy = T) %>%
    rename(hours = layer) %>% 
    mutate(year = year) %>% 
    return()
}

regions <- read_sf(dsn = here::here("data", "spatial", "regions"),
                   layer = "regions") %>% 
  filter(!id %in% eezs_exclude)

regions_raster <- regions %>%
  mutate(unique = group_indices(., id)) %>%
  fasterize(sf = .,
            raster = raster(., res = 1),
            field = "unique",
            background = 0)

regions_with_HS <- regions %>%
  mutate(unique = group_indices(., id)) %>% 
  st_set_geometry(NULL) %>% 
  rbind(data.frame(id = "HS", source = "HS", PNA = 0, country = "HS", unique = 0))

regions_raster_df <- as.data.frame(regions_raster, xy = T) %>% 
  left_join(regions_df, by = c("layer" = "unique"))

plot(regions_raster)

vessel_tracks <- vessel_tracks %>%  #readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>% 
  filter(gear == "purse_seines",
         year < 2018,
         fishing,
         treated)

vessel_tracks_2012 <- vessel_tracks %>% 
  filter(year == 2012) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)

vessel_tracks_2013 <- vessel_tracks %>% 
  filter(year == 2013) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)

vessel_tracks_2014 <- vessel_tracks %>% 
  filter(year == 2014) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)

vessel_tracks_2015 <- vessel_tracks %>% 
  filter(year == 2015) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)

vessel_tracks_2016 <- vessel_tracks %>% 
  filter(year == 2016) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)

vessel_tracks_2017 <- vessel_tracks %>% 
  filter(year == 2017) %>% 
  drop_na() %>%
  rasterize_df(r = regions_raster)


# eezs_exclude <- c("HS MUS 1", "EEZ MUS 1", "EEZ MDG 1", "EEZ MDG 2", "HS MDG 1", "HS MDG 2", "HS MOZ 1", "EEZ MOZ 1")

all_tracks <- extract_raster(vessel_tracks_2012, year = 2012) %>% 
  rbind(extract_raster(vessel_tracks_2013, 2013)) %>% 
  rbind(extract_raster(vessel_tracks_2014, 2014)) %>% 
  rbind(extract_raster(vessel_tracks_2015, 2015)) %>% 
  rbind(extract_raster(vessel_tracks_2016, 2016)) %>% 
  rbind(extract_raster(vessel_tracks_2017, 2017)) %>% 
  left_join(regions_raster_df, by = c("x", "y")) %>%
  group_by(year) %>% 
  mutate(max_hours = max(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_norm = hours / max_hours,
         treated = "Treated") %>% 
  filter(!id %in% c("HS PNG 1", "EEZ PNG 1", "HS MHL 1", "HS FSM 1", "HS NRU 1", "HS PNG 2", "HS SLB 1", "HS TUV 1")) %>%
  drop_na() %>%
  mutate(year_c = as.factor(year))
  

model <- lm(hours~year_c*id + x + y, data = all_tracks)

summary(model)

all_tracks %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = hours_norm)) +
  # geom_sf(data = eez, fill = "transparent", color = "black") +
  # geom_sf(data = mpas_WCPO, fill = "transparent", color = "red") +
  scale_fill_viridis_c(trans = "log10") +
  facet_grid(year~., switch = "y") +
  ggtheme_map() +
  coord_equal() +
  theme(legend.position = "bottom")

all_tracks %>% 
  mutate(predicted = predict(model),
         residual = hours - predicted) %>% 
  ggplot()
  geom_raster(aes(x = x, y = y, fill = hours)) +
  # geom_sf(data = eez, fill = "transparent", color = "black") +
  # geom_sf(data = mpas_WCPO, fill = "transparent", color = "red") +
  scale_fill_viridis_c(trans = "log10") +
  facet_grid(year~., switch = "y") +
  ggtheme_map() +
  theme(legend.position = "bottom")
  
  # Sacar rasters de control
  # Actualizar datos de redistribution raster
  # Grafica de cambio por año por pais tipo termplot
  
  
  
  
  




