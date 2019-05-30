library(tidyverse)

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>% 
  filter(gear == "purse_seines")


fishing_raster <- vessel_tracks %>%
  filter(fishing,
         year < 2018) %>% 
  mutate(lon = round(lon),
         lon = ifelse(lon < 0, lon + 180, lon - 180) +180,
         lat = round(lat)) %>% 
  filter(between(lon, 135, 220),
         between(lat, -20, 20)) %>% 
  ungroup() %>% 
  group_by(year, treated, lon, lat) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>%
  group_by(year, treated) %>% 
  mutate(max_hours = max(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_norm = hours / max_hours,
         treated = ifelse(treated, "Treated", "Control"))

saveRDS(object = fishing_raster, file = here::here("data", "fishing_raster_BACI.rds"))
