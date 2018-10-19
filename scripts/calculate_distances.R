

effort_by_vessel <- readRDS(file = here::here("data", "effort_by_vessel.rds"))

short_group <- effort_by_vessel %>%
  filter(baci_strict, gear == "purse_seines") %$%
  unique(mmsi)

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>% 
  filter(mmsi %in% short_group)

(a <- vessel_tracks %>%
  select(date, timestamp, mmsi, lon, lat, hours) %>% 
  sample_n(100000, replace = F) %>% 
  filter(mmsi %in% short_group) %>% 
  group_by(mmsi, date) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 3) %>% 
  arrange(date))

a %>%
  select(-date) %>% 
  arrange(timestamp) %>% 
  mutate(prev_lat = 111.139 * c(0, diff(lat)),
         prev_lon = c(0, diff(lon+180))) %$% range(prev_lon),
         prev_lon = 111.139 * ifelse(abs(prev_lon) > 1, abs(-360 + abs(prev_lon)), prev_lon),
         dist = sqrt(prev_lat^2 + prev_lon^2)) %>% 
  ggplot(aes(x = dist, fill = mmsi, group = mmsi)) +
  geom_density()
