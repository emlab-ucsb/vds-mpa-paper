#Objective: Calculate the total distance traveled by vessel
#Data: raw vessel tracks (vessel_tracks.rds)

library(tidyverse)

tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds"))

one <- filter(tracks, mmsi == 440773000) %>% 
  arrange(timestamp)

dist_fxn <- function(data){
  data %>% 
    arrange(timestamp) %>%
    mutate(lon = ifelse(lon > 0, lon, lon + 360), #Convert longitudes to 0-360 format
           lon1 = lag(lon), #Calculate previous lon
           lat1 = lag(lat), #Calculate previous lat
           delta_lon = lon1 - lon, #Calculate change in lon
           delta_lat = lat1 - lat, #Calculate change in lat
           dist_lon = delta_lon * cos(lat * pi /180) * 111.321, #Convert lon change to km change
           dist_lat = 111 * delta_lat, #Convert lat change to km change
           dist = sqrt(dist_lon ^ 2 + dist_lat ^ 2)) %$% #Calculate distance
    sum(dist, na.rm = T) #Sum across all distances
}

tibble(a = c(1, 1, 1, 2, 2, 2),
       timestamp = c(1, 2, 3, 1, 2, 3),
       lon = c(110, 120, 130, 115, 116, 117),
       lat = 0) %>%
  group_by(a) %>%
  nest() %>%
  mutate(dist = purrr::map_dbl(data, dist_fxn))

#FXN
one %>% 
  filter(date == unique(one$date)[1:2]) %>% 
  group_by(year, month, date, mmsi, gear, treated, post) %>% 
  nest() %>% 
  mutate(dist = purrr::map_dbl(data, dist_fxn))

# Manual
one %>% 
  filter(date == unique(one$date)[1:2]) %>% 
  arrange(timestamp) %>% 
  mutate(lon = ifelse(lon > 0, lon, lon + 360), #Convert longitudes to 0-360 format
         lon1 = lag(lon), #Calculate previous lon
         lat1 = lag(lat), #Calculate previous lat
         delta_lon = lon1 - lon, #Calculate change in lon
         delta_lat = lat1 - lat, #Calculate change in lat
         dist_lon = delta_lon * cos(lat * pi /180) * 111.321, #Convert lon change to km change
         dist_lat = 111 * delta_lat, #Convert lat change to km change
         dist = sqrt(dist_lon ^ 2 + dist_lat ^ 2)) %>%
  group_by(date) %>%
  summarize(dist = sum(dist, na.rm = T))





