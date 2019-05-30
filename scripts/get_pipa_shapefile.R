
## RUN ONLY ONCE ! ! ! 

# Take a shapefile of all MPAs and filter out to keep only PIPA.

library(sf)
library(tidyverse)

read_sf(here::here("raw_data", "spatial", "WDPA_Mar2018"),
        layer = "WDPA_Mar2018_marine-shapefile-polygons",
        quiet = T,
        stringsAsFactors = F) %>% 
  janitor::clean_names() %>%
  filter(wdpaid == 309888) %>% #https://www.protectedplanet.net/309888
  select(wdpaid, name) %>% 
  st_write(dsn = here::here("data", "spatial", "PIPA", "PIPA.shp"))
