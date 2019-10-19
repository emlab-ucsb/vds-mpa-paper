
# Take a shapefile of all MPAs and filter out to keep only PIPA.

# Load packages
library(here)
library(sf)
library(tidyverse)

# Read, filter, clean, export
read_sf(here::here("raw_data", "spatial", "WDPA_Jan2019"),
        layer = "WDPA_Jan2019_marine-shapefile-polygons",
        quiet = T,
        stringsAsFactors = F) %>% 
  janitor::clean_names() %>%
  filter(wdpaid == 309888) %>% #https://www.protectedplanet.net/309888
  select(wdpaid, name) %>% 
  st_write(dsn = here("data", "spatial", "PIPA.gpkg"))
