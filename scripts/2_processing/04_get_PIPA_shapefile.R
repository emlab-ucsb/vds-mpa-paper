
# Take a shapefile of all MPAs and filter out to keep only PIPA.

# Load packages
library(here)
library(sf)
library(tidyverse)

# Remove file
file.remove(here("data", "spatial", "PIPA.gpkg"))

# Read, filter, clean, export
st_read(here("data", "spatial", "LSMPAs.gpkg"),
        quiet = T) %>% 
  filter(WDPAID == 309888) %>% #https://www.protectedplanet.net/309888
  select(WDPAID, NAME) %>% 
  st_write(dsn = here("data", "spatial", "PIPA.gpkg"))

# END OF SCRIPT
