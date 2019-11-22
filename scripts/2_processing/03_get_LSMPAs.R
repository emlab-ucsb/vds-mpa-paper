###################
#   get_LSMPAs    #
###################

###############################################
# This script reads in the WDPA and keeps only
# MPAs larger than 30000 in marine area.
##############################################

# Load magrittr to have the pipe
library(here)
library(rmapshaper)
library(sf)
library(tidyverse)

# Source local functions
source(here("scripts", "0_functions", "st_rotate.R"))

# Delete files if they exist
file.remove(here("data", "spatial", "LSMPAs.gpkg"))

# The PNMS lines are different
pnms <- st_read(here("raw_data", "spatial", "PLW_shapefiles"),
                "PNMS") %>% 
  mutate(WDPAID = 555622118,
         ISO3 = "PLW") %>% 
  select(WDPAID, ISO3) %>% 
  ms_simplify(keep_shapes = T) %>% 
  st_rotate()

# Load the database
st_read(here::here("raw_data", "spatial", "WDPA_Jan2019"),
        layer = "WDPA_Jan2019_marine-shapefile-polygons",
        quiet = T,
        stringsAsFactors = F) %>% 
  filter(GIS_M_AREA > 30000,                            ### Keep LSMPAs only
         !WDPAID == 555622118) %>%                      ### Remove the PNMS, which has new boundaries
  ms_simplify(keep_shapes = T) %>% 
  st_rotate() %>% 
  select(WDPAID, ISO3) %>% 
  rbind(pnms) %>% 
  arrange(WDPAID) %>% 
  st_write(dsn = here("data", "spatial", "LSMPAs.gpkg")) #Save to file

# END OF SCRIPT