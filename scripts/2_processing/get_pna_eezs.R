
#################################################
# Take a shapefile of EEZs and filter out to keep
# only PNA members. These are the Federated States
# of Micronesia (FSM), Kiribati, Marshall Islands,
# Nauru, Palau (PU), Papua New Guinea, Solomon
# Islands Tuvalu and Tokelau
##################################################

# Needed libraries
library(here)
library(sf)
library(rmapshaper)
library(tidyverse)

# ISO3 codes for countries I need
iso_codes <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# Read, filter, simplyfy, export
read_sf(dsn = here("raw_data", "spatial", "EEZ"),
        layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% iso_codes) %>% 
  ms_simplify(keep_shapes = T) %>% 
  st_write(here("data", "spatial", "PNA_EEZ.gpkg"))

# END OF SCRIPT
