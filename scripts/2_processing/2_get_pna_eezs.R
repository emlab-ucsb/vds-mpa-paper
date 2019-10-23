
#################################################
# Take a shapefile of EEZs and filter out to keep
# only PNA members. These are the Federated States
# of Micronesia (FSM), Kiribati, Marshall Islands,
# Nauru, Palau (PU), Papua New Guinea, Solomon
# Islands Tuvalu and Tokelau
##################################################

# Needed libraries
library(here)
library(rmapshaper)
library(sf)
library(tidyverse)

# ISO3 codes for countries I need
iso_codes <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# Delete any previous files
file.remove(here("data", "spatial", "PNA_EEZ.gpkg"))

# Read, filter, simplyfy, export
st_read(dsn = here("data", "spatial", "EEZ_subset.gpkg")) %>% 
  filter(ISO_Ter1 %in% iso_codes) %>% 
  st_write(here("data", "spatial", "PNA_EEZ.gpkg"))

# END OF SCRIPT
