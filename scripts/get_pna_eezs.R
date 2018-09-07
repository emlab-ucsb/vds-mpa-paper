
## RUN ONLY ONCE ! ! ! 

# Take a shapefile of EEZs and filter out to keep only PNA members. These are the Federated States of Micronesia (FSM), Kiribati, Marshall Islands, Nauru, Palau (PU), Papua New Guinea, Solomon Islands and Tuvalu.

library(sf)
library(tidyverse)

read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "EEZ_MEOW") %>% 
  filter(ISO_Ter1 %in% c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")) %>% 
  st_write(dsn = here::here("data", "spatial", "PNA_EEZ", "PNA_EEZ.shp"))
