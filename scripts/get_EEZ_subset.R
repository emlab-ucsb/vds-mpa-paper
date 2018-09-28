library(sf)
library(tidyverse)

source(here::here("scripts", "st_rotate.R"))

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds"))

read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% unique(vessel_tracks$eez_iso3)) %>% 
  mutate(PNA = ISO_Ter1 %in% c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")) %>% 
  st_rotate() %>% 
  select(ISO_Ter1, PNA) %>% 
  st_write(dsn = here::here("data", "spatial", "EEZ_subset", "EEZ_subset.shp"))

