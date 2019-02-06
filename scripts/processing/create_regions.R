########################
#   create_regions    #
########################

################################################
# This script creates a shapefile of "regions"
# that I use to ideintify effort redistribution.
# It is basically a shapefile of eez plus a buffer
# around them.
################################################

#### SET UP ################################################

# Load packages
library(sf)
library(tidyverse)

# Load custom functions
source(here::here("scripts", "functions", "st_rotate.R"))

# Define PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# Read PIPA shapefile
pipa <- read_sf(dsn = here::here("data", "spatial", "PIPA"),
                layer = "PIPA") %>% 
  mutate(id = "PIPA 1") %>% 
  select(id) %>% 
  st_rotate() %>% 
  st_buffer(dist = 0.01) %>% 
  mutate(source = "PIPA",
         PNA = F)

# Read vessel tracks
vessel_tracks <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(fishing,
         gear == "tuna_purse_seines",
         !is.na(eez_iso3),
         year < 2018) %>% 
  group_by(eez_iso3) %>% 
  summarize(h = sum(hours)) %>% 
  arrange(desc(h)) %>% 
  mutate(prop = cumsum(h / sum(h))) %>%
  filter(prop <= 0.99)

eez <- read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% c(unique(vessel_tracks$eez_iso3), "FJI")) %>% 
  st_rotate() %>% 
  select(ISO_Ter1) %>%
  group_by(ISO_Ter1) %>%
  mutate(id = paste(ISO_Ter1, row_number())) %>%
  ungroup() %>%
  group_by(id) %>% 
  summarize() %>% 
  st_buffer(0.1)

eez_all <- eez %>%
  mutate(a = 1) %>%
  group_by(a) %>%
  summarize() %>%
  ungroup()

hs_b <- eez %>%
  st_buffer(dist = 5) %>% 
  st_difference(eez_all) %>% 
  select(id) %>% 
  mutate(source = "HS",
         PNA = F) %>% 
  st_difference()

eez_wo_pipa <- eez %>% 
  st_difference(pipa) %>% 
  select(id) %>% 
  mutate(source = "EEZ",
         PNA = substr(x = id, start = 1, 3) %in% PNA_countries)

regions <- rbind(pipa, eez_wo_pipa, hs_b) %>% 
  mutate(country = id,
         id = paste(source, id)) %>% 
  st_cast("POLYGON")

st_write(regions, dsn = here::here("data", "spatial", "regions", "regions.shp"))























