library(startR)
library(magrittr)
library(sf)
library(tidyverse)

# Custom functions that should be part of startR
source(here::here("scripts", "st_rotate.R"))
source(here::here("scripts", "sfc_as_cols.R"))

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds"))

country_groups <- c("COK", "FSM", "KIR", "MHL", "NRU", "PNG", "SLB", "TKL", "TUV", "HS")

eez <- read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% unique(vessel_tracks$eez_iso3)) %>% 
  mutate(PNA = ISO_Ter1 %in% c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")) %>% 
  st_rotate()

pipa <- read_sf(dsn = here::here("data", "spatial", "PIPA"),
                layer = "PIPA") %>% 
  mutate(inside = T) %>% 
  select(inside) %>% 
  st_rotate()

# Cuales estan dentro de PIPA

ps <- vessel_tracks %>% 
  filter(gear == "purse_seines",
         treated,
         fishing) %>% 
  st_as_sf(coords = c(7, 8), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_rotate() %>% 
  st_join(pipa) %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_set_geometry(value = NULL) %>%
  mutate(country = ifelse(is.na(eez_iso3), "HS", eez_iso3),
         country = ifelse(eez_iso3 %in% country_groups, eez_iso3, "others"),
         country = ifelse(is.na(inside), country, "PIPA")) %>%
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  group_by(year, month, country, total_hours) %>%
  summarize(h = sum(hours)) %>% 
  mutate(h_prop = h / total_hours) %>% 
  ungroup()

displacement_data_ps <- expand.grid(country = c(unique(ps$country), "HS"),
                                 year = 2012:2017,
                                 month = 1:12) %>% 
  arrange(country, year, month) %>%
  as.tibble() %>%
  left_join(ps, by = c("country", "year", "month")) %>% 
  mutate(h_prop = ifelse(is.na(h_prop), 0, h_prop)) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")),
         country = fct_relevel(country, c("PIPA", "KIR", "HS")),
         country = fct_relevel(country, "others", after = Inf),
         post = year >= 2015) %>% 
  mutate(gear = "purse_seines")

# Longliners

ll <- vessel_tracks %>% 
  filter(!gear == "purse_seines",
         treated,
         fishing) %>% 
  st_as_sf(coords = c(7, 8), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_rotate() %>% 
  st_join(pipa) %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_set_geometry(value = NULL) %>%
  mutate(country = ifelse(is.na(eez_iso3), "HS", eez_iso3),
  #        country = ifelse(eez_iso3 %in% country_groups, eez_iso3, "others"),
         country = ifelse(is.na(inside), country, "PIPA")) %>%
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  group_by(year, month, country, total_hours) %>%
  summarize(h = sum(hours)) %>% 
  mutate(h_prop = h / total_hours) %>% 
  ungroup()

displacement_data_ll <- expand.grid(country = c(unique(ps$country), "HS"),
                                    year = 2012:2017,
                                    month = 1:12) %>% 
  arrange(country, year, month) %>%
  as.tibble() %>%
  left_join(ll, by = c("country", "year", "month")) %>% 
  mutate(h_prop = ifelse(is.na(h_prop), 0, h_prop)) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")),
         country = fct_relevel(country, c("PIPA", "KIR", "HS")),
         country = fct_relevel(country, "others", after = Inf),
         post = year >= 2015) %>% 
  mutate(gear = "drifting_longlines")

displacement_data <- rbind(displacement_data_ps, displacement_data_ll)

# Save them

saveRDS(object = displacement_data,
        file = here::here("data", "displacement_data.rds"))

