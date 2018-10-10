library(startR)
library(magrittr)
library(sf)
library(tidyverse)

# Custom functions that should be part of startR
source(here::here("scripts", "st_rotate.R"))
source(here::here("scripts", "sfc_as_cols.R"))

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds"))

regions <- read_sf(dsn = here::here("data", "spatial", "regions"),
                   layer = "regions")

# Cuales estan dentro de PIPA

ps <- vessel_tracks %>% 
  filter(gear == "purse_seines",
         treated,
         fishing) %>% 
  st_as_sf(coords = c(7, 8), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_rotate() %>% 
  st_join(regions) %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_set_geometry(value = NULL) %>%
  mutate(country = ifelse(is.na(id), "HS", id)) %>%
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  group_by(year, month, country, total_hours) %>%
  summarize(h = sum(hours)) %>% 
  mutate(h_prop = h / total_hours) %>% 
  ungroup()

displacement_data_ps <- expand.grid(country = ps$country,
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

saveRDS(object = displacement_data_ps,
        file = here::here("data", "displacement_data_ps.rds"))
