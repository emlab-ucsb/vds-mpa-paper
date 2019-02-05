library(dplyr)
library(dbplyr)
library(DBI)
library(bigrquery)
library(magrittr)

BQc <- bigrquery::dbConnect(drv = bigrquery::bigquery(), 
                            project = "ucsb-gfw", 
                            dataset = "mpa_displacement", 
                            allowLargeResults = TRUE)

DBI::dbListTables(BQc)

vessel_tracks <- dplyr::tbl(BQc, "TWN_tracks") %>% 
  collect()

vessel_tracks %<>% 
  mutate(year = lubridate::year(timestamp),
         month = lubridate::month(timestamp),
         date = lubridate::date(timestamp),
         post = year >= 2015,
         fishing = nnet_score >= 0.5) %>% 
  select(year,
         month,
         date,
         timestamp,
         mmsi,
         gear,
         lon,
         lat,
         hours,
         fishing,
         treated,
         post,
         nnet_score,
         eez_iso3,
         distance_from_port,
         distance_from_shore)

before <- vessel_tracks %>% 
  filter(date < lubridate::date("2014/09/01")) %$%
  unique(mmsi)

after <- vessel_tracks %>% 
  filter(post) %$%
  mmsi %>%
  unique()

saveRDS(vessel_tracks, file = here::here("raw_data", "TWN_tracks.rds"))

effort_by_vessel <- vessel_tracks %>% 
  filter(fishing) %>% 
  group_by(post, treated, year, month, date, gear, mmsi) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(month_c = as.character(month),
         year_c = as.character(year),
         year_month = lubridate::date(paste(year, month, 1, sep = "/")),
         quarter = lubridate::quarter(date, with_year = T),
         PNA = FALSE)

saveRDS(effort_by_vessel, file = here::here("raw_data", "TWNeffort_by_vessel.rds"))
