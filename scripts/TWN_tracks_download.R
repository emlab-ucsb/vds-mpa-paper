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

saveRDS(vessel_tracks, file = here::here("raw_data", "TWN_tracks.rds"))
