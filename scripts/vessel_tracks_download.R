# I queried the database in GBQ to get the tracks fo all vessels. That table (vessel_tracks) was made with this query (also found in ~scripts/vessel_tracks.sql):

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

vessel_tracks <- dplyr::tbl(BQc, "vessel_tracks") %>% 
  collect()

PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")
VDS_countries <- c(PNA_countries, "TKL")

vessel_tracks %<>% 
  distinct() %>% 
  mutate(date = lubridate::date(timestamp),
         post = year >= 2015,
         fishing = nnet_score >= 0.5,
         month_c = as.character(month),
         year_c = as.character(year),
         year_month = lubridate::date(paste(year, month, 1, sep = "/")),
         quarter = lubridate::quarter(date, with_year = T),
         PNA = flag %in% PNA_countries,
         VDS = flag %in% PNA_countries) %>% 
  select(year,
         month,
         day,
         date,
         timestamp,
         post,
         treated,
         mmsi,
         gear = inferred_label,
         gear_label_score = label_score,
         flag,
         lon,
         lat,
         hours,
         fishing,
         nnet_score,
         eez_iso3,
         distance_from_port,
         distance_from_shore,
         seg_id,
         month_c,
         year_c,
         year_month,
         quarter,
         PNA,
         VDS)

saveRDS(vessel_tracks, file = here::here("raw_data", "vessel_tracks.rds"))
