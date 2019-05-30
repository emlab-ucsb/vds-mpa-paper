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

# Modify the data
vessel_tracks %<>%
  distinct() %>%
  mutate(
    date = lubridate::date(timestamp),
    post = year >= 2015,
    nnet_score = ifelse(is.na(nnet_score), 0, nnet_score), # If there are NAs in nnet_score, convert to 0 (no fishing)
    fishing = nnet_score >= 0.5, # Values of nnet_score > 0.5 imply fishing
    month_c = as.character(month),
    year_c = as.character(year),
    year_month = lubridate::date(paste(year, month, 1, sep = "/")),
    quarter = lubridate::quarter(
      date,
      with_year = T)) %>%
  select(
    year,
    month,
    day,
    date,
    timestamp,
    post,
    treated,
    mmsi,
    gear,
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
    quarter
  )

saveRDS(vessel_tracks, file = here::here("raw_data", "TWN_tracks.rds"))

