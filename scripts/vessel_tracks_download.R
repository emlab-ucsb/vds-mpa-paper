# I queried the database in GBQ to get the tracks fo all vessels. That table (vessel_tracks) was made with this query (also found in ~scripts/vessel_tracks.sql):

# SELECT
# A.mmsi AS mmsi,
# A.timestamp AS timestamp,
# A.lon AS lon,
# A.lat AS lat,
# A.hours AS hours,
# A.nnet_score AS nnet_score,
# A.eez_iso3,
# A.distance_from_port AS distance_from_port,
# A.distance_from_shore AS distance_from_shore,
# A.seg_id AS seg_id,
# B.treated AS treated,
# B.gear AS gear,
# B.flag AS flag
# FROM (
#   SELECT
#   mmsi,
#   timestamp,
#   lon,
#   lat,
#   hours,
#   nnet_score,
#   eez_iso3,
#   distance_from_port,
#   distance_from_shore,
#   seg_id
#   FROM
#   `world-fishing-827.gfw_research.nn7`
#   WHERE
#   mmsi IN (
#     SELECT
#     mmsi
#     FROM
#     `ucsb-gfw.mpa_displacement.vessel_groups`)
#   AND seg_id IN (
#     SELECT
#     seg_id
#     FROM
#     `world-fishing-827.gfw_research.good_segments`)) A
# JOIN
# `ucsb-gfw.mpa_displacement.vessel_groups` B
# ON
# A.mmsi = B.mmsi
# This litle scripts just downloads it.

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

saveRDS(vessel_tracks, file = here::here("raw_data", "vessel_tracks.rds"))
