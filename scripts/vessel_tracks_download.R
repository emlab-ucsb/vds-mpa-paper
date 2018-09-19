# I queried the database in GBQ to get the tracks fo all vessels. That table (vessel_tracks) was made with this query (also found in ~scripts/vessel_tracks.sql):

# SELECT
# A.mmsi AS mmsi,
# A.timestamp AS timestamp,
# A.lon AS lon,
# A.lat AS lat,
# A.hours AS hours,
# A.nnet_score AS nnet_score,
# A.eez_iso3,
# B.treated AS treated,
# B.gear AS gear
# FROM (
#   SELECT
#   mmsi,
#   timestamp,
#   lon,
#   lat,
#   hours,
#   nnet_score,
#   eez_iso3
#   FROM
#   `world-fishing-827.gfw_research.nn7`
#   WHERE
#   mmsi IN (
#     SELECT
#     mmsi
#     FROM
#     `ucsb-gfw.mpa_displacement.vessel_groups`)) A
# JOIN
# `ucsb-gfw.mpa_displacement.vessel_groups` B
# ON
# A.mmsi = B.mmsi

# This litle scripts just downloads it.

suppressPackageStartupMessages({
  library(dplyr)
  library(dbplyr)
  library(DBI)
  library(bigrquery)
})

BQc <- bigrquery::dbConnect(drv = bigrquery::bigquery(), 
                            project = "ucsb-gfw", 
                            dataset = "mpa_displacement", 
                            allowLargeResults = TRUE)

DBI::dbListTables(BQc)

vessel_tracks <- dplyr::tbl(BQc, "vessel_tracks") %>% 
  collect()

saveRDS(vessel_tracks, file = here::here("raw_data", "vessel_tracks.rds"))