####################################################################################
# This query will extract all vessels that evr fished
# in the PNA, and then assign them to groups based on
# when and where they fished. There are two groups:
# - "displaced": vessels that fished inside PIPA before Jan, 2015 and continued to
#    fish elsewhere
# - "non-displaced": vessels that never fished inside PIPA before Jan, 2015, and that
#   we observe before and after the closure of PIPA
#
############################
#
# The query will proceed in the following way:
# 1) Create a subquery that contains the EEZ identifiers (numeric) based on the known
#     iso3 codes for PAN countries
# 2) Create a subquery for all vessels that ever fished within these EEZs. Here, we'll
#     define fishing as having a neural net score of nnet_score > 0.5
# 3) Then, create a subquery of all vessel tracks (that is, with lat lon positions)
#     for all vessels that ever fished in PNA countries
# 4) Create a spatial table using the PIPA shapefile
# 5) Intersect the vessel tracks with the PIPA shapefile to find vessels that fished
#     within PIPA before Jan, 2015 (these are the "displaced" vessels)
# 6) Create a table of ssvid and group
#
####################################################################################
############################
# 1) Get eez info
############################
WITH
  eez_info AS (
  SELECT
    CAST(eez_id AS STRING) AS eez_id,
    territory1_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info`
  WHERE
    territory1_iso3 IN ("FSM",
      "KIR",
      "MHL",
      "NRU",
      "PLW",
      "PNG",
      "SLB",
      "TUV",
      "TKL")),
  #
  #
  #
  #
  ########
############################
# 2) Vessels that fished within the PNA
############################
  vessels_i_want AS (
  SELECT
    DISTINCT(ssvid)
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  CROSS JOIN
    UNNEST(activity.eez) AS eez
  WHERE
    eez.value IN (
    SELECT
      eez_id
    FROM
      eez_info)
    AND best.best_vessel_class = "tuna_purse_seines" #keep purse seiners only
    AND year < 2019                                  #only 2012 - 2018
    AND CAST(ssvid AS int64) NOT IN (
    SELECT
      ssvid
    FROM
      `world-fishing-827.gfw_research.bad_mmsi`
    CROSS JOIN
      UNNEST(ssvid) AS ssvid)                        # filter our bad mmsis
    AND ssvid NOT IN ("1")
    AND on_fishing_list_best
    AND NOT activity.offsetting
    AND activity.fishing_hours > 24
    AND activity.active_hours > 24),                 # keep only vessels with fishing within PNA
  #
  #
  #
  #
  ########
############################
# 3) get vessel tracks for vessels above
############################
  vessel_tracks AS (
  SELECT
    date,
    ssvid,
    hours,
    lon,
    lat,
    nnet_score > 0.5 AS fishing
  FROM
    `world-fishing-827.gfw_research.pipe_v20190502_fishing`
  CROSS JOIN UNNEST(regions.eez) AS eez
  WHERE
    ssvid IN (
    SELECT
      ssvid
    FROM
      vessels_i_want)
    AND date < TIMESTAMP("2019-01-01")),                    # Again, only data before 2019
  #
  #
  #
  #
  ########
############################
# 4) Create geography object
############################
  geog AS (
  SELECT
    *,
    ST_GeogFromGeoJSON(geom) AS g
  FROM
    `ucsb-gfw.mpa_displacement.pipa_border` ),
  #
  #
  #
  #
  ########
############################
# 5) Perform intersection to identify displaced
############################
  ssvids_displaced AS (
  SELECT
    DISTINCT(ssvid)
  FROM
    vessel_tracks AS tracks,
    geog AS pipa
  WHERE
    ST_INTERSECTS( ST_GeogPoint(tracks.lon,
        tracks.lat),
      pipa.g )
    AND date < TIMESTAMP("2015-01-01")
    AND fishing )
  #
  #
  #
  #
  ########
############################
# 6) Create table
############################
SELECT
  DISTINCT(ssvid) AS ssvid,
  (CASE
    WHEN ssvid IN (SELECT ssvid FROM ssvids_displaced) THEN "displaced"
    ELSE "non_displaced"
  END) AS `group`
FROM
  vessels_i_want
ORDER BY `group`, ssvid