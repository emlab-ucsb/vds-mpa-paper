-- This gets tracks for all Taiwanese purse seiners. Vessels that continously fished must still be identified, but I can do that in R
SELECT
  A.mmsi AS mmsi,
  A.timestamp AS timestamp,
  A.lon AS lon,
  A.lat AS lat,
  A.hours AS hours,
  A.nnet_score AS nnet_score,
  A.eez_iso3,
  A.distance_from_port AS distance_from_port,
  A.distance_from_shore AS distance_from_shore,
  A.seg_id AS seg_id,
  FALSE AS treated,
  B.gear AS gear,
  "TWN" AS flag
FROM (
  SELECT
    mmsi,
    timestamp,
    lon,
    lat,
    hours,
    nnet_score,
    eez_iso3,
    distance_from_port,
    distance_from_shore,
    seg_id
  FROM
    [world-fishing-827.gfw_research.nn7]
  WHERE
  YEAR(timestamp) < 2018 --tracks before 2018
  AND
  mmsi IN (SELECT mmsi FROM [world-fishing-827.gfw_research.vessel_info] WHERE iso3 = "TWN" AND best_label = "purse_seines") --vessels that are from Taiwan and are purse seiners
  AND
  mmsi NOT IN(SELECT mmsi FROM [ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa]) --vessels that never fished inside PIPA
  AND
  seg_id IN ( SELECT seg_id FROM [world-fishing-827.gfw_research.good_segments]) -- vessels that have good segments
  ) A
JOIN (
  SELECT
    mmsi,
    best_label AS gear
  FROM
    [world-fishing-827.gfw_research.vessel_info]
  WHERE
    iso3 = "TWN") B
ON
  A.mmsi = B.mmsi