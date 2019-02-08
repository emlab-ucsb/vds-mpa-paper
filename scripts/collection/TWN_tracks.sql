  -- This gets tracks for all Taiwanese purse seiners. Vessels that continously fished must still be identified, but I can do that in R
SELECT
  A.mmsi AS mmsi,
  A.year,
  A.month,
  A.day,
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
    EXTRACT(YEAR
    FROM
      timestamp) AS year,
    EXTRACT(MONTH
    FROM
      timestamp) AS month,
    EXTRACT(DAY
    FROM
      timestamp) AS day,
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
    `world-fishing-827.gfw_research.nn7`
  WHERE
    mmsi IN (
    SELECT
      DISTINCT(mmsi)
    FROM
      `world-fishing-827.gfw_research.vessel_info_20181002`
    WHERE
      best_flag = "TWN"
      AND best_label = "tuna_purse_seines") --vessels that are from Taiwan and are tuna purse seiners
    AND mmsi NOT IN(
    SELECT
      mmsi
    FROM
      `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`) --vessels that never fished inside PNA before 2015
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      `world-fishing-827.gfw_research.good_segments`) -- vessels that have good segments
    ) A
JOIN (
  SELECT
    mmsi,
    year,
    best_label AS gear
  FROM
    `world-fishing-827.gfw_research.vessel_info_20181002`
  WHERE
    best_flag = "TWN") B
ON
  A.mmsi = B.mmsi
  AND A.year = B.year