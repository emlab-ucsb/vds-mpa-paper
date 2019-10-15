/* Last run on 18/01/2018 */

/* Last run on 19/01/2018 */

SELECT
  A.year AS year,
  A.month AS month,
  A.day AS day,
  A.timestamp AS timestamp,
  A.mmsi AS mmsi,
  A.lon AS lon,
  A.lat AS lat,
  A.hours AS hours,
  A.nnet_score AS nnet_score,
  A.eez_iso3,
  A.distance_from_port_m AS distance_from_port,
  A.distance_from_shore_m AS distance_from_shore,
  A.seg_id AS seg_id,
  B.treated AS treated,
  B.best_label AS inferred_label,
  B.iso3 AS flag
FROM (
  SELECT
    EXTRACT(year FROM timestamp) AS year,
    EXTRACT(month FROM timestamp) AS month,
    EXTRACT(day FROM timestamp) AS day,
    timestamp,
    ssvid AS mmsi,
    lon,
    lat,
    hours,
    nnet_score,
    regions.eez AS eez_iso3,
    distance_from_port_m,
    distance_from_shore_m,
    seg_id
  FROM
    `world-fishing-827.gfw_research.pipe_production_b_fishing`
  WHERE
    ssvid IN (
    SELECT
      CAST(mmsi as STRING) AS mmsi
    FROM
      `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`)
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      `world-fishing-827.gfw_research.good_segments`)) A
JOIN
  (SELECT
      year,
      CAST(mmsi AS STRING) AS mmsi,
      treated,
      best_label, 
      iso3
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`) B
ON
  A.mmsi = B.mmsi
  AND A.year = B.year