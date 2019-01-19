/* Last run on 18/01/2018 */

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
  A.distance_from_port AS distance_from_port,
  A.distance_from_shore AS distance_from_shore,
  A.seg_id AS seg_id,
  B.treated AS treated,
  B.inferred_label AS inferred_label,
  B.label_score AS label_score,
  B.iso3 AS flag
FROM (
  SELECT
    EXTRACT(year FROM timestamp) AS year,
    EXTRACT(month FROM timestamp) AS month,
    EXTRACT(day FROM timestamp) AS day,
    timestamp,
    mmsi,
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
      mmsi
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
      mmsi,
      treated,
      inferred_label, 
      label_score,
      iso3
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`) B
ON
  A.mmsi = B.mmsi
  AND A.year = B.year