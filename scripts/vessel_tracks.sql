/* Last run on 14/01/2018 */

SELECT
  A.mmsi AS mmsi,
  A.year,
  A.timestamp AS timestamp,
  A.lon AS lon,
  A.lat AS lat,
  A.hours AS hours,
  A.nnet_score AS nnet_score,
  A.eez_iso3,
  A.distance_from_port AS distance_from_port,
  A.distance_from_shore AS distance_from_shore,
  A.seg_id AS seg_id,
  B.treated AS treated,
  B.gear AS gear,
  B.flag AS flag
FROM (
  SELECT
    mmsi,
    timestamp,
    EXTRACT(year FROM timestamp) as year,
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
      `ucsb-gfw.mpa_displacement.vessel_groups`)
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      `world-fishing-827.gfw_research.good_segments`)) A
JOIN
  `ucsb-gfw.mpa_displacement.vessel_groups` B
ON
  A.mmsi = B.mmsi AND A.year = B.year