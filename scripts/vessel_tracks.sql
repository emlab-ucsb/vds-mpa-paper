/* Last run on 19/09/2018 */

SELECT
A.mmsi AS mmsi,
A.timestamp AS timestamp,
A.lon AS lon,
A.lat AS lat,
A.hours AS hours,
A.nnet_score AS nnet_score,
A.eez_iso3,
B.treated AS treated,
B.gear AS gear
FROM (
  SELECT
  mmsi,
  timestamp,
  lon,
  lat,
  hours,
  nnet_score,
  eez_iso3
  FROM
  `world-fishing-827.gfw_research.nn7`
  WHERE
  mmsi IN (
    SELECT
    mmsi
    FROM
    `ucsb-gfw.mpa_displacement.vessel_groups`)) A
JOIN
`ucsb-gfw.mpa_displacement.vessel_groups` B
ON
A.mmsi = B.mms