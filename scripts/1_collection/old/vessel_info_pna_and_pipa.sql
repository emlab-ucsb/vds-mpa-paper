/* Last run Oct 14, 2019 */

SELECT
  mmsi,
  IF(mmsi IN (
    SELECT
      mmsi
    FROM
      `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`),
    TRUE,
    FALSE) AS treated
FROM (
  SELECT
    *
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  UNION ALL
  SELECT
    *
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`)