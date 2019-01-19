SELECT
  mmsi,
  year,
  inferred_label,
  label_score,
  inferred_label_allyears,
  iso3,
  IF(mmsi IN (SELECT mmsi FROM `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`), TRUE, FALSE) AS treated
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