SELECT
  EXTRACT(YEAR FROM date) AS year,
  FLOOR(lat * 1) * 1 + 0.5 AS lat_bin_center,
  FLOOR(lon * 1) * 1 + 0.5 AS lon_bin_center,
  `group`,
  sum(hours) AS hours
FROM
  `world-fishing-827.gfw_research.pipe_v20190502_fishing`
LEFT JOIN  `mpa_displacement.a_ssvids_displaced_and_not_displaced` USING (ssvid)
WHERE
  ssvid IN (
  SELECT
    ssvid
  FROM
    `mpa_displacement.a_ssvids_displaced_and_not_displaced`)
AND nnet_score > 0.5
AND date < TIMESTAMP("2019-01-01")
GROUP BY `group`, year, lon_bin_center, lat_bin_center