  ###########################################################
  # Get vessel info for all vessels
  # This query extracts the observable vesel characteristics
  # for all vessels that ever fished within PNA countries
  # for the period 2012 - 2018.
  #
  ###
  #
  # The resulting table is saved at:
  # `mpa_displacement.vessel_info_pna_purse_seines`
  # And is also exported as a CSV to our repo
  #
  ###########################################################
SELECT
  ssvid,
  best.best_flag AS flag,
  best.best_length_m AS length_m,
  CASE
    WHEN best.best_length_m < 50 THEN 0.5
    WHEN best.best_length_m BETWEEN 50 AND 80 THEN 1
    ELSE 1.5
  END AS length_factor,
  best.best_tonnage_gt AS tonnage_gt,
  best.best_engine_power_kw AS engine_power_kw,
  best.best_crew_size AS crew_size,
  `group`
FROM
  `world-fishing-827.gfw_research.vi_ssvid_v20190430`
LEFT JOIN
  `mpa_displacement.a_ssvids_displaced_and_not_displaced`
USING
  (ssvid)
WHERE
  ssvid IN (
  SELECT
    ssvid
  FROM
    `mpa_displacement.a_ssvids_displaced_and_not_displaced`)
ORDER BY
  ssvid