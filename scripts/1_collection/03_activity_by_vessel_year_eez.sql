###########################################################
# This query gets different measures of fishing and activity
# for each of the 318 vessels that we are interested in.
# The query proceeds as follws:
# 1) Subquery of PNA EEz info
# 2) Subquery with activity by vessel for each PNA eez
# 3) Subquery with general activity by vessel for all eezs
# 4) Lookup table for all EEZs
# 5) Combine everything
###########################################################
#
#
#
#
########
# 1) Subquery of PNA EEz info
WITH
  pna_eezs AS (
  SELECT
    CAST(eez_id AS string)
  FROM
    `world-fishing-827.gfw_research.eez_info`
  WHERE
    territory1_iso3 IN ('FSM',
      'KIR',
      'MHL',
      'NRU',
      'PLW',
      'PNG',
      'SLB',
      'TUV',
      'TKL')),
  #
  #
  #
  #
  ########
  # 2) Subquery with activity by vessel for each PNA eez
  pna_fishing_vessels AS (
  SELECT
    SUM(e.fishing_hours) fishing_hours_in_PNA,
    SUM(e.hours) total_hours_in_PNA,
    ssvid,
    year
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  CROSS JOIN
    UNNEST(activity.eez) AS e
  WHERE
    e.value IN (
    SELECT
      *
    FROM
      pna_eezs )
    AND ssvid IN (SELECT ssvid FROM `mpa_displacement.a_ssvids_displaced_and_not_displaced`)
    AND year < 2019
  GROUP BY
    ssvid,
    year),
  #
  #
  #
  #
  ########
  # 3) Subquery with general activity by vessel for all eezs
  fishing_activity AS (
  SELECT
    year,
    IF(e.value IS NULL,
      NULL,
      e.value) eez_code,
    ssvid,
    e.hours AS hours,
    e.fishing_hours AS fishing_hours,
    activity.fishing_hours AS total_fishing_hours_in_year,
    activity.active_hours AS total_active_hours_in_year,
    activity.hours AS total_hours_in_year
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  CROSS JOIN
    UNNEST(activity.eez) AS e
  WHERE ssvid IN (SELECT ssvid FROM `mpa_displacement.a_ssvids_displaced_and_not_displaced`)),
  #
  #
  #
  #
  #########
  # 4) Lookup table for all EEZs
  eez_lookup AS (
  SELECT
    CAST(eez_id AS string) eez_code,
    territory1_iso3 eez_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info`),
  #
  #
  #
  #
  #######
  # 5) Combine everything
results AS (SELECT
  year,
  IF (eez_code IS NULL,
    "HS",
    eez_code) AS eez_code,
  IF (eez_iso3 IS NULL,
    "HS",
    eez_iso3) AS eez_iso3,
  ssvid,
  `group`,
  `group` = "displaced" AS treated,
  flag, length_m, tonnage_gt, engine_power_kw, crew_size,
  length_factor,
  hours,
  fishing_hours,
  hours * length_factor AS hours_length,
  fishing_hours_in_PNA,
  total_hours_in_PNA,
  total_fishing_hours_in_year,
  total_active_hours_in_year,
  total_hours_in_year
FROM
  fishing_activity
JOIN
  pna_fishing_vessels
USING
  (ssvid,
    year)
LEFT JOIN `mpa_displacement.a_vessel_info_pna_purse_seines` USING (ssvid)
LEFT JOIN
  eez_lookup
USING
  (eez_code)
ORDER BY
  year,
  fishing_hours_in_PNA DESC)
  #
  #
  #
  #
  ########
  
  SELECT * FROM results ORDER BY ssvid