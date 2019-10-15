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
    AND e.fishing_hours > 1
    AND on_fishing_list_best
    AND ssvid NOT IN ("1")
    AND year < 2019
  GROUP BY
    ssvid,
    year),
  #
  #
  #
  ####
  fishing_activity AS (
  SELECT
    year,
    IF(e.value IS NULL,
      NULL,
      e.value) eez_code,
    ssvid,
    best.best_vessel_class AS best_vessel_class,
    best.best_length_m AS best_length,
    best.best_flag AS flag,
    ais_identity.shipname_mostcommon.value AS broadcast_shipname,
    ais_identity.n_shipname_mostcommon.value AS broadcast_callsign,
    e.hours AS hours,
    activity.fishing_hours AS total_fishing_hours_in_year,
    activity.active_hours AS total_active_hours_in_year,
    activity.hours AS total_hours_in_year
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  CROSS JOIN
    UNNEST(activity.eez) AS e
  WHERE
    best.best_vessel_class IN ("tuna_purse_seines")),
  #
  #
  #
  #########
  eez_lookup AS (
  SELECT
    CAST(eez_id AS string) eez_code,
    territory1_iso3 eez_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info`),
  #
  #
  #
  #######
  vessel_groups AS (
  SELECT
    year,
    CAST(mmsi AS string) AS ssvid,
    treated
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`),
  #
  #
  #
  #######
results AS (SELECT
  year,
  IF (eez_code IS NULL,
    "HS",
    eez_code) AS eez_code,
  IF (eez_iso3 IS NULL,
    "HS",
    eez_iso3) AS eez_iso3,
  ssvid,
  treated,
  best_vessel_class,
  best_length,
  CASE
    WHEN best_length < 50 THEN 0.5
    WHEN best_length BETWEEN 50 AND 80 THEN 1
    ELSE 1.5
  END AS length_factor,
  flag,
  broadcast_shipname,
  broadcast_callsign,
  hours,
  CASE
    WHEN best_length < 50 THEN 0.5 * hours
    WHEN best_length BETWEEN 50 AND 80 THEN 1 * hours
    ELSE 1.5 * hours
  END AS hours_length,
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
LEFT JOIN
  vessel_groups
USING
  (ssvid,
    year)
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