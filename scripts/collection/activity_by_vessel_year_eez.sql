/* activity_by_vessel_year_eez */

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
  pna_fishing_vessels AS (
  SELECT
    SUM(e.fishing_hours) fishing_hours_in_PNA,
    SUM(e.hours) total_hours_in_PNA,
    ssvid,
    year
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear`
  CROSS JOIN
    UNNEST(activity.eez) AS e
  WHERE
    e.value IN (
    SELECT
      *
    FROM
      pna_eezs )
    AND on_fishing_list_best
    AND activity.offsetting IS FALSE
  GROUP BY
    ssvid,
    year
  HAVING
    fishing_hours_in_PNA > 10 ),
  #
  #
  #
  ####
  fishing_activity AS (
  SELECT
    year,
    e.value AS eez_code,
    ssvid,
    best.best_vessel_class AS best_vessel_class,
    best.best_flag AS flag,
    ais_identity.shipname_mostcommon.value AS broadcast_shipname,
    ais_identity.n_shipname_mostcommon.value AS broadcast_callsign,
    e.hours AS hours,
    activity.fishing_hours AS total_fishing_hours_in_year,
    activity.active_hours AS total_active_hours_in_year,
    activity.hours AS total_hours_in_year
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear`
  CROSS JOIN
    UNNEST(activity.eez) AS e
  WHERE
    best.best_vessel_class IN ("drifting_longlines",
      "tuna_purse_seines")),
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
    `ucsb-gfw.mpa_displacement.vessel_info_pna_and_pipa`)
  #
  #
  #
  #######
SELECT
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
  flag,
  broadcast_shipname,
  broadcast_callsign,
  hours,
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
JOIN vessel_groups
USING(ssvid,
    year)
LEFT JOIN
  eez_lookup
USING
  (eez_code)
ORDER BY
  year,
  fishing_hours_in_PNA DESC