WITH
  kir_eez_info AS (
  SELECT
    CAST(eez_id AS STRING) eez_id,
    sovereign1_iso3 AS eez_iso3,
    territory1
  FROM
    `world-fishing-827.gfw_research.eez_info`
  WHERE
    sovereign1_iso3 = "KIR"),
  #
  #
  #
  #
  ########
  vessel_info AS (
  SELECT
    year,
    ssvid,
    eez.value AS eez_id,
    best.best_vessel_class AS best_vessel_class
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear` CROS
  JOIN
    UNNEST(activity.eez) AS eez
  WHERE
    eez.value IN (
    SELECT
      eez_id
    FROM
      kir_eez_info)
    AND best.best_vessel_class IN ("drifting_longlines",
      "tuna_purse_seines")),
  #
  #
  #
  #
  ########
  tracks AS (
  SELECT
    EXTRACT(YEAR
    FROM
      _Partitiontime) AS year,
    EXTRACT(MONTH
    FROM
      _Partitiontime) AS month,
    eez_iso3,
    territory1,
    ssvid,
    SUM(hours) AS hours
  FROM
    `world-fishing-827.gfw_research.pipe_production_b_fishing` CROS
  JOIN
    UNNEST(regions.eez) AS eez_id
  JOIN
    kir_eez_info
  USING
    (eez_id)
  WHERE
    ssvid IN (
    SELECT
      ssvid
    FROM
      vessel_info)
    AND eez_id IN (
    SELECT
      eez_id
    FROM
      kir_eez_info)
  GROUP BY
    year,
    month,
    ssvid,
    eez_iso3,
    territory1)
  #
  #
  #
  #
  ########
SELECT
  year,
  month,
  eez_iso3,
  territory1,
  best_vessel_class,
  SUM(hours) AS hours
FROM
  tracks
JOIN
  vessel_info
USING
  (year,
    ssvid)
GROUP BY
  year,
  month,
  eez_iso3,
  territory1,
  best_vessel_class
  ORDER BY
    year,
    month,
    best_vessel_class