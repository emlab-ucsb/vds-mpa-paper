###########################################################
# This query creates a gridded version of fishing hours by
# year-month within Palau, at a 0.1 degree resolution.
# It collects data for longliners and purse seiners.
###########################################################
#1) Query the data
WITH eez_info AS (
SELECT CAST(eez_id AS STRING) AS eez_id FROM `world-fishing-827.gfw_research.eez_info` WHERE sovereign1_iso3 = "PLW"
),
#
#
#
#
########
vessels_i_want AS (
SELECT ssvid,
best.best_vessel_class AS vessel_class,
best.best_length_m AS length_m,
  CASE
    WHEN best.best_length_m < 50 THEN 0.5
    WHEN best.best_length_m BETWEEN 50 AND 80 THEN 1
    ELSE 1.5
  END AS length_factor
FROM `world-fishing-827.gfw_research.vi_ssvid_v20190430` 
CROSS JOIN UNNEST (activity.eez) AS eez
WHERE eez.value IN (SELECT eez_id FROM eez_info)
AND best.best_vessel_class IN ("tuna_purse_seines", "drifting_longlines")
AND CAST(ssvid AS int64) NOT IN (
    SELECT
      ssvid
    FROM
      `world-fishing-827.gfw_research.bad_mmsi`
    CROSS JOIN
      UNNEST(ssvid) AS ssvid)                        # filter our bad mmsis
    AND ssvid NOT IN ("1")
    AND on_fishing_list_best
    AND NOT activity.offsetting
    AND activity.fishing_hours > 24
    AND activity.active_hours > 24
),
#
#
#
#
########
  intermediate AS (SELECT
    EXTRACT(YEAR
    FROM
      date) AS year,
    EXTRACT(MONTH
    FROM
      date) AS month,
      vessel_class,
      length_factor,
    CAST(FLOOR(lat / 0.1) * 0.1 AS NUMERIC) + 0.05 AS lat_bin_center,
    CAST(FLOOR(lon / 0.1) * 0.1 AS NUMERIC) + 0.05 AS lon_bin_center,
   (nnet_score > 0.5 AND nnet_score IS NOT NULL) AS fishing,
    SUM(hours) AS hours
  FROM
    `world-fishing-827.gfw_research.pipe_v20190502_fishing`
    CROSS JOIN UNNEST(regions.eez) AS eez
  LEFT JOIN vessels_i_want USING(ssvid)
  WHERE
    ssvid IN (
    SELECT
      ssvid
    FROM
      `vessels_i_want`)
    AND date < TIMESTAMP("2019-07-01")
    AND eez IN (SELECT eez_id FROM eez_info)
    AND distance_from_shore_m > 5 * 1854
    AND distance_from_port_m > 5 * 1854
    AND seg_id IN (SELECT seg_id FROM `world-fishing-827.gfw_research.pipe_production_v20190502_segs` WHERE good_seg AND NOT overlapping_and_short)
  GROUP BY
    year,
    month,
    vessel_class,
    fishing,
    length_factor,
    lon_bin_center,
    lat_bin_center)
    #
    #
    #
    #
    ########
    SELECT
  year,
  month,
  lat_bin_center,
  lon_bin_center,
  vessel_class,
  fishing,
  SUM(hours * length_factor) hours_length,
  SUM(hours) AS hours
FROM
  intermediate
GROUP BY
  year,
  month,
  lat_bin_center,
  lon_bin_center,
  vessel_class,
  fishing

#SELECT * FROM vessels_i_want