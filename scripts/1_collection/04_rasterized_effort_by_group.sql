###########################################################
# This query creates a gridded version of fishing hours by
# group of vessels, accounting for the lengths of a vessel
# in the same way as the PNA does
# The query proceeds as follws:
# 1) Subquery the raw data to keep only the 318 vessels we 
#   track, and join to the vessel_info table (this has length)
# 2) Use the length factor to correct for the data
###########################################################
#1) Query the data
WITH
  a AS (
  SELECT
    EXTRACT(YEAR
    FROM
      date) AS year,
    CAST(FLOOR(lat / 1) * 1 AS NUMERIC) + 0.5 AS lat_bin_center,
    CAST(FLOOR(lon / 1) * 1 AS NUMERIC) + 0.5 AS lon_bin_center,
    `group`,
    length_factor,
    SUM(hours) AS hours
  FROM
    `world-fishing-827.gfw_research.pipe_v20190502_fishing`
  LEFT JOIN
    `mpa_displacement.a_vessel_info_pna_purse_seines`
  USING
    (ssvid)
  WHERE
    ssvid IN (
    SELECT
      ssvid
    FROM
      `mpa_displacement.a_ssvids_displaced_and_not_displaced`)
    AND nnet_score > 0.5                                            # KEEP ONLY FISHING EVENTS
    AND date < TIMESTAMP("2019-01-01")
  GROUP BY
    `group`,
    year,
    length_factor,
    lon_bin_center,
    lat_bin_center)
    #
    #
    #
    #
    ########
    #2) Apply the length-correction
SELECT
  year,
  lat_bin_center,
  lon_bin_center,
  `group`,
  SUM(hours * length_factor) hours_length,
  SUM(hours) AS hours
FROM
  a
GROUP BY
  year,
  lat_bin_center,
  lon_bin_center,
  `group`