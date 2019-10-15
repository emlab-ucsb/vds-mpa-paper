###########################################################
# Get vessel info for all vessels
# This query extracts the observable vesel characteristics
# for all vessels that ever fished within PNA countries
# for the period 2012 - 2018.
####
# The subquery structure is as follows:
-- 1) Get a list of eez codes for PNA, based on ISO3 values
-- 2) Get a list of ssvids that fished in the above eez_id
-- 3) Get the vessel characteristics for each vessel
###
# The resulting table is saved at:
# `mpa_displacement.vessel_info_pna_purse_seines`
# And is also exported as a CSV to our repo
###########################################################

#################################
# This first subquery gets all
# the eez ids that correspond to
# the EEZ ISO3 codes for PNA countries
WITH
  eez_list AS (
  SELECT
    CAST(eez_id AS string) eez_id
  FROM
    `world-fishing-827.gfw_research.eez_info`
  WHERE
    territory1_iso3 IN ("FSM",
      "KIR",
      "MHL",
      "NRU",
      "PLW",
      "PNG",
      "SLB",
      "TUV")),
  #
  #
  #
  #
  ########
  #################################
  # I then extract the distinct
  # vessel identifiers for vessels
  # that fished in ther eez_ids identifed
  # above
  my_vessels AS (
  SELECT
    DISTINCT(ssvid)
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  CROSS JOIN
    UNNEST(activity.eez) AS eez
  WHERE
    eez.value IN (
    SELECT
      eez_id
    FROM
      eez_list)
    AND eez.fishing_hours > 1
    AND best.best_vessel_class = "tuna_purse_seines"
    AND year < 2019
    AND ssvid NOT IN ("1"))
  #
  #
  #
  #
  ########
  #################################
  # We can then get the information
  # each of these vessels.
SELECT
  ssvid,
  best.best_flag AS flag,
  best.best_length_m AS length_m,
  best.best_tonnage_gt AS tonnage_gt,
  best.best_engine_power_kw AS engine_power_kw,
  best.best_crew_size AS crew_size
FROM
  `world-fishing-827.gfw_research.vi_ssvid_v20190430`
WHERE
  ssvid IN (
  SELECT
    ssvid
  FROM
    my_vessels)
ORDER BY ssvid