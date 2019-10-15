-- This gets all data within PNA countries
-- IE the number of points we have

SELECT
  COUNT(mmsi)
FROM
  [world-fishing-827.gfw_research.nn7]
WHERE
  mmsi IN (
  SELECT
    mmsi
  FROM
    [world-fishing-827.gfw_research.nn7]
  WHERE
    eez_iso3 IN ("FSM",
      "KIR",
      "MHL",
      "NRU",
      "PLW",
      "PNG",
      "SLB",
      "TUV")
    AND YEAR(timestamp) < 2015
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      [world-fishing-827.gfw_research.good_segments])
    AND mmsi IN (
    SELECT
      mmsi
    FROM
      [world-fishing-827.gfw_research.vessel_info]
    WHERE
      best_label = "purse_seines"))
