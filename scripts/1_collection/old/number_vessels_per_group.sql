-- Number of data points (26084414)
SELECT
  COUNT(mmsi)
FROM
  `ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE mmsi IN (
  SELECT
    mmsi
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines") OR mmsi IN (
  SELECT
    mmsi
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines")

-- number of vessels BEFORE (62)
SELECT
  COUNT(DISTINCT(mmsi))
FROM
  `ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
  timestamp < TIMESTAMP("2014-09-01")
  AND mmsi IN (
  SELECT
    mmsi
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines")


-- number of vessels AFTER (64)
SELECT
  COUNT(DISTINCT(mmsi))
FROM
  `ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp > TIMESTAMP("2015-01-01")
  AND mmsi IN (
  SELECT
    mmsi
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines")

-- number of vessels present INSIDE PIPA BEFORE AND AFTER (61)
SELECT
COUNT(DISTINCT(mmsi))
FROM
`ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp < TIMESTAMP("2014-09-01")
AND mmsi IN (
  SELECT
  mmsi
  FROM
  `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines")
AND mmsi IN (SELECT
             DISTINCT(mmsi)
             FROM
             `ucsb-gfw.mpa_displacement.mmsis_by_group`
             WHERE
             timestamp > TIMESTAMP("2015-01-01")
             AND mmsi IN (
               SELECT
               mmsi
               FROM
               `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines"))

-- vessels present INSIDE PIPA BEFORE but NOT AFTER (1) mmsi is 444042912
SELECT 
COUNT(DISTINCT(mmsi))
FROM
`ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp < TIMESTAMP("2014-09-01")
AND mmsi IN (
  SELECT
  mmsi
  FROM
  `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines")
AND mmsi NOT IN (SELECT
             DISTINCT(mmsi)
             FROM
             `ucsb-gfw.mpa_displacement.mmsis_by_group`
             WHERE
             timestamp > TIMESTAMP("2015-01-01")
             AND mmsi IN (
               SELECT
               mmsi
               FROM
               `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`
  WHERE
    inferred_label_allyears = "purse_seines"))
               
-- vessels NOT inside PIPA and BEFOR (32)
SELECT
  COUNT(DISTINCT(mmsi))
FROM
  `ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
  timestamp < TIMESTAMP("2014-09-01")
  AND mmsi IN (
  SELECT
    mmsi
  FROM
    `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines")

-- vessels NOT inside PIPA AFTER (38)
SELECT
COUNT(DISTINCT(mmsi))
FROM
`ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp > TIMESTAMP("2015-01-01")
AND mmsi NOT IN (
  SELECT
  mmsi
  FROM
  `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pipa`) 

-- vessel outside PIPA BEFORE AND AFTER (28)
SELECT
COUNT(DISTINCT(mmsi))
FROM
`ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp < TIMESTAMP("2014-09-01")
AND mmsi IN (
  SELECT
  mmsi
  FROM
  `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines")
AND mmsi IN (SELECT
             DISTINCT(mmsi)
             FROM
             `ucsb-gfw.mpa_displacement.mmsis_by_group`
             WHERE
             timestamp > TIMESTAMP("2015-01-01")
             AND mmsi IN (
               SELECT
               mmsi
               FROM
               `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines"))
  
-- vessels outside PIPA BEFORE NOT AFTER (4)
SELECT
COUNT(DISTINCT(mmsi))
FROM
`ucsb-gfw.mpa_displacement.mmsis_by_group`
WHERE
timestamp < TIMESTAMP("2014-09-01")
AND mmsi IN (
  SELECT
  mmsi
  FROM
  `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines")
AND mmsi NOT IN (SELECT
             DISTINCT(mmsi)
             FROM
             `ucsb-gfw.mpa_displacement.mmsis_by_group`
             WHERE
             timestamp > TIMESTAMP("2015-01-01")
             AND mmsi IN (
               SELECT
               mmsi
               FROM
               `ucsb-gfw.mpa_displacement.vessel_info_mmsi_inside_pna`
  WHERE
    inferred_label_allyears = "purse_seines"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  