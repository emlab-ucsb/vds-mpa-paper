# Displacement of fishing effort by Large Scale Marine Protected Areas

## Data gatehring /creation

### Spatial cropped shapefiles

We have two large shapefiles, one for Marine Protected Areas from the [WDPA](https://www.protectedplanet.net/) and another one for EEZ from [MEOW](http://marineregions.org/). These data were manually downloaded from their sites in March and Sept 2018, respetively. I then used the following scripts to read them in, process them, and export them to the `data` folder:

- [`get_pna_eezs.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/get_pna_eezs.R) Subsets the EEZ shapefile (not on GitHub because of size) and keeps only PNA countries.
- [`scripts/get_pipa_shapefile.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/get_pipa_shapefile.R) Subsets the WDPA dataset (not on HitHub due to size) and exports only a shapefile for the Phoenix Islands Protected Area ([PIPA](https://www.protectedplanet.net/phoenix-islands-protected-area-protected-area))
- [`scripts/create_regions.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/create_regions.R): Subsets the EEZ shapefile (not on GitHub because of size) to get a subset with all EEZs where vessels fished, PIPA, as well as the High seas buffer.

### Identify the vessels we need to work with

Using data from [Kroodsma et al. 2018](https://globalfishingwatch.org/datasets-and-code/fishing-effort/) and the shapefile for the PIPA we can identify which vessels ever fished inside PIPA. Using the same GFW data along with the [EEZ](http://marineregions.org/eezmapper.php) for PNA countries (and adyacent ones) we can identify which ones vished in the region. These data are all under the `data` folder. These are the scripts involved:

- [`scripts/vessels_inside_pipa.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessels_inside_pipa.R) identifies the mmsi numbers for vessels inside PIPA. It exports [a csv](https://github.com/jcvdav/MPA_displacement/blob/master/data/vessels_inside_pipa.csv) with `mmsi`, `year`, `month`, `total_fishing_hours` and `n_points`.
- [`scripts/vessels_inside_pna_oustide_pipa.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessels_inside_pna_outside_pipa.R) identifies the mmsi numbers for vessels inside PNA but outside PIPA. It exports [a csv](https://github.com/jcvdav/MPA_displacement/blob/master/data/vessels_inside_pna_outside_pipa.csv) with `mmsi`, `year`, `month`, `total_fishing_hours` and `n_points`.

Using the mmsis identified, I now query [Global Fishing Watch](www.globalfishingwatch.com/map) data from [Google BigQuery](https://bigquery.cloud.google.com) to get the mos updated data, as well as vessel info. There are two `SQL` scripts that do this:

- [`scripts/vessel_info_mmsi_inside_pipa.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_mmsi_inside_pipa.sql): Gets vessel info (`mmsi`, `year`, `inferred_label`, `label_score`, `inferred_label_allyears`, `iso3`)
- [`script/svessel_info_mmsi_inside_pna.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_mmsi_inside_pna.sql): Gets vessel info (`mmsi`, `year`, `inferred_label`, `label_score`, `inferred_label_allyears`, `iso3`)

I then stack (`UNION`) these two tables into one, using `[scripts/vessel_info_pna_and_pipa.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_pna_and_pipa.sql). This produces a table with the same name.

Once we have identified which vessels we need, as well as their characteristics, we can get their full tracks to have all their time at sea, travel time, travel distances as well as fishing time and locations. At the same time, we join them to the `vessel_info_pna_and_pipa` table. The query, found at [`scripts/vessel_tracks.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_tracks.sql) gets the full tracks for all vessels that are in both groups, with some filtering (*.i.e.* GFW's [good segments]())

## Repository structure 

```
-- data
   |__displacement_data.rds
   |__displacement_data_ps.rds
   |__effort_by_vessel.csv
   |__effort_by_vessel.rds
   |__mmsi_inside_pipa
   |__pipa_effort_by_vessel.csv
   |__pna_mmsis.csv
   |__pna_vessels_outside_pipa.csv
   |__spatial
   |__USMonuments
   |__vessel_groups.csv
   |__vessel_info_mmsi_inside_pipa.csv
   |__vessel_info_mmsi_inside_pna.csv
   |__vessels_inside_pipa.csv
-- derby.log
-- DID.html
-- docs
   |__Manuscript.pdf
   |__Manuscript.tex
   |__Manuscript_files
-- img
   |__2015-2016.JPG
   |__BACI_gear.png
   |__BACI_relaxed_gear.png
   |__BACI_strict_gear.png
   |__Fig1a.pdf
   |__Fig1b.pdf
   |__Fig1c.pdf
   |__Fig1SPF.R
   |__today.JPG
-- Lit Review
-- logs
   |__log4j.spark.log
-- months.html
-- MPA_displacement.Rproj
-- quarters.html
-- raw_data
   |__gfw_daily
   |__spatial
   |__vessel_tracks.rds
-- README.md
-- rmd
   |__Lit Review.Rmd
   |__Lit_Review.pdf
   |__Manuscript.pdf
   |__Manuscript.Rmd
   |__Manuscript.tex
   |__Manuscript_files
   |__references.bib
-- scripts
   |__combine_vessel_info_and_effort.R
   |__displacement_data.R
   |__displacement_data_ll.R
   |__displacement_data_ps.R
   |__get_EEZ_subset.R
   |__get_pipa_shapefile.R
   |__get_pna_eezs.R
   |__pipa_effort_by_vessel.R
   |__pna_vessels_outside_pipa.R
   |__sfc_as_cols.R
   |__spatial_redistribution.R
   |__st_rotate.R
   |__vessel_info_mmsi_inside_pipa.sql
   |__vessel_info_mmsi_inside_pna.sql
   |__vessel_tracks.sql
   |__vessel_tracks_download.R
   |__vessels_inside_pipa.R
-- slides
   |__ERE_lunch.aux
   |__ERE_lunch.out
   |__ERE_lunch.pdf
   |__ERE_lunch.Rmd
   |__img
   |__references.bib
   |__small_data
```

--------- 

<a href="https://orcid.org/0000-0003-1245-589X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">orcid.org/0000-0003-1245-589X</a>
