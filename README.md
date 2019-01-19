# Displacement of fishing effort by Large Scale Marine Protected Areas

## Overview

This repository contains the data used in Villaseñor-Derbez et al. The purpose is to guide anyone interested in replicating the analsyses through the process of getting the data and analysing it in the way we did. All data are made publically available in this repository.

## Citations

For citations of the raw Global Fishing Watch data (anything under the `raw_data` folder, please cite: [Kroodsma et al. "Tracking the global footprint of fisheries." Science 359.6378 (2018): 904-908.](http://science.sciencemag.org/content/359/6378/904.abstract)

For citations of our data (anything in the `data`) folder, please cite: Us, in the future.

## Contents

The repository contains five key directories:

- `raw_data`: Contains the original, raw data either from GFW or spatial information from EEZs and WDPA. These data have not been manipulated directly by us.
- `data`: The data folder contains transformed and processed data.
- `scripts`: This folder contains processing and analysis scripts. Essentially, a processing script reads data from `raw_data` or a cloud-based database, does something to it, and exports it to `data`. Some also export images to `img` or content to `docs`
- `img`: Contains images that go in the manuscript, supplementary materials, and exploratory figures we may have generated at some point.
- `docs`: Contains the `*.tex` files, bibliography, and `*.pdf` of the manuscript.

## Data gathering / processing

### Spatial cropped shapefiles

We have two large shapefiles, one for Marine Protected Areas from the [WDPA](https://www.protectedplanet.net/) and another one for EEZ from [MEOW](http://marineregions.org/). These data were manually downloaded from their sites in March and Sept 2018, respetively. I then used the following scripts to read them in, process them, and export them to the `data` folder:

- [`get_pna_eezs.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/get_pna_eezs.R) Subsets the EEZ shapefile (not on GitHub because of size) and keeps only PNA countries.
- [`scripts/get_pipa_shapefile.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/get_pipa_shapefile.R) Subsets the WDPA dataset (not on HitHub due to size) and exports only a shapefile for the Phoenix Islands Protected Area ([PIPA](https://www.protectedplanet.net/phoenix-islands-protected-area-protected-area))
- [`scripts/create_regions.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/create_regions.R): Subsets the EEZ shapefile (not on GitHub because of size) to get a subset with all EEZs where vessels fished, PIPA, as well as the High seas buffer.

### Identify the vessels we need to work with

#### Start in R

Using data from [Kroodsma et al. 2018](https://globalfishingwatch.org/datasets-and-code/fishing-effort/) and the shapefile for the PIPA we can identify which vessels ever fished inside PIPA. Using the same GFW data along with the [EEZ](http://marineregions.org/eezmapper.php) for PNA countries (and adyacent ones) we can identify which ones vished in the region. These data are all under the `data` folder. These are the scripts involved:

- [`scripts/vessels_inside_pipa.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessels_inside_pipa.R) identifies the mmsi numbers for vessels inside PIPA. It exports [a csv](https://github.com/jcvdav/MPA_displacement/blob/master/data/vessels_inside_pipa.csv) with `mmsi`, `year`, `month`, `total_fishing_hours` and `n_points`.
- [`scripts/vessels_inside_pna_oustide_pipa.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessels_inside_pna_outside_pipa.R) identifies the mmsi numbers for vessels inside PNA but outside PIPA. It exports [a csv](https://github.com/jcvdav/MPA_displacement/blob/master/data/vessels_inside_pna_outside_pipa.csv) with `mmsi`, `year`, `month`, `total_fishing_hours` and `n_points`.

#### Second step in SQL

Using the mmsis identified, I now query [Global Fishing Watch](www.globalfishingwatch.com/map) data from [Google BigQuery](https://bigquery.cloud.google.com) to get the mos updated data, as well as vessel info. There are two `SQL` scripts that do this:

- [`scripts/vessel_info_mmsi_inside_pipa.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_mmsi_inside_pipa.sql): Gets vessel info (`mmsi`, `year`, `inferred_label`, `label_score`, `inferred_label_allyears`, `iso3`)
- [`script/svessel_info_mmsi_inside_pna.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_mmsi_inside_pna.sql): Gets vessel info (`mmsi`, `year`, `inferred_label`, `label_score`, `inferred_label_allyears`, `iso3`)

I then stack (`UNION`) these two tables into one, using [`scripts/vessel_info_pna_and_pipa.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_info_pna_and_pipa.sql). This produces a table with the same name.

Once we have identified which vessels we need, as well as their characteristics, we can get their full tracks to have all their time at sea, travel time, travel distances as well as fishing time and locations. The query, found at [`scripts/vessel_tracks.sql`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_tracks.sql) gets the full tracks for all vessels that are in both groups, with some filtering (*.i.e.* GFW's "good segments"). This is all the stuff done in Google BigQuery, the rest is done in R.

#### Final steps in R

The last step is to download the data. Since the table is too big to directly export from Google BigQuery, I wrote a short R script ([`scripts/vessel_tracks_download.R`](https://github.com/jcvdav/MPA_displacement/blob/master/scripts/vessel_tracks_download.R)) that connects to GBQ and downloads the data. The script also creates a new variables, gets rid of the few duplicated rows (about 0.07% of Fishing hours)

### Vessel-level data

For some of our analyses, we need to have a dataset where we have vessel-level information. In this case, we want to have the 

## TO DO

- Make sure that for the spatial re-distribution, I am using the baci / experimental features that I defined for `effort_by_vessel.csv`.

--------- 

<a href="https://orcid.org/0000-0003-1245-589X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">orcid.org/0000-0003-1245-589X</a>
