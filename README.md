# Environmental Market Design for Large-Scale Marine Conservation

## Overview

This repository contains the data used in Villaseñor-Derbez et al. The purpose is to guide anyone interested in replicating the analsyses through the process of getting the data and analysing it in the way we did. All data are made publically available in this repository.

## Citations

For citations of the raw Global Fishing Watch data (anything under the `raw_data` folder, please cite: [Kroodsma et al. "Tracking the global footprint of fisheries." Science 359.6378 (2018): 904-908.](http://science.sciencemag.org/content/359/6378/904.abstract)

For citations of our data (anything in the `data` folder), please cite this work directly.

## Contents

The repository contains four key directories:

- `raw_data`: Contains the original, raw data either from GFW or the FFA. These data have not been manipulated directly by us. GFW data is collected by SQL scripts outlined in `scripts/01_collection\`.
- `data`: The data folder contains transformed and processed data. Scripts that take raw data and clean it are found in `scripts/2_processing/`.
- `scripts`: This folder contains a set of helper functions, processing and analysis scripts, and content-generating scripts. Essentially, a processing script reads data from `raw_data` or a cloud-based database, does something to it, and exports it to `data`.
- `docs`: Contains the `*.tex` files, figures, tables, bibliography, and supplementary information of the manuscript.

## Repository structure 

```
-- data
   |__financial_data.csv
   |__spatial
-- docs
   |__img
   |__tab
   |__Markets_Conservation_final.tex
   |__nature.bst
   |__Supplementary_materials.aux
   |__Supplementary_materials.bbl
   |__Supplementary_materials.blg
   |__Supplementary_materials.log
   |__Supplementary_materials.out
   |__Supplementary_materials.pdf
   |__Supplementary_materials.synctex.gz
   |__Supplementary_materials.tex
-- raw_data
   |__FFA
   |__activity_by_vessel_year_eez.rds
   |__rasterized_effort_by_group.rds
   |__rasterized_effort_plw_by_gear.rds
   |__vessel_info_pna_purse_seines.rds
-- scripts
   |__0_functions
   |__1_collection
   |__2_processing
   |__3_model
   |__4_content
```

--------- 

<a href="https://orcid.org/0000-0003-1245-589X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">orcid.org/0000-0003-1245-589X</a>
