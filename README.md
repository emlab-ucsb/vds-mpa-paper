# Well-Designed Fishery Markets Enable Large Scale Marine Conservation

## Overview

This repository contains the data used in Villaseñor-Derbez et al. The purpose is to guide anyone interested in replicating the analsyses through the process of getting the data and analysing it in the way we did. All data are made publically available in this repository.

## Citations

For citations of the raw Global Fishing Watch data (anything under the `raw_data` folder, please cite: [Kroodsma et al. "Tracking the global footprint of fisheries." Science 359.6378 (2018): 904-908.](http://science.sciencemag.org/content/359/6378/904.abstract)

For citations of our data (anything in the `data`) folder, please cite thiw work directly.

## Contents

The repository contains five key directories:

- `raw_data`: Contains the original, raw data either from GFW or spatial information from EEZs and WDPA. These data have not been manipulated directly by us.
- `data`: The data folder contains transformed and processed data.
- `scripts`: This folder contains processing and analysis scripts. Essentially, a processing script reads data from `raw_data` or a cloud-based database, does something to it, and exports it to `data`. Some also export images to `img` or content to `docs`
- `img`: Contains images that go in the manuscript, supplementary materials, and exploratory figures we may have generated at some point.
- `docs`: Contains the `*.tex` files, bibliography, and `*.pdf` of the manuscript.

The key files to run the model are located in `scripts/model/*`, and can be seen as html documents of docuemented code. Key scripts that generate content are under `scripts/content/*`.

--------- 


<a href="https://orcid.org/0000-0003-1245-589X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">orcid.org/0000-0003-1245-589X</a>
