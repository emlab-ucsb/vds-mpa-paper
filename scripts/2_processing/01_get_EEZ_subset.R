
#################################################
# Take a shapefile of EEZs and filter out to keep
# only some EEZs.
##################################################

# Needed libraries
library(here)
library(sf)
library(rmapshaper)
library(tidyverse)

# Source local functions
source(here("scripts", "0_functions", "st_rotate.R"))

# ISO3 codes for countries I need
iso_codes <- c("KIR",
               "ASM",
               "COK",
               "FSM",
               "MHL",
               "NRU",
               "PNG",
               "SLB",
               "TKL",
               "TUV",
               "UMI",
               "FJI",
               "NIU",
               "TON",
               "WSM",
               "WLF",
               "VUT",
               "NCL",
               "PLW",
               "IDN")

# Delete any previous files
file.remove(here("data", "spatial", "EEZ_subset.gpkg"))

# Read, filter, simplyfy, export
st_read(dsn = here("raw_data", "spatial", "EEZ"),
        layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% iso_codes) %>% 
  ms_simplify(keep_shapes = T) %>% 
  st_rotate() %>% 
  group_by(ISO_Ter1) %>% 
  summarize() %>% 
  st_write(here("data", "spatial", "EEZ_subset.gpkg"))

# END OF SCRIPT
