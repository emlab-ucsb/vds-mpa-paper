#####################################################
##### Load and summarize GFW data with sparklyr #####
#####################################################
# This script loads all raw GFW data from different #
# CSV files found in raw_data/gfw_daily. All of     #
# these files have the same structure. This script  #
# will identify all vessels that fished inside PNA  #
# countries, but no PIPA                            #
#####################################################

########################################################################
################### About the raw data #################################
########################################################################
# From https://globalfishingwatch.force.com/gfw/s/data-download:       #
#                                                                      #
# Daily Fishing Effort at 10th Degree Resolution by MMSI, 2012-2016    #
# Fishing effort is binned into grid cells 0.1 degrees on a side,      #
# and measured in units of hours. The time is calculated by assigning  #
# an amount of time to each AIS detection (which is half the time      #
# to the previous plus half the time to the next AIS position). To get #
# information on each mmsi, see Global Fishing Watch data on fishing   #
# vessels.                                                             #
#                                                                      #
# More documentation:                                                  #
# https://github.com/GlobalFishingWatch/global-footprint-of-fisheries/blob/master/data_documentation/fishing_effort_byvessel.md

## Load packages
library(sparklyr) #install.packages("sparlyr")
library(here) #install.packages("here")
library(sf) #install.packages("sf")
library(purrr) #install.packages("purrr")
library(magrittr) #install.packages("magrittr")
library(dplyr) #install.packages("dplyr")

## Clear the environment
rm(list = ls())

source(here("scripts","sfc_as_cols.R"))

## Install spark on computer (ncomment next line)
# sparklyr::spark_install()

## Establish a connection on sparklyr
sc <- spark_connect(master = "local")


## Read the first rows of a sample dataset of January 1st 2012
gfw <- read.csv(here("raw_data","gfw_daily","2012-01-01.csv"), nrows = 10) %>% 
  map(function(x){"character"})


## Load all data into sparkly local session
gfw_data <- spark_read_csv(sc = sc,
                           name = "gfw_data",
                           path = here("raw_data","gfw_daily"),
                           columns = gfw,
                           infer_schema = F)
# Get pipa vessels
pipa_vessels <- read.csv(here::here("data", "vessels_inside_pipa.csv"))

mmsis <- sort(unique(pipa_vessels$mmsi))

eez_pna <- read_sf(dsn = here::here("data", "spatial", "PNA_EEZ", "PNA_EEZ.shp"))

# First, get mmsis of PNA-fishing vessels
pna_mmsis <- gfw_data %>% 
  dplyr::select(date, lat_bin, lon_bin, fishing_hours, mmsi) %>% 
  mutate(year = SUBSTRING(date, 1, 4),
         month = SUBSTRING(date, 6, 2)) %>%
  dplyr::select(-date) %>% 
  mutate_all(as.numeric) %>% # Convert data to numeric, because sparklyr gives characters
  mutate(lon_bin = lon_bin / 10,
         lat_bin = lat_bin / 10) %>% # Convert to decimal degree (see GFW documentation)
  filter(!between(lon_bin, -130, 120),
         between(lat_bin, -7, 0), # Crop down to a smaller area to speed up
         !mmsi %in% mmsis) %>% # Remove vessels that were in PIPA (see scripts/vessels_inside_pipa.R)
  collect() %>% # Collect the results into memory
  st_as_sf(coords = c(2, 1), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% # Convert to spatial object
  st_intersection(eez_pna) %>% # <----------------  This key line keeps the points within PNA
  st_set_geometry(value = NULL) %$% # Get rid of the geometry column
  unique(mmsi) #Get the unique list of mmsis

# Now get full tracks
tracks <- gfw_data %>% 
  dplyr::select(date, lat_bin, lon_bin, fishing_hours, mmsi) %>% 
  mutate(year = SUBSTRING(date, 1, 4),
         month = SUBSTRING(date, 6, 2)) %>%
  dplyr::select(-date) %>% 
  mutate_all(as.numeric) %>% 
  filter(mmsi %in% pna_mmsis) %>%
  group_by(mmsi, year, month) %>%
  summarize(hours = sum(fishing_hours, na.rm = T)) %>% 
  collect() %>% 
  mutate(kir = between(mmsi, 529000000, 530000000),
         post = year >= 2015,
         date = lubridate::date(paste(year, month, "01", sep = "/")))

write.csv(x = tracks, file = here::here("data", "pna_vessels_outside_pipa.csv"), row.names = F)

## Close connection
spark_disconnect_all()






