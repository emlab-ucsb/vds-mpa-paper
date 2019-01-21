#############################
#   vessel_tracks_download  #
#############################

########################################################################
# This script downloads the vessel_tracks dataset from Google BigQuery.
# I queried the database in GBQ to get the tracks of all vessels we need.
# That table (vessel_tracks) was made with scripts/vessel_tracks.sql.
# After downloading the data, I add a couple of features that will come
# in handy in the processing and fine-tuning of the next scripts. These
# are basically getting rid of the few duplicate records, adding some
# variables like factors for time-variables, and arranging the columns
# in the order I want them (for now).
########################################################################

# Load the packages that I need
library(dplyr)
library(dbplyr)
library(DBI)
library(bigrquery)
library(magrittr)

# Create a connection to Google BigQuery
# For this to run, you require authentication and be part of ucsb-gfw
BQc <- bigrquery::dbConnect(
  drv = bigrquery::bigquery(),
  project = "ucsb-gfw", # This is the project, to which you must belong
  dataset = "mpa_displacement", # This is the dataset, where I keep all the tables
  allowLargeResults = TRUE #We're downloading about 10 GB of data
)

# List all tables to test the connection
DBI::dbListTables(BQc)


# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
vessel_tracks <- dplyr::tbl(BQc, "vessel_tracks") %>%
  collect() # collect brings the information out of the database and into memory

# ADD FEATURES
# First I define a set of variables of interest,
# like the countries that belong to the PNA.

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

# Set a large memory size for when this code runs on Windows
if(unname(Sys.info()[1] == "Windows")){
  memory.limit(size = 8e6)
}

# Modify the data
vessel_tracks %<>%
  distinct() %>%
  mutate(
    date = lubridate::date(timestamp),
    post = year >= 2015,
    nnet_score = ifelse(is.na(nnet_score), 0, nnet_score), # If there are NAs in nnet_score, convert to 0 (no fishing)
    fishing = nnet_score >= 0.5, # Values of nnet_score > 0.5 imply fishing
    month_c = as.character(month),
    year_c = as.character(year),
    year_month = lubridate::date(paste(year, month, 1, sep = "/")),
    quarter = lubridate::quarter(
      date,
      with_year = T),
    PNA = flag %in% PNA_countries,
    VDS = flag %in% PNA_countries
  ) %>%
  select(
    year,
    month,
    day,
    date,
    timestamp,
    post,
    treated,
    mmsi,
    gear = inferred_label,
    flag,
    lon,
    lat,
    hours,
    fishing,
    nnet_score,
    eez_iso3,
    distance_from_port,
    distance_from_shore,
    seg_id,
    month_c,
    year_c,
    year_month,
    quarter,
    PNA,
    VDS
  )

# Save it to disk
saveRDS(vessel_tracks, file = here::here("raw_data", "vessel_tracks.rds"))
