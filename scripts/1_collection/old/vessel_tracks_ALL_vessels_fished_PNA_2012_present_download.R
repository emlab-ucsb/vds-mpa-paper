###################################################################
#   vessel_tracks_ALL_vessels_fished_PNA_2012_present_download    #
###################################################################


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
vessel_tracks <- dplyr::tbl(BQc, "vessel_tracks_ALL_vessels_fished_PNA_2012_present") %>%
  collect() # collect brings the information out of the database and into memory

# SAVE RESULTS
saveRDS(vessel_tracks,
        file = here::here("raw_data", "vessel_tracks_ALL_vessels_fished_PNA_2012_present.rds"))
