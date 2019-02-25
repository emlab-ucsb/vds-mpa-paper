##################################
#   activity_by_vessel_year_eez  #
##################################

########################################################################
# This script downloads activity_by_vessel_year_eez dataset from
# Google BigQuery. I queried the database in GBQ to get the activity of
# all vessels that ever fished inside PNA using the latest tables.
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
vessel_activity <- dplyr::tbl(BQc, "activity_by_vessel_year_eez") %>%
  collect() # collect brings the information out of the database and into memory

# Save it to disk
saveRDS(vessel_activity,
        file = here::here("raw_data", "activity_by_vessel_year_eez.rds"))
