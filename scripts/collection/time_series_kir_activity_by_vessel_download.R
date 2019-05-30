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
data <- dplyr::tbl(BQc, "time_series_kir_activity_by_vessel") %>%
  collect() # collect brings the information out of the database and into memory


# Save it to disk
saveRDS(data,
        file = here("raw_data", "time_series_kir_activity_by_vessel.rds"))
