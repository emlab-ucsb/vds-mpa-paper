##################################
#   activity_by_vessel_year_eez  #
##################################

########################################################################
# This script downloads activity_by_vessel_year_eez dataset from
# Google BigQuery. I queried the database in GBQ to get the activity of
# all vessels that ever fished inside PNA using the latest tables.
########################################################################

# Load packages
library(startR)
library(here)

# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
vessel_activity <- get_table(project = "ucsb-gfw",
                             dataset = "mpa_displacement",
                             table = "a_activity_by_vessel_year_eez")
  
# Save it to disk
saveRDS(vessel_activity,
        file = here("raw_data", "activity_by_vessel_year_eez.rds"))
