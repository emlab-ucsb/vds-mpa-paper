##################################
#   rasterized_effort_plw_by_gear #
##################################

########################################################################
# This script downloads rasterized_effort_plw_by_gear dataset from
# Google BigQuery. I queried the database in GBQ to get the activity of
# all vessels that ever fished inside PNA using the latest tables.
########################################################################

# Load packages
library(startR)
library(here)

# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
rasterized_effort_plw_by_gear <-
  get_table(project = "ucsb-gfw",
            dataset = "mpa_displacement",
            table = "a_rasterized_effort_plw_by_gear")

# Save it to disk
saveRDS(rasterized_effort_plw_by_gear,
        file = here("raw_data", "rasterized_effort_plw_by_gear.rds"))
