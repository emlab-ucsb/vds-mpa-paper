#####################################
#   a_vessel_info_pna_purse_seines  #
#####################################

########################################################################
# This script downloads a_vessel_info_pna_purse_seines dataset from
# Google BigQuery. I queried the database in GBQ to get the info of
# all vessels that ever fished inside PNA using the latest tables.
########################################################################

# Load packages
library(startR)
library(here)

# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
vessel_info_pna_purse_seines <- get_table(project = "ucsb-gfw",
                                          dataset = "mpa_displacement",
                                          table = "a_vessel_info_pna_purse_seines")

# Save it to disk
saveRDS(vessel_info_pna_purse_seines,
        file = here("raw_data", "vessel_info_pna_purse_seines.rds"))

