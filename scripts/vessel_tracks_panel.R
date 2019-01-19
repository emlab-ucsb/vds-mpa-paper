###########################
#   vessel_tracks_panels   #
###########################

##################################################################################
# This script takes the vessel_tracks_baci dataset which has already been cleaned
# and formated. However, I still need to have a set of variables that I will use
# response variables in the regression. The variables that I need are:
# - daily fishing hours
# - daily non-fishing hours
# - daily distance traveled
# - daily mean distance from shore
# - daily mean distance from port
# - daily proportion fishing hours vs. non fishing hours
# - monthly proportion fishing hours in Kiribati EEZ
# - monthly proportion of fishing hours inside PNA EEZs
# Since not all measures will fit in the same panel, I will create many of them
##################################################################################


# Load packages
library(tidyverse)

# Load the data
vessel_tracks_baci<- readRDS(file = here::here("data", "vessel_tracks_baci.rds"))

# Here I group all my variables except for hours
fishing_hours_by_vessel <- vessel_tracks_baci %>%
  group_by(year,
           quarter,
           month,
           year_month,
           day,
           date,
           gear,
           flag,
           mmsi,
           treated,
           post,
           baci_strict,
           baci_relaxed,
           month_c,
           year_c,
           experiment1,
           experiment1.1,
           experiment2,
           experiment3) %>%
  summarize(hours = sum(hours, na.rm = T)) %>%
  ungroup()

# Save data
saveRDS(effort_by_vessel, file = here::here("data", "effort_by_vessel.rds"))
