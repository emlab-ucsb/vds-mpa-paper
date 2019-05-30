######################
#   spatial_panels   #
######################

##################################################################################
# This script takes the vessel_tracks_baci dataset which has already been cleaned
# and formated. However, I still need to have a set of variables that I will use
# response variables in the regression.
# Since not all measures will fit in the same panel, I will create more than one.
# The variables that I need are:
# PANEL 1:
# - monthly proportion fishing hours in Kiribati EEZ with respect to all VDS hours
# 
# PANEL 2:
# - monthly proportion of fishing hours inside PNA EEZs with respect to all hours
# 
# There are similar panels at the vessel-level, which ask the following question:
# For a given vessel, what proportion of their fishing hours happened in month X
# and EEZ / spatial region Y?
# Instead, here our units are the spatial regions, and we see how fishing within
# them changes through time, without making distinctions on the vessel that fishes
# in those waters.
##################################################################################


# Load packages
library(magrittr)
library(tidyverse)

# Set a large memory size for when this code runs on Windows
if(unname(Sys.info()[1] == "Windows")){
  memory.limit(size = 8e6)
}

# Load the data
vessel_tracks_baci <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(baci_strict,
         fishing)

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

######### PANEL 1 ########################
## PROPORTION OF FISHING HOURS WITHIN KIR

prop_fishing_hours_kir_by_month_panel <- vessel_tracks_baci %>% 
  filter(eez_iso3 %in% VDS_countries) %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           post,
           month_c,
           year_c) %>% 
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           eez_iso3,
           post,
           month_c,
           year_c,
           total_hours) %>% 
  summarize(eez_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop_hours = eez_hours / total_hours) %>% 
  arrange(year, month)

# Save data
saveRDS(prop_fishing_hours_kir_by_month_panel,
        file = here::here("data", "panels", "prop_fishing_hours_kir_by_month_panel.rds"))






######### PANEL 2 ########################
## PROPORTION OF FISHING HOURS WITHIN VDS

prop_fishing_hours_vds_by_month_panel <- vessel_tracks_baci %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           post,
           month_c,
           year_c) %>% 
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(eez_iso3 %in% VDS_countries) %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           post,
           month_c,
           year_c,
           total_hours) %>% 
  summarize(eez_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop_hours = eez_hours / total_hours) %>% 
  arrange(year, month)

# Save data
saveRDS(prop_fishing_hours_vds_by_month_panel,
        file = here::here("data", "panels", "prop_fishing_hours_vds_by_month_panel.rds"))


















