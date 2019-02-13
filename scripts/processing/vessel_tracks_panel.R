###########################
#   vessel_tracks_panels   #
###########################

##################################################################################
# This script takes the vessel_tracks_baci dataset which has already been cleaned
# and formated. However, I still need to have a set of variables that I will use
# response variables in the regression.
# Since not all measures will fit in the same panel, I will create many of them.
# The variables that I need are:
# PANEL 1:
# - daily fishing hours
# - daily non-fishing hours
# 
# PANEL 2:
# - daily proportion fishing hours vs. non fishing hours
# 
# PANEL 3:
# - daily distance traveled
# 
# PANEL 4:
# - daily mean distance from shore (min and max)
# - daily mean distance from port (min and max)
# 
# PANEL 5:
# - daily mean distance from port and shore fishing only (min and max)
# 
# PANEL 6:
# - monthly proportion fishing hours in Kiribati EEZ
# 
# PANEL 7:
# - monthly proportion of fishing hours inside PNA EEZs
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
  filter(baci_strict)

######################## BEGIN PANEL 1 ##########
## FISHING AND NON-FISHING HOURS
# Here I group all my variables except for hours
daily_hours_by_vessel_panel <- vessel_tracks_baci %>%
  group_by(year,
           quarter,
           month,
           year_month,
           day,
           date,
           fishing,
           gear,
           flag,
           mmsi,
           treated,
           post,
           month_c,
           year_c) %>%
  summarize(hours = sum(hours, na.rm = T)) %>%
  ungroup() %>% 
  arrange(date, mmsi)

# Save data
saveRDS(daily_hours_by_vessel_panel,
        file = here::here("data", "panels", "daily_hours_by_vessel_panel.rds"))








######################## BEGIN PANEL 2 ##########
## PROPORTION OF FISHING vs NON-FISHING HOURS
daily_prop_fishing_hours_by_vessel_panel <- daily_hours_by_vessel_panel %>% 
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
           month_c,
           year_c) %>%
  mutate(total_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  filter(fishing) %>% 
  mutate(prop_fishing = hours / total_hours)

saveRDS(daily_prop_fishing_hours_by_vessel_panel,
        file = here::here("data", "panels", "daily_prop_fishing_hours_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(daily_hours_by_vessel_panel)
rm(daily_prop_fishing_hours_by_vessel_panel)








######################## BEGIN PANEL 3 ##########
## DAILY DISTANCE TRAVELED
# First I must define a function that calculates the distance
dist_fxn <- function(data){
  data %>% 
    arrange(timestamp) %>%
    mutate(lon = ifelse(lon > 0, lon, lon + 360), #Convert longitudes to 0-360 format
           lon1 = lag(lon), #Calculate previous lon
           lat1 = lag(lat), #Calculate previous lat
           delta_lon = lon1 - lon, #Calculate change in lon
           delta_lat = lat1 - lat, #Calculate change in lat
           dist_lon = delta_lon * cos(lat * pi /180) * 111.321, #Convert lon change to km change
           dist_lat = 111 * delta_lat, #Convert lat change to km change
           dist = sqrt(dist_lon ^ 2 + dist_lat ^ 2)) %$% #Calculate distance
    sum(dist, na.rm = T) #Sum across all distances
}

# Group at the date-mmsi level
daily_distance_by_vessel_panel <- vessel_tracks_baci %>%
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
           month_c,
           year_c) %>%
  nest() %>%
  mutate(dist = purrr::map_dbl(data, dist_fxn)) %>% 
  select(-data) %>% 
  arrange(date, mmsi)
  
# Save data
saveRDS(daily_distance_by_vessel_panel,
        file = here::here("data", "panels", "daily_distance_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(daily_distance_by_vessel_panel)
 








######################## BEGIN PANEL 4 ##########
## DISTANCE FROM SHORE AND FROM PORT
# Group at the date-mmsi level
distance_from_port_shore_by_vessel_panel <- vessel_tracks_baci %>% 
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
           month_c,
           year_c) %>% 
  summarize(min_dist_port = min(distance_from_port, na.rm = T),
            max_dist_port = max(distance_from_port, na.rm = T),
            mean_dist_port = mean(distance_from_port, na.rm = T),
            min_dist_shore = min(distance_from_shore, na.rm = T),
            max_dist_shore = max(distance_from_shore, na.rm = T),
            mean_dist_shore = mean(distance_from_shore, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(date, mmsi)

saveRDS(distance_from_port_shore_by_vessel_panel,
        file = here::here("data", "panels", "distance_from_port_shore_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(distance_from_port_shore_by_vessel_panel)








######################## BEGIN PANEL 5 ############ DISTANCE FROM SHORE AND FROM PORT FISHING ONLY!!!!!!
# Group at the date-mmsi level
distance_from_port_shore_fishing_by_vessel_panel <- vessel_tracks_baci %>% 
  filter(fishing) %>% 
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
           month_c,
           year_c) %>% 
  summarize(min_dist_port = min(distance_from_port, na.rm = T),
            max_dist_port = max(distance_from_port, na.rm = T),
            mean_dist_port = mean(distance_from_port, na.rm = T),
            min_dist_shore = min(distance_from_shore, na.rm = T),
            max_dist_shore = max(distance_from_shore, na.rm = T),
            mean_dist_shore = mean(distance_from_shore, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(date, mmsi)

# Save the data
saveRDS(distance_from_port_shore_fishing_by_vessel_panel,
        file = here::here("data", "panels", "distance_from_port_shore_fishing_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(distance_from_port_shore_fishing_by_vessel_panel)








######################## BEGIN PANEL 6 ##########
# PROPORTION OF HOURS FISHING IN KIR EEZ
KIR_fishing_hours_by_vessel_panel <- vessel_tracks_baci %>%
  filter(fishing) %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           flag,
           mmsi,
           treated,
           post,
           month_c,
           year_c,
           experiment1,
           experiment2,
           experiment3) %>%
  mutate(total_hours = sum(hours, na.rm = T)) %>% #Calculate total hours per vessel-month
  ungroup() %>% 
  filter(eez_iso3 == "KIR") %>% # Keep only KIR fishing hours
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           flag,
           mmsi,
           treated,
           post,
           month_c,
           year_c,
           experiment1,
           experiment2,
           experiment3,
           total_hours) %>%
  summarize(kir_hours = sum(hours, na.rm = T)) %>% # Calculate total kir fishing hours
  ungroup() %>% 
  mutate(prop_hours = kir_hours / total_hours) %>% # Calculate the proportion
  arrange(year, month, mmsi) %>% 
  filter(!is.nan(prop_hours)) #Remove NaNs generated by 0 / 0

# Save the data
saveRDS(KIR_fishing_hours_by_vessel_panel,
        file = here::here("data", "panels", "KIR_fishing_hours_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(KIR_fishing_hours_by_vessel_panel)








######################## BEGIN PANEL 7 ##########
# PROPORTION OF HOURS FISHING IN VDS EEZ
# This is the same as above, but for VDS (PNA + TKL)

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

VDS_fishing_hours_by_vessel_panel <- vessel_tracks_baci %>%
  filter(fishing) %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           flag,
           mmsi,
           treated,
           post,
           month_c,
           year_c,
           experiment1,
           experiment2,
           experiment3) %>%
  mutate(total_hours = sum(hours, na.rm = T)) %>%
  ungroup() %>% 
  filter(eez_iso3 %in% VDS_countries) %>% 
  group_by(year,
           quarter,
           month,
           year_month,
           gear,
           flag,
           mmsi,
           treated,
           post,
           month_c,
           year_c,
           experiment1,
           experiment2,
           experiment3,
           total_hours) %>%
  summarize(vds_hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(prop_hours = vds_hours / total_hours) %>% 
  arrange(year, month, mmsi) %>% 
  filter(!is.nan(prop_hours)) #Remove NaNs generated by 0 / 0

# Save the data
saveRDS(VDS_fishing_hours_by_vessel_panel,
        file = here::here("data", "panels", "VDS_fishing_hours_by_vessel_panel.rds"))

# Remove last dataset from memory
rm(VDS_fishing_hours_by_vessel_panel)




















