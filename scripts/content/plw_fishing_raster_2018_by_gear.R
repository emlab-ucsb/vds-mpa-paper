########################################
##  plw_fishing_raster_2018_by_gear   ##
########################################

## SET UP
# Load libraries
library(startR)
library(here)
library(cowplot)
library(raster)
library(sf)
library(tidyverse)

# Load custom functions
source(here("scripts", "functions", "st_rotate.R"))

# Load Palauan EEZ
plw_eez <- read_sf(here::here("data", "spatial", "PNA_EEZ"), "PNA_EEZ") %>% 
  st_rotate() %>%
  select(ISO_Ter1) %>% 
  filter(ISO_Ter1 %in% c("PLW")) %>% 
  st_simplify(100)

# Load PNMS boundaries
pnms <- read_sf(dsn = here::here("data", "spatial", "LSMPAs"), layer = "LSMPAs") %>%
  filter(WDPAID == "555622118") %>%
  select(WDPAID)

# Get effort data from BigQuery
plw <- get_table(dataset = "mpa_displacement",
                 table = "plw_raster") %>% 
  group_by(year, best_vessel_class, lon, lat) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup()

# Save effort data for reproducibility
# write.csv(x = plw,
#           file = here("raw_data", "plw_fishing_raster_2018_by_year.csv"),
#           row.names = F)

# The above call to get_table won't work because there
# is no authetication, read in this file instead
# plw <- read.csv(here("raw_data", "plw_fishing_raster_2018_by_year.csv"),
#                 stringsAsFactors = F)

# Plot longline fishing effort for 2018
(plw_2018_ll <- plw %>% 
    filter(year == 2018,
           best_vessel_class == "drifting_longlines") %>% 
    ggplot() +
    geom_raster(aes(x = lon, y = lat, fill = days)) +
    geom_sf(data = plw_eez, fill = "transparent", color = "black", size = 2) +
    geom_sf(data = pnms, fill = "transparent", color = "red", size = 1) +
    scale_fill_viridis_c(option = "C",
                         trans = "log10",
                         limits = c(0.01, 1000),
                         breaks = c(0.01, 0.1, 1, 10, 100, 1000),
                         labels = c(0.01, 0.1, 1, 10, 100, 1000),
                         na.value = 0.1) +
    scale_x_continuous(breaks = seq(130, 136, by = 2)) +
    theme_minimal() +
    guides(fill = guide_colorbar(title = "Vessel-days",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(x = "", y = ""))

# Plot purse seine fishing effort for 2018
(plw_2018_ps <- plw %>% 
    filter(best_vessel_class == "tuna_purse_seines") %>% 
    ggplot() +
    geom_raster(aes(x = lon, y = lat, fill = days)) +
    geom_sf(data = plw_eez, fill = "transparent", color = "black", size = 2) +
    geom_sf(data = pnms, fill = "transparent", color = "red", size = 1) +
    scale_fill_viridis_c(option = "C",
                         trans = "log10",
                         limits = c(0.01, 1),
                         breaks = c(0.01, 0.03, 0.1, 0.3, 1),
                         labels = c(0.01, 0.03, 0.1, 0.3, 1),
                         na.value = 0.1) +
    scale_x_continuous(breaks = seq(130, 136, by = 2)) +
    theme_minimal() +
    guides(fill = guide_colorbar(title = "Vessel-days",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(x = "", y = ""))

# Put plots together
plw_2018 <- plot_grid(plw_2018_ll,
                      plw_2018_ps,
                      ncol = 2,
                      labels = c("Longlines", "Purse seines"),
                      label_fontface = "plain",
                      label_size = 12)
# Export plot as png
ggsave(plw_2018,
       filename = here("docs", "img", "plw_2018.png"),
       width = 7,
       height = 3.5)
# Export plot as pdf
ggsave(plw_2018,
       filename = here("docs", "img", "plw_2018.pdf"),
       width = 7,
       height = 3.5)


#### Effort within PNMS?
# How much longline effort is displaced?
# Create a raster object of longline vessel-days
ll_rast <- plw %>% 
  filter(year == 2018,
         best_vessel_class == "drifting_longlines") %>% 
  select(lon, lat, days) %>% 
  rasterFromXYZ(xyz = ., crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Mask it with the monument boundaries and calculate ratio to total
ll_rast %>%
  mask(x = ., mask = pnms_sp) %>% 
  cellStats(sum) / cellStats(ll_rast, sum) * 100

#56.20% of longline vessel days occurs within PNMS

# How much purse seine effort is displaced?
# Create a raster object of purse seine vessel-days
ps_rast <- plw %>% 
  filter(year == 2018,
         !best_vessel_class == "drifting_longlines") %>% 
  select(lon, lat, days) %>% 
  rasterFromXYZ(xyz = ., crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Mask it by the monumnet boundaries an calculate ratio to total
ps_rast %>%
  mask(x = ., mask = pnms_sp) %>% 
  cellStats(sum) / cellStats(ps_rast, sum) * 100

#91.70 % of purse seine VDS occurs inside PNMS boundaries












