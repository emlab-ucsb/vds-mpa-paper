# This script checks for a fishing the line effect

# Load packages
library(startR)
library(here)
library(sf)
library(fishwatchr)
library(tidyverse)

# Load other functions
source(here("scripts", "0_functions", "st_rotate.R"))

# Load spatial layers
## PNA EEZ, and filter for Kiribati
eez <- st_read(here("data", "spatial", "PNA_EEZ.gpkg")) %>% 
  filter(ISO_Ter1 == "KIR") # Keep Phoenix islands only

# PIPA
pipa <- st_read(here("data", "spatial", "PIPA.gpkg"))

# Bounding box for the region
bbox <- st_bbox(eez)

# Load the data, and "rotate" it
yearly_effort_raster <-
  readRDS(file = here("raw_data", "rasterized_effort_by_group.rds")) %>% 
  mutate(lon_bin_center = ifelse(lon_bin_center < 0,
                                 lon_bin_center + 180,
                                 lon_bin_center - 180) + 180) %>% 
  filter(between(lon_bin_center, bbox[1], bbox[3]),
         between(lat_bin_center, bbox[2], bbox[4])) %>% 
  group_by(year, lat_bin_center, lon_bin_center) %>% 
  summarize(hours = sum(hours, na.rm = T))

# plot it
plot <- ggplot(yearly_effort_raster) +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = pipa, color = "red", fill = "transparent") +
  facet_wrap(~ year, ncol = 2) +
  scale_fill_gradientn(colours = gfw_palette(25, name = "map_effort", type = "continuous")) +
  ggtheme_map()  +
  guides(fill = guide_colorbar(title = "Fishing hours",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "")

# Export the figure
ggsave(plot = plot,
       filename = here("docs", "img", "fishing_the_line_by_year.pdf"),
       width = 6,
       height = 6)

ggsave(plot = plot,
       filename = here("docs", "img", "fishing_the_line_by_year_ED_Fig2.tiff"),
       width = 6,
       height = 6)


