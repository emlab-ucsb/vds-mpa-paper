###############
#   PNA_map   #
###############

#####################################################
# Creates a map of countries that belong to the PNA
# and that jointly manage the fishery through a VDS
# that includes all PNA countries as well as TKL
#####################################################

# Load packages
library(ggrepel)
library(ggsflabel) #devtools::install_github("yutannihilation/ggsflabel")
library(cowplot)
library(countrycode)
library(startR)
library(sf)
library(here)
library(tidyverse)

# Source local functions
source(here("scripts", "0_functions", "st_rotate.R"))

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

eez <- read_sf(dsn = here("data", "spatial", "EEZ_subset.gpkg")) %>% 
  mutate(KIR = ISO_Ter1 == "KIR",
         VDS = ISO_Ter1 %in% VDS_countries) %>% 
  lwgeom::st_make_valid()

# Extract ISO3 codes for countries present
countries <- eez$ISO_Ter1

# Get coastline
small_coast <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(sov_a3 %in% countries) %>% 
  st_rotate()

# Create labeling datast (just to add names to the map)
labels <- eez %>% 
  filter(ISO_Ter1 %in% VDS_countries) %>% 
    mutate(label = countrycode(sourcevar = ISO_Ter1,
                               origin = "iso3c",
                               destination = "country.name"),
           label = ifelse(ISO_Ter1 == "FSM", "Federal States\nof Micronesia", label)) %>% 
  lwgeom::st_make_valid()

# Load PIPA
pipa <- st_read(dsn = here("data", "spatial", "PIPA.gpkg"))

# Load PNMS
pnms <- st_read(here("raw_data", "spatial", "PLW_shapefiles"),
                "PNMS")

world <- rnaturalearth::ne_countries(continent = c("Oceania", "Asia"),
                                     returnclass = "sf") %>% 
  st_rotate()

bbox <- eez %>% 
  filter(ISO_Ter1 != "IDN") %>% 
  st_bbox() %>% 
  st_as_sfc()

reference <- ggplot() +
  geom_sf(data = world, color = "gray", fill = "gray") +
  geom_sf(data = bbox, fill = "transparent", size = 1) +
  ggtheme_map() +
  theme(plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

main <- ggplot() +
  geom_sf(data = eez, aes(color = KIR, fill = VDS), size = 0.5) +
  geom_sf(data = pipa, fill = "red") +
  geom_sf(data = pnms, fill = "red", size = 0) +
  geom_sf(data = small_coast, color = "black", fill = "#E3E3E3", size = 0.1) +
  geom_sf_label_repel(data = labels, mapping = aes(label = ISO_Ter1), force = 40, seed = 2) +
  ggtheme_plot() +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("transparent", "steelblue")) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(120, NA)) +
  scale_y_continuous(limits = c(-47.5, NA))

plot <- ggdraw() +
  draw_plot(main) +
  draw_plot(reference, x = 0.075, y = 0.01, width = 0.45, height = 0.45)

ggsave(plot = plot,
       filename = here("docs", "img", "PNA_map.png"),
       width = 6,
       height = 4.6)

ggsave(plot = plot,
       filename = here("docs", "img", "PNA_map.pdf"),
       width = 6,
       height = 4.6)




















