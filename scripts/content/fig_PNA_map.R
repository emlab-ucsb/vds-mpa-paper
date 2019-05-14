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
library(countrycode)
library(startR)
library(sf)
library(here)
library(tidyverse)

# Source local functions
source(here("scripts", "functions", "st_rotate.R"))
source(here("scripts", "functions", "sfc_as_cols.R"))

# List of countries
countries <- c("PIPA",
               "KIR",
               "HS",
               "ASM",
               "COK",
               "FSM",
               "MHL",
               "NRU",
               "PNG",
               "SLB",
               "TKL",
               "TUV",
               "UMI",
               "FJI",
               "NIU",
               "TON",
               "WSM",
               "WLF",
               "VUT",
               "NCL",
               "PLW")

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

# Get coastline
small_coast <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  filter(sov_a3 %in% countries) %>% 
  st_rotate()

eez <- read_sf(dsn = here("data", "spatial", "EEZ_subset"),
               layer = "EEZ_subset") %>% 
  filter(ISO_Ter1 %in% countries) %>% 
  mutate(KIR = ISO_Ter1 == "KIR",
         VDS = ifelse(ISO_Ter1 %in% VDS_countries, "VDS", "Non-PNA")) %>% 
  group_by(ISO_Ter1, VDS, KIR) %>% 
  summarize()

labels <- eez %>% 
  filter(ISO_Ter1 %in% VDS_countries) %>% 
  # lwgeom::st_make_valid() %>% 
    mutate(label = countrycode(sourcevar = ISO_Ter1,
                               origin = "iso3c",
                               destination = "country.name"),
           label = ifelse(ISO_Ter1 == "FSM", "Federal States\nof Micronesia", label)) %>% 
  st_simplify(dTolerance = 0.1)


pipa <- sf::read_sf(dsn = here("data", "spatial", "PIPA"), layer = "PIPA") %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_rotate()

pnms <- read_sf(dsn = here::here("data", "spatial", "LSMPAs"), layer = "LSMPAs") %>%
  filter(WDPAID == "555622118") %>%
  select(WDPAID)

plot <- ggplot() +
  geom_sf(data = eez, aes(color = KIR, fill = VDS), alpha = 0.5, size = 0.5) +
  geom_sf(data = pipa, fill = "red") +
  geom_sf(data = pnms, fill = "purple", size = 0) +
  geom_sf(data = small_coast, color = "black", fill = "#E3E3E3", size = 0.1) +
  geom_sf_label_repel(data = labels, mapping = aes(label = ISO_Ter1), force = 40, seed = 2) +
  ggtheme_plot() +
  scale_color_manual(values = c("black", "red")) +
  scale_fill_manual(values = c("transparent", "steelblue")) +
  theme(legend.position = "none")

ggsave(plot = plot,
       filename = here("docs", "img", "PNA_map.png"),
       width = 6,
       height = 3.5)

ggsave(plot = plot,
       filename = here("docs", "img", "PNA_map.pdf"),
       width = 6,
       height = 3.5)




















