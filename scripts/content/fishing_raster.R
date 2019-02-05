#######################
#   fishing_raster    #
#######################

################################################################################
# Yearly spatial distribution of fishing effort by treated and control vessels.
# Colors have been adjusted relative to the maximum observed by group and year.
# Red polygons show LGMPAs in the region.
################################################################################

#### SET UP ##################################################

# Load packages
library(sf)
library(tidyverse)

# Source local functions
source(here::here("scripts", "functions", "st_rotate.R"))
source(here::here("scripts", "functions", "sfc_as_cols.R"))

##### EEZs #################################################
# Countries I want to show
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
               "NCL")
# Load EEZs
eez <- read_sf(dsn = here::here("data", "spatial", "EEZ_subset"),
               layer = "EEZ_subset") %>% 
  filter(ISO_Ter1 %in% countries) %>% 
  mutate(KIR = ISO_Ter1 == "KIR",
         PNA = ifelse(PNA, "PNA", "Non-PNA")) %>% 
  group_by(ISO_Ter1, PNA, KIR) %>% 
  summarize()

##### MPAS #################################################
# Load MPA shapefiles
mpas <- read_sf(dsn = here::here("data", "spatial", "LSMPAs"), layer = "LSMPAs") %>% 
  janitor::clean_names() %>% 
  filter(!wdpaid %in% c(555512002, 555512001)) %>% 
  st_transform(crs = 4326) %>% 
  st_rotate() %>%
  mutate(strict = ifelse(no_tk_area > 0 |
                           iucn_cat %in% c("Ia", "Ib") |
                           desig_eng == "Protected Area",
                         "No-Take", "Others"),
         strict = ifelse(is.na(strict), "Others", strict),
         strict = ifelse(wdpaid == 309888, "PIPA", strict),
         Legend = fct_relevel(strict, c("No-Take", "PIPA", "Others"))) %>% 
  filter(!name == "Longline",
         iso3 %in% countries)


# Load raster
yearly_effort_raster <-readRDS(file = here::here("data", "rasterized_effort_by_region.rds"))


# Plot it
plot <- yearly_effort_raster %>% 
  # filter(x > 150) %>% 
  ggplot() +
  geom_raster(mapping = aes(x = x, y = y, fill = hours_norm * 100)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradientn(trans = "log10",
                       colors = viridis::viridis_pal()(100),
                       breaks = c(0, 1, 10, 100),
                       labels = c(0, 1, 10, 100)) +
  # scale_fill_viridis_c(trans = "log10") +
  facet_grid(year~treated, switch = "y") +
  startR::ggtheme_map() +
  theme(legend.position = "bottom") #+
  # guides(fill = guide_legend(title = "% hours"))

ggsave(plot, filename = here::here("docs", "img", "fishing_raster.png"), width = 7.5, height = 9)

