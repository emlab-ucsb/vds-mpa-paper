############################
#   fishing_raster_diff    #
############################

################################################################################
# Yearly spatial distribution of fishing effort by treated and control vessels.
# Colors have been adjusted relative to the maximum observed by group and year.
# Red polygons show LGMPAs in the region.
################################################################################

#### SET UP ##################################################

# Load packages
library(here)
library(sf)
library(cowplot)
library(tidyverse)

# Source local functions
source(here("scripts", "0_functions", "st_rotate.R"))
source(here("scripts", "0_functions", "sfc_as_cols.R"))

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
               "NCL",
               "PLW")
# Load EEZs
eez <- read_sf(dsn = here("data", "spatial", "EEZ_subset"),
               layer = "EEZ_subset") %>% 
  filter(ISO_Ter1 %in% countries) %>% 
  mutate(KIR = ISO_Ter1 == "KIR",
         PNA = ifelse(PNA, "PNA", "Non-PNA")) %>% 
  group_by(ISO_Ter1, PNA, KIR) %>% 
  summarize() %>% 
  rmapshaper::ms_simplify()

##### MPAS #################################################
# Load MPA shapefiles
mpas <- read_sf(dsn = here("data", "spatial", "LSMPAs"), layer = "LSMPAs") %>% 
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


# Load effort raster
yearly_effort_raster <-readRDS(file = here("raw_data", "rasterized_effort_by_group.rds")) %>% 
  mutate(post = ifelse(year < 2015, "Pre", "Post"),
         lon_bin_center = ifelse(lon_bin_center < 0,
                                 lon_bin_center + 180,
                                 lon_bin_center - 180) + 180) %>% 
  filter(between(lon_bin_center, 129, 213),
         between(lat_bin_center, -26.5, 23))

# change for displaced
change_displaced <- yearly_effort_raster %>% 
  filter(group == "displaced") %>%
  group_by(lon_bin_center, lat_bin_center, post) %>% 
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Change\nin hours",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "")

ggsave(plot = change_displaced,
       file = here("docs", "img", "fishing_raster_displaced.png"),
       height = 2.5,
       width = 4.5)
# Change for not displaced
change_not_displaced <- yearly_effort_raster %>% 
  filter(!group == "displaced") %>%
  group_by(lon_bin_center, lat_bin_center, post) %>% 
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Change\nin hours",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "")

ggsave(plot = change_not_displaced,
       file = here("docs", "img", "fishing_raster_not_displaced.png"),
       height = 2.5,
       width = 4.5)

# Difference between groups
difference_between_groups <- yearly_effort_raster %>% 
  group_by(lon_bin_center, lat_bin_center, post, group) %>%
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  spread(group, dif, fill = 0) %>% 
  mutate(dif = displaced - non_displaced) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Change\nin hours",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "")

ggsave(plot = difference_between_groups,
       file = here("docs", "img", "fishing_raster_difference_between_groups.png"),
       height = 2.5,
       width = 4.5)


plot_change <- plot_grid(change_displaced,
                         change_not_displaced,
                         difference_between_groups,
                         ncol = 1,
                         labels = "AUTO")

ggsave(plot = plot_change,
       file = here("docs", "img", "fishing_raster_diff.png"),
       height = 7,
       width = 5)








