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
library(tidyverse)

# Source local functions
source(here("scripts", "functions", "st_rotate.R"))
source(here("scripts", "functions", "sfc_as_cols.R"))

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
  st_simplify(0.05)

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


# Load raster
yearly_effort_raster <-readRDS(file = here("data", "rasterized_effort_by_region.rds")) %>% 
  mutate(post = ifelse(year < 2015, "Pre", "Post")) %>% 
  group_by(x, y, post, treated) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  filter(!dif == 0)

ggplot() +
  geom_raster(data = yearly_effort_raster,
              mapping = aes(x = x, y = y, fill = dif)) +
  facet_wrap(~treated, ncol = 1) +
  # geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradientn(#trans = "log10",
                       colors = viridis::viridis_pal()(100)) +
  # scale_fill_viridis_c(trans = "log10") +
  startR::ggtheme_map() +
  theme(legend.position = "bottom")


yearly_effort_raster <- readRDS(file = here("data", "rasterized_effort_by_region.rds")) %>% 
  mutate(post = ifelse(year < 2015, "Pre", "Post")) %>% 
  # filter(year %in% c(2013, 2017)) %>% 
  group_by(x, y, post, treated) %>%
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(treated, days, fill = 0) %>% 
  mutate(dif = Treated - Control)

ggplot() +
  geom_raster(data = yearly_effort_raster,
              mapping = aes(x = x, y = y, fill = dif)) +
  facet_wrap(~post, ncol = 1) +
  # geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradientn(#trans = "log10",
    colors = viridis::viridis_pal()(10)) +
  # scale_fill_viridis_c(trans = "log10") +
  startR::ggtheme_map()

# change for displaced
yearly_effort_raster <-readRDS(file = here("data", "rasterized_effort_by_region.rds")) %>% 
  mutate(post = ifelse(year < 2015, "Pre", "Post")) %>% 
  filter(between(x, 129, 213),
         between(y, -26.5, 23))

change_displaced <- yearly_effort_raster %>% 
  filter(treated == "Treated") %>%
  group_by(x, y, post) %>%
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  ggplot() +
  geom_raster(mapping = aes(x = x, y = y, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  # scale_fill_viridis_c() +
  # scale_fill_gradientn(colours = pal) +
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

change_not_displaced <- yearly_effort_raster %>% 
  filter(treated == "Control") %>%
  group_by(x, y, post) %>%
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  ggplot() +
  geom_raster(mapping = aes(x = x, y = y, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  # scale_fill_viridis_c() +
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

difference_between_groups <- yearly_effort_raster %>% 
  group_by(x, y, post, treated) %>%
  summarize(days = mean(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  spread(post, days, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  spread(treated, dif, fill = 0) %>% 
  mutate(dif = Treated - Control) %>% 
  ggplot() +
  geom_raster(mapping = aes(x = x, y = y, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  # scale_fill_viridis_c() +
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








