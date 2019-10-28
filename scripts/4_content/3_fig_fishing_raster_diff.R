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


##### EEZs #################################################
# Load EEZs
eez <- read_sf(dsn = here("data", "spatial", "EEZ_subset.gpkg")) %>% 
  filter(!ISO_Ter1 == "IDN") #remove Indonesia from this map

# Extract ISO3 codes for countries present
countries <- eez$ISO_Ter1

##### MPAS #################################################
# Load MPA shapefiles
mpas <- st_read(dsn = here("data", "spatial", "PIPA.gpkg"))


# Load effort raster
yearly_effort_raster <-readRDS(file = here("raw_data", "rasterized_effort_by_group.rds")) %>% 
  mutate(post = ifelse(year < 2015, "Pre", "Post"),
         lon_bin_center = ifelse(lon_bin_center < 0,
                                 lon_bin_center + 180,
                                 lon_bin_center - 180) + 180) %>% 
  filter(between(lon_bin_center, 129, 213),
         between(lat_bin_center, -26.5, 23)) %>% 
  mutate(hours = hours_length)

# change for displaced
change_displaced <- yearly_effort_raster %>% 
  filter(group == "displaced") %>%
  group_by(lon_bin_center, lat_bin_center, post) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  spread(post, hours, fill = 0) %>% 
  mutate(dif = Post - Pre,
         dif = dif / max(abs(dif))) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white", limits = c(-1, 1)) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Normalized\nchange",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "") +
  ggtitle("Displaced vessels")

ggsave(plot = change_displaced,
       file = here("docs", "img", "fishing_raster_displaced.pdf"),
       height = 2.5,
       width = 4.5)

# Change for not displaced
change_not_displaced <- yearly_effort_raster %>% 
  filter(!group == "displaced") %>%
  group_by(lon_bin_center, lat_bin_center, post) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  spread(post, hours, fill = 0) %>% 
  mutate(dif = Post - Pre,
         dif = dif / max(abs(dif))) %>% 
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white", limits = c(-1, 1)) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Normalized\nchange",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "") + 
  ggtitle("Non-displaced vessels")

ggsave(plot = change_not_displaced,
       file = here("docs", "img", "fishing_raster_not_displaced.pdf"),
       height = 2.5,
       width = 4.5)

# Difference between groups
difference_between_groups <- yearly_effort_raster %>% 
  group_by(lon_bin_center, lat_bin_center, post, group) %>%
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  spread(post, hours, fill = 0) %>% 
  mutate(dif = Post - Pre) %>% 
  select(-c(Post, Pre)) %>% 
  group_by(group) %>% 
  mutate(max_group = max(abs(dif))) %>% 
  ungroup() %>% 
  mutate(dif = (dif / max_group)) %>% 
  select(-max_group) %>% 
  spread(group, dif, fill = 0) %>%
  mutate(dif = displaced - non_displaced) %>%
  ggplot() +
  geom_tile(mapping = aes(x = lon_bin_center, y = lat_bin_center, fill = dif)) +
  geom_sf(data = eez, fill = "transparent", color = "black") +
  geom_sf(data = mpas, fill = "transparent", color = "red") +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, mid = "white", limits = c(-1, 1)) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "transparent")) +
  guides(fill = guide_colorbar(title = "Difference",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "", y = "") +
  ggtitle("Relative redistribution")

ggsave(plot = difference_between_groups,
       file = here("docs", "img", "fishing_raster_difference_between_groups.pdf"),
       height = 2.5,
       width = 4.5)

plot_change <- plot_grid(change_displaced,
                         change_not_displaced,
                         difference_between_groups,
                         ncol = 1,
                         labels = "auto")

ggsave(plot = plot_change,
       file = here("docs", "img", "fishing_raster_diff.png"),
       height = 7,
       width = 4)

ggsave(plot = plot_change,
       file = here("docs", "img", "fishing_raster_diff.pdf"),
       height = 7,
       width = 4)







