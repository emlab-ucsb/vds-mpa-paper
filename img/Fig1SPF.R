# Load packages

library(sf)
library(startR)
library(tidyverse)
library(ggrepel)

# Load custom function to "rotate" the view

source(here::here("scripts", "st_rotate.R"))
source(here::here("scripts", "sfc_as_cols.R"))

###### Map

# Coastline
coastline <- rnaturalearth::ne_countries(scale = "medium",
                                         returnclass = "sf",
                                         continent = c("Asia",
                                                       "North America",
                                                       "Oceania",
                                                       "Europe",
                                                       "South America")) %>% 
  st_rotate() %>% 
  mutate(feature = "Coastline") %>% 
  select(feature)

# EEZs
eez <- read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "eez_v10") %>% 
  filter(Sovereign1 == "United States") %>% 
  st_rotate() %>% 
  group_by(Sovereign1) %>% 
  summarize() %>% 
  ungroup() %>% 
  mutate(feature = "US EEZ") %>% 
  select(feature)

#MPAs
MPAs <- read_sf(here::here("raw_data", "spatial", "WDPA_Mar2018"),
        layer = "WDPA_Mar2018_marine-shapefile-polygons",
        quiet = T,
        stringsAsFactors = F) %>% 
  janitor::clean_names() %>%
  filter(wdpaid %in% c(220201, 400011)) %>% #https://www.protectedplanet.net/
  select(wdpaid, name) %>% 
  st_rotate() %>% 
  group_by(wdpaid) %>% 
  summarize() %>% 
  ungroup() %>% 
  mutate(feature = c("PMNM", "PRI")) %>% 
  select(feature)

# # Dataframe with labels for MPAs (1, and 2)
# MPAs_label <- MPAs %>% 
#   mutate(n = c(1, 2)) %>%
#   st_cast("POLYGON") %>%
#   st_centroid() %>% 
#   sfc_as_cols() %>% 
#   mutate(nrow = rownames(.)) %>% 
#   filter(!nrow == 2)

# Combine EEZs and MPAs
shapes <- rbind(eez, MPAs) %>% 
  select(Legend = feature) %>% 
  mutate(Legend = fct_relevel(Legend, "US EEZ"))

# Points for ports, from original Fig 1
points <- data.frame(Port = c("Honolulu", "Pago Pago"),
                     x = c(-157.879145, -170.688597),
                     y = c(21.316288, -14.271795)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_rotate()

# Dataframe with labels for points
points_label <- points %>% 
  sfc_as_cols()

boxes <- data.frame(Fleet = "Hawaii Tuna",
                    lon = c(-123.975, -179.0983, -179.0983, -123.975, -123.975),
                    lat = c(38.4717, 38.4717, -13.5367, -13.5367, 38.4717)) %>% 
  rbind(data.frame(Fleet = "Hawaii Swordfish",
                   lon = c(-125.2267, -179.8833, -179.8833, -125.2267, -125.2267),
                   lat = c(46.5983, 46.5983, 17.2867, 17.2867, 46.5983))) %>% 
  rbind(data.frame(Fleet = "American Samoa Tuna",
                   lon = c(-135.65, -175.9583, -175.9583, -135.65, -135.65),
                   lat = c(24.0617, 24.0617, -22.9317, -22.9317, 24.0617))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(Fleet) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_rotate()

# Top panel
p1 <- ggplot() +
  geom_sf(data = shapes, aes(fill = Legend), color = "transparent") +
  geom_sf(data = coastline, color = "black", fill = "white") +
  geom_sf(data = points, aes(color = Port), size = 2) +
  geom_text_repel(data = points_label,
                  mapping = aes(x = lon, y = lat, label = Port),
                  nudge_x = 5,
                  nudge_y = 5) +
  ggtheme_map() +
  geom_sf(data = boxes, fill = "transparent", aes(linetype = Fleet), size = 1) +
  scale_fill_manual(values = c("lightyellow", "steelblue", "steelblue1")) +
  scale_color_manual(values = c("red", "purple")) +
  theme(panel.background = element_rect(fill = "#E3E3E3"),
        panel.border = element_blank(),
        legend.background = element_blank(),
        legend.justification = c(0, 0.05),
        legend.position = c(0.55, 0),
        legend.box = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11)) +
  scale_x_continuous(limits = c(112, 270), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-45, 60), expand = c(0, 0)) +
  guides(color = F,
         fill = guide_legend(ncol = 1, order = 1),
         linetype = guide_legend(title = "", ncol = 1, order = 2))

####### Set locations

# Load data
load(file = here::here("data", "USMonuments", "inout.Rdata"))

inout.data %<>%
  filter(HAWAII == 1,
         sword == 0,
         SET_YEAR > 2009) %>% 
  mutate(Outside = 1L - inside_PRI - inside_PMNM,
         year = SET_YEAR) %>% 
  select(year = SET_YEAR,
         PRI = inside_PRI,
         PMNM = inside_PMNM,
         Outside) %>% 
  gather(Location, Percent, -c(year)) %>% 
  mutate(Location = fct_relevel(Location, c("PRI", "PMNM", "Outside")))

# Bar chart
p2_1 <- inout.data %>% 
  ggplot(aes(x = Location, y = Percent, fill = Location)) +
  geom_col(color = "black", size = 0.5) +
  facet_wrap(~year, ncol = 4) +
  scale_fill_manual(values = c("steelblue1", "steelblue", "#E3E3E3")) +
  ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Year") +
  theme(legend.position = "none")


# Stacked bar chart
p2_2 <- inout.data %>%
  mutate(Location = fct_relevel(Location, c("PRI", "PMNM", "Outside"))) %>% 
  ggplot(aes(x = year, y = Percent, fill = Location)) +
  geom_col(color = "black", size = 0.5) +
  scale_fill_manual(values = c("steelblue1", "steelblue", "#E3E3E3")) +
  ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(expand = c(0,0), labels = c(2010:2017), breaks = c(2010:2017)) +
  xlab("Year") +
  theme(legend.position = "none")

#Stacked area chart without Outside
p2_3 <- inout.data %>% 
  filter(!Location == "Outside") %>% 
  ggplot(aes(x = year, y = Percent, fill = Location)) +
  geom_col(color = "black", size = 0.5) +
  scale_fill_manual(values = c("steelblue1", "steelblue")) +
  ggtheme_plot() +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "#E3E3E3")) +
  xlab("Year") +
  theme(legend.position = "none")

# Combine map and charts with cowplot, then export
fig1a <- cowplot::plot_grid(p1, p2_1, ncol = 1, rel_heights = c(2, 1))
ggsave(fig1a, filename = here::here("img/Fig1a.pdf"), width = 7.22, height = 8.05, dpi = 300)

fig1b <- cowplot::plot_grid(p1, p2_2, ncol = 1, rel_heights = c(2, 1))
ggsave(fig1b, filename = here::here("img/Fig1b.pdf"), width = 7.22, height = 8.05, dpi = 300)

fig1b2 <- cowplot::plot_grid(p1, p2_2, ncol = 1, rel_heights = c(2, 1), labels = "AUTO")
ggsave(fig1b2, filename = here::here("img/Fig1b2.pdf"), width = 7.22, height = 8.05, dpi = 300)

fig1c <- cowplot::plot_grid(p1, p2_3, ncol = 1, rel_heights = c(1.5, 1))
ggsave(fig1c, filename = here::here("img/Fig1c.pdf"), width = 7.22, height = 8.05, dpi = 300)

















