
# Load the packages that I need
library(dplyr)
library(dbplyr)
library(DBI)
library(bigrquery)
library(magrittr)

source(here::here("scripts", "functions", "st_rotate.R"))

# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
vessel_tracks <- get_table(project = "ucsb-gfw",
                           dataset = "mpa_displacement",
                           table = "sample_treated_control_vessels")

pipa <- sf::read_sf(dsn = here::here("data", "spatial", "PIPA"), layer = "PIPA") %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_rotate()

pna <- read_sf(dsn = here("data", "spatial", "PNA_EEZ"),
               layer = "PNA_EEZ") %>% 
  select(ISO_Ter1) %>% 
  st_simplify(10) %>% 
  st_rotate() %>% 
  st_union(by_feature = T)

sample_tracks <- vessel_tracks %>% 
  mutate(year = lubridate::year(timestamp),
         lon = ifelse(lon < 0, lon + 360, lon)) %>% 
  filter(fishing, year == 2014,
         between(lat, -15, 17),
         between(lon, 129, 213)) %>% 
  ggplot() +
  geom_sf(data = pna, fill = "gray90", alpha = 0.5, color = "black", size = 0.1) +
  geom_point(aes(x = lon, y = lat, fill = treated),
             size = 1,
             shape = 21,
             color = "black") +
  geom_sf(data = pipa, fill = "transparent", color = "red", size = 1) +
  ggtheme_map() +
  scale_fill_brewer(palette = "Set1") +
  theme(text = element_text(size = 12),
        legend.position = "None") +
  labs(x = "Longitude", y = "Latitude")

ggsave(plot = sample_tracks,
       filename = here("docs", "slides", "img", "sample_tracks.png"),
       width = 4.5,
       height = 2.5)
