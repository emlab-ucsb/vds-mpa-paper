
# Load the packages that I need
library(dplyr)
library(dbplyr)
library(DBI)
library(bigrquery)
library(magrittr)

source(here::here("scripts", "functions", "st_rotate.R"))

# Create a connection to Google BigQuery
# For this to run, you require authentication and be part of ucsb-gfw
BQc <- bigrquery::dbConnect(
  drv = bigrquery::bigquery(),
  project = "ucsb-gfw", # This is the project, to which you must belong
  dataset = "mpa_displacement", # This is the dataset, where I keep all the tables
  allowLargeResults = TRUE #We're downloading about 10 GB of data
)

# List all tables to test the connection
DBI::dbListTables(BQc)


# DOWNLOAD THE DATA
# Create a tbl pointing to vessel tracks
vessel_tracks <- dplyr::tbl(BQc, "sample_treated_control_vessels") %>%
  collect() # collect brings the information out of the database and into memory

pipa <- sf::read_sf(dsn = here::here("data", "spatial", "PIPA"), layer = "PIPA") %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_rotate()

vessel_tracks %>% 
  mutate(year = lubridate::year(timestamp),
         lon = ifelse(lon < 0, lon + 360, lon)) %>% 
  filter(fishing, year < 2015) %>% 
  ggplot() +
  geom_point(aes(x = lon, y = lat, color = treated)) +
  geom_sf(data = pipa, fill = "transparent", color = "red", size = 1) +
  cowplot::theme_cowplot() +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Group")) +
  theme(text = element_text(size = 12))
