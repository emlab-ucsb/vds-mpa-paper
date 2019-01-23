###################################################
#   Calculate VDs per year and per country-year   #
###################################################

# Load packages
library(tidyverse)

# Load tracks. We want to keep only the tracks of information
# o
# 
tracks <- readRDS(file = here::here("raw_data", "vessel_tracks_baci.rds")) %>% 
  filter(VDS,
         gear == "purse_seines")
  

p1 <- tracks %>% 
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year) %>% 
  summarize(total_hours = sum(hours)) %>% 
  ggplot(aes(x = year, y = total_hours)) +
  geom_col()

ggsave(p1, filename = here::here("img", "VDS_year.pdf"))


p2 <- tracks %>%
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year, eez_iso3) %>% 
  summarize(total_hours = sum(hours)) %>% 
  ggplot(aes(x = eez_iso3, y = total_hours)) +
  geom_col() +
  facet_wrap(~year)

ggsave(p2, filename = here::here("img", "VDS_country_year.pdf"), width = 8, height = 4)
