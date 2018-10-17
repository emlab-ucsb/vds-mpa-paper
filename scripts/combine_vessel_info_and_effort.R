#########################
#     Generate data     #
#########################

# Load packages
library(tidyverse)
library(magrittr)

# Put it all together

PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

## Load and bind vessel info
vessel_info_pipa <- read.csv(here::here("data", "vessel_info_mmsi_inside_pipa.csv"),
                             stringsAsFactors = F) %>% 
  mutate(treated = T)

vessel_info_pna <- read.csv(here::here("data", "vessel_info_mmsi_inside_pna.csv"),
                            stringsAsFactors = F) %>% 
  mutate(treated = F)

vessel_info <- rbind(vessel_info_pipa, vessel_info_pna) %>% 
  rename(gear = inferred_label) %>% 
  select(mmsi, year, iso3, gear)

memory.limit(size = 8e6)

## Get effort data
effort_by_vessel <- readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>%
  filter(fishing) %>% 
  group_by(post, treated, year, month, date, gear, mmsi) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(vessel_info, by = c("mmsi", "year", "gear")) %>%
  mutate(month_c = as.character(month),
         year_c = as.character(year),
         year_month = lubridate::date(paste(year, month, 1, sep = "/")),
         quarter = lubridate::quarter(date, with_year = T),
         PNA = iso3 %in% PNA_countries)

# Identify vessels suitable for BACI
# I want to make sure I don't use vessels that appear after 2015 (in the control) or that disappeared after 2015 (in the treatment).

tb <- effort_by_vessel %>% 
  filter(treated, !post, date < lubridate::date("2014/09/01")) %$%
  mmsi %>%
  unique()

ta <- effort_by_vessel  %>% 
  filter(treated, post) %$%
  mmsi %>%
  unique()

cb <- effort_by_vessel %>% 
  filter(!treated, !post, date < lubridate::date("2014/09/01")) %$%
  mmsi %>%
  unique()

ca <- effort_by_vessel %>% 
  filter(!treated, post) %$%
  mmsi %>%
  unique()

mmsi_baci_strict <- c(ta[ta %in% tb], ca[ca %in% cb])
mmsi_baci_relaxed <- c(tb, ca[ca %in% cb])

effort_by_vessel %<>%
  mutate(baci_strict = mmsi %in% mmsi_baci_strict,
         baci_relaxed = mmsi %in% mmsi_baci_relaxed,
         experiment1 = treated | PNA,
         experiment2 = treated | (!treated & iso3 == "TWN"),
         experiment3 = treated | (!treated & !iso3 == "CHN"),
         iso3 = ifelse(iso3 == "" | is.na(iso3), "OTH", iso3)) %>% 
  select(year,
         month,
         year_month,
         quarter,
         date,
         gear,
         flag = iso3,
         mmsi,
         hours,
         treated,
         post,
         baci_strict,
         baci_relaxed,
         month_c,
         year_c,
         experiment1,
         experiment2,
         experiment3)

# Save data

saveRDS(effort_by_vessel, file = here::here("data", "effort_by_vessel.rds"))


# # Generate a list of mmsis, group (treated-post), and gear.
# # I'll use this one to get the full tracks from BigQuery
# 
# vessel_groups <- effort_by_vessel %>%
#   filter(baci_strict) %>%
#   group_by(mmsi, treated, gear, flag) %>%
#   count() %>%
#   select(-n)
# 
# write.csv(vessel_groups, file = here::here("data", "vessel_groups.csv"), row.names = F)
