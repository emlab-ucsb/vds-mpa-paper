#########################
#     Generate data     #
#########################

# Load packages
library(tidyverse)
library(magrittr)

# Put it all together

## Load and bind vessel info
vessel_info_pipa <- read.csv(here::here("data", "vessel_info_mmsi_inside_pipa.csv"), stringsAsFactors = F) %>% 
  mutate(treated = T)

vessel_info_pna <- read.csv(here::here("data", "vessel_info_mmsi_inside_pna.csv"), stringsAsFactors = F) %>% 
  mutate(treated = F)

vessel_info <- rbind(vessel_info_pipa, vessel_info_pna) %>% 
  rename(gear = inferred_label)

## Get effort data
effort_by_vessel <- read.csv(here::here("data", "pipa_effort_by_vessel.csv"), stringsAsFactors = F) %>% 
  rbind(read.csv(here::here("data", "pna_vessels_outside_pipa.csv"), stringsAsFactors = F)) %>% 
  left_join(vessel_info, by = c("mmsi", "year")) %>% 
  filter(gear %in% c("purse_seines", "drifting_longlines")) %>%
  mutate(month_c = as.character(month),
         year_c = as.character(year),
         date = lubridate::date(date))

# Identify vessels suitable for BACI
# I want to make sure I don't use vessels that appear after 2015 (in the control) or that disappeared after 2015 (in the treatment).

tb <- effort_by_vessel %>% 
  filter(treated, !post) %$%
  mmsi %>%
  unique()

ta <- effort_by_vessel  %>% 
  filter(treated, post) %$%
  mmsi %>%
  unique()

cb <-  effort_by_vessel %>% 
  filter(!treated, !post) %$%
  mmsi %>%
  unique()

ca <-  effort_by_vessel %>% 
  filter(!treated, post) %$%
  mmsi %>%
  unique()

mmsi_baci_strict <- c(ta[ta %in% tb], ca[ca %in% cb])
mmsi_baci_relaxed <- c(tb, ca[ca %in% cb])

effort_by_vessel %<>%
  mutate(baci_strict = mmsi %in% mmsi_baci_strict,
         baci_relaxed = mmsi %in% mmsi_baci_relaxed) %>% 
  select(post, treated, baci_relaxed, baci_strict, flag = iso3, mmsi, year, year_c, month, month_c, date, hours, gear, gear_score = label_score, kir)

# Plot it

## All data
p1 <- group_by(effort_by_vessel, mmsi, date, gear, post, treated) %>%
  summarize(h = sum(hours, na.rm = T)) %>%
  arrange(treated, post, mmsi) %>%
  ggplot(aes(
    x = date,
    y = as.character(mmsi),
    size = h,
    fill = post,
    alpha = treated
  )) +
  geom_point(color = "black", shape = 21) +
  theme_bw() +
  facet_grid(treated ~ gear)

ggsave(p1, filename = here::here("img", "BACI_gear.png"), width = 15, height = 10)

## Just baci_relaxed
p2 <- filter(effort_by_vessel, baci_relaxed) %>% 
  group_by(mmsi, date, gear, post, treated) %>%
  summarize(h = sum(hours, na.rm = T)) %>%
  arrange(treated, post, mmsi) %>%
  ggplot(aes(
    x = date,
    y = as.character(mmsi),
    size = h,
    fill = post,
    alpha = treated
  )) +
  geom_point(color = "black", shape = 21) +
  theme_bw() +
  facet_grid(treated ~ gear)

ggsave(p2, filename = here::here("img", "BACI_relaxed_gear.png"), width = 15, height = 10)

## Just baci_strict
p3 <- filter(effort_by_vessel, baci_strict) %>% 
  group_by(mmsi, date, gear, post, treated) %>%
  summarize(h = sum(hours, na.rm = T)) %>%
  arrange(treated, post, mmsi) %>%
  ggplot(aes(
    x = date,
    y = as.character(mmsi),
    size = h,
    fill = post,
    alpha = treated
  )) +
  geom_point(color = "black", shape = 21) +
  theme_bw() +
  facet_grid(treated ~ gear)

ggsave(p3, filename = here::here("img", "BACI_strict_gear.png"), width = 15, height = 10)

# Save data

write.csv(effort_by_vessel, file = here::here("data", "effort_by_vessel.csv"))