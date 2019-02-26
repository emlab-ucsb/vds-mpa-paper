###############################
#   PS_VDS_PNA_by_year_eez    #
###############################


##########################################################################
# This script calculates the number of total hours that ALL purse seiners
# spent in PNA waters (PNA includes TKL).
##########################################################################

#### SETUP ########################################################
# Load packages
library(here)
library(ggridges)
library(tidyverse)

PNA_without_KIR <- c("FSM",
         # "KIR",
         "MHL",
         "NRU",
         "PLW",
         "PNG",
         "SLB",
         "TUV",
         "TKL")


# Load data
vessel_activity <- readRDS(file = here::here("raw_data",
                                             "activity_by_vessel_year_eez.rds")) %>% 
  filter(best_vessel_class == "tuna_purse_seines") %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         group = ifelse(is.na(treated), "others", treated),
         location = case_when(eez_iso3 == "KIR" ~ "KIR",
                              eez_iso3 %in% PNA_without_KIR ~ "other PNA countries",
                              eez_iso3 == "HS" ~"HS",
                              T ~ "Other countries"),
         days = hours / 24)

#### CREATE THE DATA ################################################


# Data for eez-level proportional allocations
vessel_prop_activity_year_loc <- vessel_activity %>% 
  group_by(year, group, ssvid, location) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, group, ssvid) %>% 
  mutate(total = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(proportion = days/total)

# Data for eez-level proportional
vessel_activity_year_loc <- vessel_activity %>% 
  group_by(year, group, location) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

#### PLOTS ########################################################

p1 <- ggplot(data = vessel_prop_activity_year_loc,
       mapping = aes(x = proportion, y = as.character(year), fill = group)) +
  geom_density_ridges(alpha = 0.5) +
  facet_wrap(~location) +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) +
  labs(x = "Vessel-level proportion of fishing days", y = "Year")

ggsave(plot = p1,
       file = here("docs", "img", "yearly_distribution_prop_fishing_by_region.pdf"),
       width = 6,
       height = 4)

p2 <- ggplot(data = vessel_prop_activity_year_loc,
       mapping = aes(x = year, y = proportion, color = group, fill = group)) +
  geom_jitter(height = 0, width = 0.2, color = "black", shape = 21, alpha = 0.5) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2) +
  stat_summary(geom = "line", fun.y = mean, size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~location, scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) +
  geom_vline(xintercept = 2014.5, linetype = "dashed") +
  labs(x = "Year", y = "Vessel-level proportion of fishing days")

ggsave(plot = p2,
       file = here("docs", "img", "yearly_prop_fishing_by_region.pdf"),
       width = 6,
       height = 4)

p3 <- ggplot(data = vessel_activity_year_loc,
       mapping = aes(x = year, y = days, color = group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~location, scale = "free_y") +
  scale_color_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) +
  geom_vline(xintercept = 2014.5, linetype = "dashed") +
  labs(x = "Year", y = "Total vessel-days")

ggsave(plot = p3,
       file = here("docs", "img", "yearly_total_fishing_by_region.pdf"),
       width = 6,
       height = 4)

filter(vessel_activity_year_loc, location %in% c("KIR", "PNA countries")) %>% 
  group_by(year, group) %>%
  summarize(days = sum(days, na.rm = T)) %>% 
  ggplot(aes(x = year, y = days, fill = group)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 45000, linetype = "dashed")

filter(vessel_activity_year_loc, location %in% c("KIR", "PNA countries")) %>% 
  group_by(year, group) %>%
  summarize(days = sum(days, na.rm = T)) %>% 
  ggplot(aes(x = year, y = days, color = group)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1")

#### TABLES ###############


# Table of days for "other countries" by year
vessel_activity %>% 
  filter(location == "Other countries") %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  spread(year, days, fill = 0) %>% 
  knitr::kable()

# Table of countries that ahd no fishing before PIPA
vessel_activity %>% 
  filter(location == "Other countries",
         treated == "treated") %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  spread(year, days, fill = 0) %>% 
  gather(year, days, -eez_iso3) %>% 
  filter(year < 2015 & days == 0) %>% 
  spread(year, days) %>% 
  drop_na()























