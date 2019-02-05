###############################
#   PS_VDS_PNA_by_year_eez    #
###############################


##########################################################################
# This script calculates the number of total hours that ALL purse seiners
# spent in PNA waters (PNA includes TKL).
##########################################################################

#### SETUP ########################################################
# Load packages
library(tidyverse)

# Load data
# Filter to keep only purse seiners before 2018 and modify the fishing label
vessel_activity <- readRDS(file = here::here("raw_data",
                                             "vessel_tracks_ALL_vessels_fished_PNA_2012_present.rds")) %>% 
  filter(eez_iso3 %in% c("FSM",
                         "KIR",
                         "MHL",
                         "NRU",
                         "PLW",
                         "PNG",
                         "SLB",
                         "TUV",
                         "TKL"),
         inferred_label == "tuna_purse_seines",
         year < 2018) %>% 
  mutate(fishing = ifelse(is.na(fishing), F, fishing),
         fishing = ifelse(fishing, "Fishing", "Non-fishing"))

#### CREATE THE DATA ################################################


## All Vessels yearly #########

# Data for yearly vessel activity (fishing not fishing)
vessel_activity_year <- vessel_activity %>% 
  group_by(year, fishing) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

# Data for eez-level days
vessel_activity_year_country <- vessel_activity %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

## Treated Vessels yearly ####

# Data for yearly vessel activity (fishing not fishing) for included / excluded
vessel_activity_year_included <- vessel_activity %>% 
  filter(!is.na(treated)) %>%
  group_by(year, fishing) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

# Data for eez-level days for included / excluded
vessel_activity_year_country_included <- vessel_activity %>% 
  filter(!is.na(treated)) %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

## All Vessels Monthly by EEZ ####

# Data for monthly activity by EEZ
vessel_activity_month_country <- vessel_activity %>% 
  group_by(year, month, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

#### PLOTS ########################################################

# Plot for yearly PS VDS by activity
p1 <- ggplot(vessel_activity_year, aes(x = year, y = days, fill = fishing)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days") +
  geom_hline(yintercept = 45000, linetype = "dashed")

# Plot for yearly PS VDS by country
p2 <- ggplot(vessel_activity_year_country, aes(x = year, y = days, fill = eez_iso3)) + 
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days") +
  geom_hline(yintercept = 45000, linetype = "dashed")

# Combine plots
p12 <- cowplot::plot_grid(p1, p2, labels = "AUTO", ncol = 1)

#Save plot
ggsave(p12, filename = here::here("docs", "img", "all_PS_VDS_cty_year.pdf"), width = 6, height = 7)

# Plot for yearly PS VDS by activity by included vessels
p1.1 <- ggplot(vessel_activity_year_included, aes(x = year, y = days, fill = fishing)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days") +
  geom_hline(yintercept = 45000, linetype = "dashed")

# Plot for yearly PS VDS by country
p2.1 <- ggplot(vessel_activity_year_country_included, aes(x = year, y = days, fill = eez_iso3)) + 
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days") +
  geom_hline(yintercept = 45000, linetype = "dashed")

# Combine plots
p12.1 <- cowplot::plot_grid(p1.1, p2.1, labels = "AUTO", ncol = 1)

#Save plot
ggsave(p12.1, filename = here::here("docs", "img", "included_PS_VDS_cty_year.pdf"), width = 6, height = 7)

##### MONTHLY PLOTS ##############################################

# Plot of monthly vessel days per EEZ
p3 <- ggplot(vessel_activity_month_country, aes(x = date, y = days, color = eez_iso3)) + 
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())+
  labs(x = "Year", y = "Vessel-days") +
  facet_wrap(~eez_iso3, scales = "free_y") +
  geom_smooth()

# Save plot
ggsave(p3, filename = here::here("docs", "img", "all_PS_VDS_cty_month.pdf"), width = 6, height = 4)

