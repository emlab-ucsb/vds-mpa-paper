#######################
#     change_in_vds   #
#######################

######################################################################
# This script calculates the number of total hours that purse seiners
# spent in PNA waters (PNA includes TKL).
######################################################################

#### SETUP ########################################################
# Load packages
library(tidyverse)

# Load tracks
tracks <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(gear == "tuna_purse_seines",
         year < 2018) %>% 
  mutate(fishing = ifelse(fishing, "Fishing", "Non-fishing"))

# List of PNA countries
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

# List of countries that hold VDS rights (and then sell to others)
VDS_countries <- c(PNA_countries, "TKL")

#### CREATE THE DATA ################################################

# PNA-Year
all_vds <- tracks %>% 
  filter(eez_iso3 %in% VDS_countries) %>% 
  group_by(year, fishing) %>% 
  summarize(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  mutate(days = total_hours / 24)

# Country-year
country_year <- tracks %>%
  filter(eez_iso3 %in% VDS_countries) %>% 
  group_by(fishing, year) %>% 
  mutate(total_year = sum(hours)) %>% 
  ungroup() %>% 
  group_by(fishing, year, eez_iso3, total_year) %>% 
  summarize(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  mutate(prop_hours = total_hours / total_year,
         days = total_hours / 24)


#### PLOTS ########################################################

# Plot for VDS in all PNA
p1 <- ggplot(data = all_vds, aes(x = year, y = days, fill = fishing)) +
   geom_col(color = "black") +
   scale_fill_brewer(palette = "Set1") +
   cowplot::theme_cowplot() +
   theme(text = element_text(size = 10),
         axis.text = element_text(size = 8))+
   labs(x = "Year", y = "Vessel-days")

ggsave(p1, filename = here::here("docs", "img", "VDS_year.pdf"), width = 6, height = 3.5)

# Plot for days by country
p2 <- ggplot(country_year, aes(x = year, y = days, fill = eez_iso3)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.justification = c(0,1),
        legend.position = c(0,1))+
  guides(fill = guide_legend(title = "Country")) +
  labs(x = "Year", y = "Vessel-days") +
  facet_wrap(~fishing, scale = "free_y")

# Plot for days by country, relative
p3 <- ggplot(country_year, aes(x = year, y = prop_hours, fill = eez_iso3)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.position = "none")+
  labs(x = "Year", y = "Vessel-days") +
  facet_wrap(~fishing) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

p23 <- cowplot::plot_grid(p2, p3, ncol = 1)

ggsave(p23, filename = here::here("docs", "img", "VDS_country_year.pdf"), width = 7, height = 7)
