#########################
#   hist_fishing_kir    #
#########################

###########################################################################
# Make histograms of the % of total fishing hours that take place within
# Kiribati EEZ waters by year for treated vessels where the unit of
# observation is an individual vessel
###########################################################################


#### SET UP ###########################################################################

# Load packages
library(tidyverse)

# Load data
# I only want fishing tracks of purse seiners inside KIR before 2018
tracks <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(gear == "tuna_purse_seines",
         year < 2018,
         fishing) 

#### THE DATA ########################################################################

# Generate the data
kir_fishing <- tracks %>% 
  group_by(mmsi, year, year_c) %>% 
  mutate(total_hours = sum(hours)) %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(mmsi, year, year_c, total_hours) %>% 
  summarize(kir_hours = sum(hours)) %>% 
  ungroup() %>% 
  mutate(prop_hours = kir_hours / total_hours) %>% 
  arrange(year, mmsi)

kir_n <- tracks %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(year, treated) %>% 
  summarize(n = n_distinct(mmsi)) %>% 
  ungroup() %>% 
  spread(treated, n) %>% 
  magrittr::set_colnames(c("year", "control", "treated")) %>% 
  mutate(label = paste("C = ", control, "; T = ", treated),
         hours = 1.1,
         year = year + 0.8)

#### THE PLOT ########################################################################

hist_kir_fishing <- ggplot() +
  ggridges::geom_density_ridges(data = kir_fishing, aes(x = prop_hours, y = year, fill = year_c), alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")+
  guides(fill = guide_legend(title = "Year")) +
  labs(x = "Fishing hours", y = "Year") +
  geom_text(data = kir_n, aes(x = hours, y = year, label = label)) +
scale_x_continuous(labels = scales::percent_format(accuracy = 1))

ggsave(hist_kir_fishing, filename = here::here("docs", "img", "hist_kir_fishing.pdf"), width = 6, height = 3.5)


