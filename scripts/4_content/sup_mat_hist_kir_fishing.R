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
library(startR)
library(here)
library(tidyverse)
library(ggridges)

# Load data
# I only want fishing tracks of purse seiners inside KIR before 2018
tracks <- readRDS(file = here("raw_data", "activity_by_vessel_year_eez.rds")) %>% 
  filter(group == "displaced",
         year > 2013,
         eez_iso3 == "KIR")

#### THE DATA ########################################################################

# Generate the data
kir_fishing <- tracks %>% 
  select(ssvid, year, contains("hours")) %>% 
  mutate(prop_hours = fishing_hours / fishing_hours_in_PNA) %>% 
  arrange(year, ssvid) %>% 
  mutate(year_c = as.character(year))

#### THE PLOT ########################################################################

hist_kir_fishing <- ggplot() +
  geom_density_ridges(data = kir_fishing,
                      aes(x = prop_hours, y = year, fill = year_c),
                      alpha = 0.5,
                      quantile_lines = TRUE,
                      quantiles = 2) +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")+
  guides(fill = guide_legend(title = "Year")) +
  labs(x = "Proportion of fishing hours in KIR", y = "Year") +
scale_x_continuous(labels = scales::percent_format(accuracy = 1))

# Save the plot
ggsave(plot = hist_kir_fishing,
       filename = here::here("docs", "img", "hist_kir_fishing.pdf"),
       width = 6, height = 3.5)


