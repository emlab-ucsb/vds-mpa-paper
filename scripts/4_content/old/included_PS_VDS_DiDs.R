#############################
#   included_PS_VDS_DiDs    #
#############################

##########################################################
# This script creates two plots. The first plot compares
# the number of yearly vessel-days applied by the treated and
# control fleets to PNA countries and TKL. The figure shows
# the how treated vessels (the ones that fished inside PIPA)
# eventually leave the PNA, while control vessels continue
# to fish in PNA waters. 
# The second figure does the same, but by country. This other
# figure shows us who won and lost VDS after PIPA.
##########################################################

#### SETUP ########################################################
# Load packages
library(cowplot)
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
         fishing = ifelse(fishing, "Fishing", "Non-fishing"),
         group = ifelse(treated, "Treatment", "Control"),
         group = ifelse(is.na(treated), "Others", group))

#### CREATE THE DATA ################################################


## Treated Vessels yearly ####

# Data for yearly vessel activity (fishing not fishing) for included / excluded
vessel_activity_year_included <- vessel_activity %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

# Data for yearly vessel activity (fishing not fishing) for included / excluded
vessel_activity_year_included_KIR <- vessel_activity %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

# Data for eez-level days for included / excluded
vessel_activity_year_country_included <- vessel_activity %>% 
  group_by(year, eez_iso3, group) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

# Plot for yearly PS VDS by activity by included vessels
p1 <- ggplot(vessel_activity_year_included, aes(x = year, y = days/1000, color = group)) +
  geom_line() +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  guides(color = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(1, 0),
        legend.position = c(1, -0.05))+
  labs(x = "Year", y = "Vessel-days (Ths)") +
  geom_vline(xintercept = 2015, linetype = "dashed")

# Plot for yearly PS VDS by activity by included vessels
p2 <- ggplot(vessel_activity_year_included_KIR, aes(x = year, y = days/1000, color = group)) +
  geom_line() +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  guides(color = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "none")+
  labs(x = "Year", y = "Vessel-days (Ths)") +
  geom_vline(xintercept = 2015, linetype = "dashed")

p12 <- plot_grid(p1, p2, ncol = 1, labels = "AUTO")

#Save plot
ggsave(p12,
       filename = here::here("docs", "img", "included_PS_VDS_year_DiD.pdf"),
       width = 3.4,
       height = 4.4)


# Plot for yearly PS VDS by country
p2 <- ggplot(vessel_activity_year_country_included, aes(x = year, y = days, color = group)) + 
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days") +
  facet_wrap(~eez_iso3, scales = "free_y")

#Save plot
ggsave(p2,
       filename = here::here("docs", "img", "included_PS_VDS_cty_DiD.pdf"),
       width = 8,
       height = 5)
