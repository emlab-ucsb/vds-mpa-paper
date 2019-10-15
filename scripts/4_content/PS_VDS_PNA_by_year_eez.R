################################
#   PS_VDS_PNA_by_month_eez    #
################################


##########################################################################
# This script calculates the number of total hours that ALL purse seiners
# spent in PNA waters (PNA includes TKL).
##########################################################################

#### SETUP ########################################################
# Load packages
library(here)
library(tidyverse)

# Load data
# Filter to keep only purse seiners before 2018 and modify the fishing label
vessel_activity <- readRDS(file = here("raw_data",
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
         year < 2019) %>% 
  mutate(fishing = ifelse(is.na(fishing), F, fishing),
         fishing = ifelse(fishing, "Fishing", "Non-fishing"),
         group = ifelse(treated, "Treatment", "Control"),
         group = ifelse(is.na(treated), "Others", group),
         eez_iso3 = fct_relevel(eez_iso3, "KIR"))

#### CREATE THE DATA ################################################
# Data for monthly activity by EEZ
vessel_activity_month_country <- vessel_activity %>% 
  group_by(year, month, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")))

##### MONTHLY PLOTS ##############################################

# Plot of monthly vessel days per EEZ
p3 <- ggplot(vessel_activity_month_country,
             mapping = aes(x = date, y = days, color = eez_iso3, fill = eez_iso3)) + 
  geom_line() +
  geom_point(shape = 21,
             color = "black",
             size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())+
  labs(x = "Year", y = "Vessel-days") +
  facet_wrap(~eez_iso3, scales = "free_y") +
  geom_vline(xintercept = lubridate::date("2015-01-01"), linetype = "dashed") +
  guides(color = guide_legend(title = "Country"),
         fill = guide_legend(title = "Country"))

# Save plot
ggsave(p3,
       filename = here("docs", "img", "PS_VDS_PNA_by_month_eez.pdf"),
       width = 6,
       height = 4)

