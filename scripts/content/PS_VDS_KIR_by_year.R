###########################
#   PS_VDS_KIR_by_year_   #
###########################


##########################################################################
# This script calculates the number of total hours that ALL purse seiners
# spent in KIR waters.
##########################################################################

#### SETUP ########################################################
# Load packages
library(tidyverse)

# Load data
# Filter to keep only purse seiners before 2018 and modify the fishing label
vessel_activity <- readRDS(file = here::here("raw_data",
                                             "vessel_tracks_ALL_vessels_fished_PNA_2012_present.rds")) %>% 
  filter(eez_iso3 == "KIR",
         inferred_label == "tuna_purse_seines",
         year < 2018) %>% 
  mutate(fishing = ifelse(is.na(fishing), F, fishing),
         fishing = ifelse(fishing, "Fishing", "Non-fishing"),
         group = ifelse(treated, "Treatment", "Control"),
         group = ifelse(is.na(treated), "Others", group),
         group = fct_relevel(group, c("Treatment", "Control", "Others")))

#### CREATE THE DATA ################################################

## All Vessels yearly #########

# Data for yearly vessel activity (fishing not fishing)
vessel_activity_year <- vessel_activity %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

#### PLOTS ##########################################################
# Plot for yearly PS VDS by activity
p1 <- ggplot(vessel_activity_year, aes(x = year, y = days, color = group)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days")

#Save plot
ggsave(p1, filename = here::here("docs", "img", "PS_VDS_KIR_by_year.pdf"), width = 6, height = 3.5)

