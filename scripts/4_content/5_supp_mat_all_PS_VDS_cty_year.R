#################################
#   all_PS_VDS_cty_year  #
#################################


###########################################################
# This script generates VDS allocation by country and year
###########################################################

#### SETUP ########################################################
# Load packages
library(here)
library(ggridges)
library(cowplot)
library(tidyverse)

PNA_without_KIR <- c("FSM",
         "MHL",
         "NRU",
         "PLW",
         "PNG",
         "SLB",
         "TUV",
         "TKL")

PNA_countries <- c(PNA_without_KIR, "KIR")


# Load data
vessel_activity <- readRDS(file = here("raw_data",
                                       "activity_by_vessel_year_eez.rds")) %>% 
  mutate(group = ifelse(group == "displaced", "Displaced", "Non-displaced"),
         group = fct_relevel(group, "Non-displaced"),
         location = case_when(eez_iso3 == "KIR" ~ "KIR",
                              eez_iso3 %in% PNA_without_KIR ~ "other PNA countries",
                              eez_iso3 == "HS" ~"HS",
                              T ~ "Other countries"),
         days = hours_length / 24)

# annual PS VDS by country
all_PS_VDS_cty_year_data <- vessel_activity %>% 
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

# Plot
all_PS_VDS_cty_year_plot <-
  ggplot(data = all_PS_VDS_cty_year_data,
         mapping = aes(x = year, y = days, fill = eez_iso3)) + 
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 45, linetype = "dashed") +
  cowplot::theme_cowplot() +
  guides(fill = guide_legend(title = "Country")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8))+
  labs(x = "Year", y = "Vessel-days (1,000)")

# Save plot
ggsave(all_PS_VDS_cty_year_plot,
       filename = here("docs", "img", "all_PS_VDS_cty_year.pdf"),
       width = 6,
       height = 4)

ggsave(all_PS_VDS_cty_year_plot,
       filename = here("docs", "img", "all_PS_VDS_cty_year_ED_Fig1.tiff"),
       width = 6,
       height = 4)
