# This script generates a 4-panel figure.
# The panels contain:
# - vessel-days for Kiribati
# - vessel-days for all PNA
# - historical revenue from licenses
# - licenses vs AIS-derived vessel-days




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


# Annual vessel-days in PNA countries by group (treated, control, others)
all_PS_VDS_year_data <- vessel_activity %>% 
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

# Bar plot
all_PS_VDS_year_plot <- 
  ggplot(data = all_PS_VDS_year_data,
         mapping = aes(x = year, y = days, fill = group)) +
  geom_col(color = "black") +
  geom_hline(yintercept = 45, linetype = "dashed") +
  cowplot::theme_cowplot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1),
        legend.position = c(0, 1))+
  labs(x = "Year", y = "Vessel-days (1,000)") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("All PNA nations")

############ PS VDS for KIR
all_PS_VDS_year_KIR_data <- vessel_activity %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

# Bars for total VDS in KIR
all_PS_VDS_KIR_year_plot <- 
  ggplot(data = all_PS_VDS_year_KIR_data,
         mapping = aes(x = year, y = days, fill = group)) +
  geom_col(color = "black") +
  geom_hline(yintercept = 11, linetype = "dashed") +
  cowplot::theme_cowplot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = "None")+
  labs(x = "Year", y = "Vessel-days (1,000)") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Kiribati")

# Financial data
financial_data <- read.csv(file = here("data", "financial_data.csv")) %>% 
  mutate(country = fct_relevel(country, "KIR"))

# Plot licenses
license_revenues <- financial_data %>% 
  drop_na(revenue) %>% 
  ggplot(mapping = aes(x = year, y = revenue, fill = country)) +
  geom_line(size = 0.5) +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  geom_vline(xintercept = 2015,
             linetype = "dashed") +
  theme_cowplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Revenue from licenses\n(Million USD)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  guides(fill = guide_legend(ncol = 2,
                             title = "Country"))

vessel_activity_ffa <- vessel_activity %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days) / 1000) %>% 
  left_join(financial_data, by = c("year", "eez_iso3" = "country")) %>% 
  drop_na(revenue) %>% 
  rename(country = eez_iso3) %>% 
  mutate(country = fct_relevel(country, "KIR"))

revenue_FFA_GFW <-
  ggplot(vessel_activity_ffa, aes(x = days, y = revenue)) +
  geom_smooth(method = "lm",
              linetype = "dashed",
              color = "black",
              se = T) +
  geom_point(aes(fill = country),
             size = 3,
             shape = 21,
             alpha = 0.7) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  theme_cowplot() +
  theme(text = element_text(size = 10)) +
  labs(x = "Vessel-days (1,000)",
       y = "Reported revenue\n(million USD)") +
  scale_size_continuous(breaks = c(1, 5, 10))

#Plot all
p <- plot_grid(all_PS_VDS_KIR_year_plot,
               all_PS_VDS_year_plot,
               license_revenues,
               revenue_FFA_GFW,
               ncol = 2,
               labels = "auto")

#Save plot
ggsave(p,
       filename = here("docs", "img", "empirical.pdf"),
       width = 6.8,
       height = 5.4)
