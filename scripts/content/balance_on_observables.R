##########################
# Balance on observables
##########################

####################################################
# This script the vessel information that we have
# to create a balance table and a balance histogram
# What want to check is if the vessels have different
# characteristics, which may influence the location
# where they fish.
####################################################

# Load packages
library(here)
library(ggridges)
library(tidyverse)

# Load vessel info
vessel_info <- read.csv(file = here("raw_data", "vessel_info_pna_purse_seines.csv")) %>% 
  mutate(ssvid = as.character(ssvid))

# Load vessel activity (this has the treatment groups)
vessel_activity <- readRDS(file = here("raw_data",
                                       "activity_by_vessel_year_eez.rds"))
# Get the group for each ssvid
vessel_treatments <- vessel_activity %>% 
  mutate(treated = ifelse(treated, "Displaced", "Non-displaced")) %>% 
  group_by(ssvid, treated) %>% 
  count() %>% 
  ungroup() %>% 
  select(-n)

# Combien both datasets, and clean-up
balance_table <- vessel_info %>% 
  left_join(vessel_treatments, by = "ssvid") %>% 
  mutate(group = ifelse(is.na(treated), "Others", treated),
         group = fct_relevel(group, "Non-displaced", "Others", "Displaced")) %>% 
  select(-c(flag, treated)) %>% 
  gather(measure, value, -c(ssvid, group)) %>% 
  mutate(measure = case_when(measure == "crew_size" ~ "Crew size (n)",
                             measure == "engine_power_kw" ~ "Engine Power (KW)",
                             measure == "length_m" ~ "Length (m)",
                             T ~ "Tonnage (GT)"))


# Create a density plot for each variable
density_plot <- balance_table %>% 
  ggplot(aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~measure, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  ggtheme_plot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(1, 1),
        legend.position = c(1, 0.3)) +
  labs(x = "Value", y = "Density")

# Expor it
ggsave(plot = density_plot,
       filename = here("docs", "img", "balance_density_plot.pdf"),
       width = 6.8,
       height = 5.4)

# Create a function that returns mean (sd) for a variable
mean_sd <- function(x) {
  m <- round(mean(x), 2)
  sd <- round(sd(x), 2)
  
  paste0(m, " (", sd, ")")
}

# Function to apply ttest
my_ttest <- function(data){
  t.test(value ~ group, data = data)
}

# Function to apply broom::tidy
my_broom <- function(x) {
  broom::tidy(x) %>% 
    select(estimate, statistic, p.value)
}

# Calculate t-test
ttest <- balance_table %>% 
  filter(!group == "Others") %>% 
  select(-ssvid) %>% 
  group_by(measure) %>% 
  nest() %>% 
  mutate(ttest = map(data, my_ttest),
         tidy = map(ttest, my_broom))

# get only the stars
ttest_stars <- ttest %>% 
  select(measure, tidy) %>% 
  unnest(cols = tidy) %>% 
  mutate(star = case_when(p.value < 0.001 ~ "**",
                          p.value < 0.01 ~ "*",
                          T ~ "")) %>% 
  select(measure, star)

# Create balance table and export
balance_table %>% 
  group_by(group, measure) %>% 
  summarize(value = mean_sd(value)) %>% 
  ungroup() %>% 
  spread(group, value) %>% 
  select(measure, `Non-displaced`, Displaced, Others) %>% 
  left_join(ttest_stars, by = "measure") %>% 
  mutate(Displaced = paste0(Displaced, star)) %>% 
  select(-star) %>% 
  rename(Characteristic = measure) %>% 
  knitr::kable(format = "latex",
               caption = "Average values on observable characteristics by vessel for displaced (n = 88), non-displaced (n = 97), and other vessels (n = 128). Numbers in parentheses indicate standard deviation. Asterisks inicate significant differences between the displaced and non-displaced groups using a two-tailed t-test (* p < 0.01; ** p < 0.001).") %>% 
  cat(file = here("docs", "tab", "balance_table.tex"))




















  