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
library(startR)
library(here)
library(janitor)
library(ggridges)
library(tidyverse)

# Load vessel info
vessel_info <- readRDS(file = here("raw_data",
                                   "vessel_info_pna_purse_seines.rds")) %>% 
  select(-length_factor)

vessel_activity <- readRDS(file = here("raw_data",
                                       "activity_by_vessel_year_eez.rds")) %>% 
  filter(year == 2014) %>% 
  select(ssvid, group, fishing_hours_in_PNA) %>% 
  distinct()

# Combien both datasets, and clean-up
balance_table <- vessel_info %>% 
  left_join(vessel_activity, by = c("ssvid", "group")) %>% 
  mutate(group = ifelse(group == "displaced", "Displaced", "Non-displaced"),
         group = fct_relevel(group, "Non-displaced")) %>% 
  select(-c(flag)) %>% 
  gather(measure, value, -c(ssvid, group)) %>% 
  mutate(measure = case_when(measure == "crew_size" ~ "Crew size (n)",
                             measure == "engine_power_kw" ~ "Engine Power (KW)",
                             measure == "length_m" ~ "Length (m)",
                             measure == "fishing_hours_in_PNA" ~ "PNA fishing in 2014 (hours)",
                             measure == "tonnage_gt" ~ "Tonnage (GT)"))


# Create a density plot for each variable
density_plot <- balance_table %>% 
  ggplot(aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~measure, scales = "free", ncol = 2) +
  ggtheme_plot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(1, 1),
        legend.position = c(1, 0.3)) +
  labs(x = "Value", y = "Density") +
  scale_fill_brewer(palette = "Set1")

# Expor it
ggsave(plot = density_plot,
       filename = here("docs", "img", "balance_density_plot.pdf"),
       width = 6.8,
       height = 5.4)

# Create a function that returns mean (sd) for a variable
mean_sd <- function(x) {
  m <- round(mean(x, na.rm = T), 2)
  sd <- round(sd(x, na.rm = T), 2)
  
  paste0(m, " (", sd, ")")
}

# Function to apply ttest
my_ttest <- function(data){
  t.test(value ~ group, data = drop_na(data))
}

# Function to apply broom::tidy
my_broom <- function(x) {
  broom::tidy(x) %>% 
    select(estimate, statistic, p.value) %>% 
    mutate(estimate = round(estimate, 2),
           statistic = round(statistic, 2))
}

# Calculate t-test
ttest <- balance_table %>% 
  select(-ssvid) %>% 
  group_by(measure) %>% 
  nest() %>% 
  mutate(ttest = map(data, my_ttest),
         tidy = map(ttest, my_broom))

# get only the stars
ttest_stars <- ttest %>% 
  select(measure, tidy) %>% 
  unnest(cols = tidy) %>% 
  mutate(star = case_when(p.value < 0.01 ~ "***",
                          p.value < 0.05 ~ "**",
                          p.value < 0.1 ~ "*",
                          T ~ ""),
         dif = paste0(estimate, " (", statistic, ") ", star)) %>% 
  select(measure, Difference = dif)

# Create balance table and export
balance_table %>% 
  group_by(group, measure) %>% 
  summarize(value = mean_sd(value)) %>% 
  ungroup() %>% 
  spread(group, value) %>% 
  select(measure, Displaced, `Non-displaced`) %>% 
  left_join(ttest_stars, by = "measure") %>% 
  rename(Characteristic = measure) %>% 
  knitr::kable(format = "latex",
               caption = "Mean values on observable characteristics by vessel for displaced (n = 64), and non-displaced vessels (n = 254). Numbers in parentheses indicate standard deviation. The last column contains the difference in means (t-scores), with asterisks inicating significant differences as indicated by a two-tailed t-test (* p < 0.1; ** p< 0.05; *** p < 0.01).") %>% 
  cat(file = here("docs", "tab", "balance_table.tex"))



# Create a graph for flag proportions
flags <- c("ESP", "TWN", "JPN", "CHN")

vessel_info %>% 
  mutate(group = ifelse(group == "displaced", "Displaced", "Non-displaced"),
         group = fct_relevel(group, "Non-displaced")) %>% 
  select(group, flag) %>% 
  mutate(flag = ifelse(is.na(flag), "Not reported", flag)) %>%
  group_by(group) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(group, flag, n) %>% 
  summarize(n2 = n()) %>% 
  ungroup() %>% 
  mutate(prop = n2 / n * 100) %>% 
  select(group, Flag = flag, prop) %>% 
  spread(group, prop, fill = 0) %>% 
  mutate(Flag = fct_relevel(Flag, "Not reported", after = Inf)) %>% 
  arrange(Flag) %>% 
  adorn_totals("row") %>% 
  knitr::kable(format = "latex",
               caption = "Proportion of vessel flags by group. Note that we do not observe the flag for two vessels (0.78\\%) in the non-displaced group.",
               digits = 2) %>% 
  cat(file = here("docs", "tab", "balance_table_flags.tex"))















  