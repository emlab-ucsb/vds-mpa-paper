#################################
#   figs_all_PS_VDS_2012_2018   #
#################################


###########################################################
# This script generates all the figures of VDS through time
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
  filter(best_vessel_class == "tuna_purse_seines") %>% 
  mutate(treated = ifelse(treated, "Displaced", "Non-displaced"),
         group = ifelse(is.na(treated), "Others", treated),
         group = fct_relevel(group, "Non-displaced", "Others", "Displaced"),
         location = case_when(eez_iso3 == "KIR" ~ "KIR",
                              eez_iso3 %in% PNA_without_KIR ~ "other PNA countries",
                              eez_iso3 == "HS" ~"HS",
                              T ~ "Other countries"),
         days = hours / 24)

# Annual vessel-days in PNA countries by group (treated, control, others)
all_PS_VDS_year_data <- vessel_activity %>% 
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

# Bar plot
all_PS_VDS_year_plot <- ggplot(data = all_PS_VDS_year_data,
                               mapping = aes(x = year, y = days, fill = group)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  geom_hline(yintercept = 45, linetype = "dashed") +
  cowplot::theme_cowplot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1),
        legend.position = c(-0.01, 0.98))+
  labs(x = "Year", y = "Vessel-days (1,000)")

#Save plot
ggsave(all_PS_VDS_year_plot,
       filename = here("docs", "img", "all_PS_VDS_year.pdf"),
       width = 3.4,
       height = 2.7)

# Line plot
all_PS_VDS_year_plot2 <-
  ggplot(data = all_PS_VDS_year_data,
         mapping = aes(x = year, y = days, color = group)) +
  geom_line() +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  guides(color = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")+
  geom_vline(xintercept = 2015, linetype = "dashed") +
  labs(x = "Year", y = "Vessel-days (1,000)")

############ PS VDS for KIR
all_PS_VDS_year_KIR_data <- vessel_activity %>% 
  filter(eez_iso3 == "KIR") %>% 
  group_by(year, group) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

all_PS_VDS_year_KIR_plot <-
  ggplot(data = all_PS_VDS_year_KIR_data,
         mapping = aes(x = year, y = days, color = group)) +
  geom_line() +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  guides(color = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(1, 0),
        legend.position = c(1, 0.5))+
  labs(x = "Year", y = "Vessel-days (1,000)") +
  geom_vline(xintercept = 2015, linetype = "dashed")

p12 <- plot_grid(all_PS_VDS_year_plot2,
                 all_PS_VDS_year_KIR_plot,
                 ncol = 1, labels = "AUTO")

#Save plot
ggsave(p12,
       filename = here("docs", "img", "included_PS_VDS_year_DiD.pdf"),
       width = 3.4,
       height = 4.4)

# Bars for total VDS in KIR
all_PS_VDS_KIR_year_plot <- 
  ggplot(data = all_PS_VDS_year_KIR_data,
       mapping = aes(x = year, y = days, fill = group)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  guides(fill = guide_legend(title = "Group")) +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(0, 1),
        legend.position = c(0, 1))+
  labs(x = "Year", y = "Vessel-days (1,000)")

#Save plot
ggsave(all_PS_VDS_KIR_year_plot,
       filename = here("docs", "img", "all_PS_VDS_KIR_year.pdf"),
       width = 6,
       height = 4)

# annual PS VDS by country

all_PS_VDS_cty_year_data <- vessel_activity %>% 
  filter(eez_iso3 %in% PNA_countries) %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days, na.rm = T) / 1000) %>% 
  ungroup()

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

ggsave(all_PS_VDS_cty_year_plot,
       filename = here("docs", "img", "all_PS_VDS_cty_year.pdf"),
       width = 6,
       height = 4)

################ PLOTS FOR QUENTIN

# Data for eez-level proportional allocations
vessel_prop_activity_year_loc <- vessel_activity %>% 
  group_by(year, group, ssvid, location) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year, group, ssvid) %>% 
  mutate(total = sum(days, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(proportion = days/total)

# Data for eez-level proportional
vessel_activity_year_loc <- vessel_activity %>% 
  group_by(year, group, location) %>% 
  summarize(days = sum(days, na.rm = T)) %>% 
  ungroup()

#### PLOTS ########################################################

p1 <- ggplot(data = vessel_prop_activity_year_loc,
       mapping = aes(x = proportion, y = as.character(year), fill = group)) +
  geom_density_ridges(alpha = 0.5) +
  facet_wrap(~location) +
  scale_fill_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) +
  labs(x = "Vessel-level proportion of fishing days", y = "Year")

ggsave(plot = p1,
       file = here("docs", "img", "yearly_distribution_prop_fishing_by_region.pdf"),
       width = 6,
       height = 4)


p3 <- ggplot(data = vessel_activity_year_loc,
       mapping = aes(x = year, y = days, color = group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~location, scale = "free_y") +
  scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8")) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank()) +
  geom_vline(xintercept = 2014.5, linetype = "dashed") +
  labs(x = "Year", y = "Total vessel-days")

ggsave(plot = p3,
       file = here("docs", "img", "yearly_total_fishing_by_region.pdf"),
       width = 6,
       height = 4)

p4 <- vessel_prop_activity_year_loc %>% 
  filter(location == "KIR",
         group == "Displaced",
         year > 2013) %>% 
  ggplot(mapping = aes(x = proportion,
                       y = as.character(year),
                       fill = as.character(year))) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  labs(x = "Vessel-level proportion of fishing days in KIR", y = "Year")

ggsave(plot = p4,
       file = here("docs", "img", "hist_KIR_fishing.pdf"),
       width = 6,
       height = 4)





















