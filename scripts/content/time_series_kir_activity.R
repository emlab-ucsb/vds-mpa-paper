################################
##  time_series_kir_activity  ##
################################

#############################################################
# This script generates tow figures. One shows vessel-days
# by each patch of EEZs that Kiribati has and by gear. This
# essentially shows that the loss in revenue from longlines
# comes after 2016, but that fishing effort in Phoenix group
# decreased after 2015 for both vessels. The second graph
# generates the timeseries of all vessel-activity in Kiribati
# by gear.
#############################################################


# Load libraries
library(lubridate)
library(here)
library(cowplot)
library(tidyverse)

# Nino4 index
nino4 <- read.csv(here("data", "all_indices.csv")) %>% 
  select(year, month = month_n, nino4anom)

longline_kir <- read.csv(here("raw_data", "longlines_KIR_2012_2018.csv")) %>% 
  mutate(date = date(paste(year, month, 1, sep = "-")),
         days = hours / 24,
         label = ifelse(best_vessel_class == "drifting_longlines",
                        "Drifting Longlines",
                        "Tuna Purse Seines")) %>% 
  left_join(nino4, by = c("year", "month"))


time_series_gear_group <- ggplot(data = longline_kir,
       mapping = aes(x = date, y = days, color = territory1, fill = territory1)) +
  geom_line() +
  geom_point(color = "black",
             size = 1,
             shape = 21,
             alpha = 0.8) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        strip.background = element_blank()) +
  labs(x = "Date", y = "Vessel-days") +
  geom_vline(xintercept = date("2015-01-01")) +
  facet_wrap(territory1 ~ label, scales = "free_y", ncol = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Group"),
         fill = guide_legend(title = "Group"))

ggsave(plot = time_series_gear_group,
       file = here("docs", "img", "time_series_gear_group.pdf"),
       width = 6.5,
       height = 4)


time_series_ll <- longline_kir %>% 
  filter(best_vessel_class == "drifting_longlines") %>% 
  group_by(date) %>%
  summarize(days = sum(days)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = days)) +
  geom_point(fill = "steelblue",
             alpha = 0.8,
             color = "black",
             size = 3,
             shape = 21) +
  geom_smooth(method = "loess",
              color = "black") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(x = "Date", y = "Vessel-days") +
  geom_vline(xintercept = date("2015-01-01"))

ggsave(plot = time_series,
       file = here("docs", "img", "time_series_vd_KIR.pdf"),
       width = 6.5,
       height = 4)

time_series_ps <- longline_kir %>% 
  filter(best_vessel_class == "tuna_purse_seines") %>% 
  group_by(date, nino4anom) %>%
  summarize(days = sum(days)) %>%
  ungroup() %>% 
  ggplot(aes(x = date, y = days)) +
  geom_point(#fill = "steelblue",
    aes(fill = nino4anom),
             alpha = 0.8,
             color = "black",
             size = 3,
             shape = 21) +
  geom_smooth(method = "loess",
              color = "black") +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  labs(x = "Date", y = "Vessel-days") +
  geom_vline(xintercept = date("2015-01-01"))

time_series <- plot_grid(time_series_ll,
                         time_series_ps,
                         labels = "AUTO")


ggsave(plot = time_series,
       file = here("docs", "img", "time_series_vd_KIR.pdf"),
       width = 6.5,
       height = 4)

# nino4 vs vessel days by group of islands
longline_kir %>% 
  filter(best_vessel_class == "tuna_purse_seines") %>% 
  group_by(date, nino4anom, territory1) %>%
  summarize(days = sum(days)) %>% 
  ungroup() %>% 
  mutate(post = ifelse(date > date("2015-01-01"), "Post", "Pre")) %>% 
  ggplot(aes(x = nino4anom, y = days)) +
  geom_point(size = 3, aes(color = post)) +
  geom_smooth(method = "lm") +
  facet_wrap(~territory1, ncol = 2, scales = "free_y")

time_series_by_vessel <- readRDS(here("raw_data", "time_series_kir_activity_by_vessel.rds")) %>% 
  mutate(date = date(paste(year, month, 1, sep = "-"))) %>% 
  group_by(date, year, month, territory1) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(nino4, by = c("year", "month")) %>% 
  mutate(post = ifelse(date > date("2015-01-01"), "Post", "Pre"),
         post = fct_relevel(post, "Pre"))


ggplot(data = time_series_by_vessel,
       mapping = aes(x = nino4anom, y = n)) +
  geom_point(size = 3, aes(color = post)) +
  geom_smooth(method = "lm") +
  facet_wrap(~territory1, ncol = 2, scales = "free_y")


ggplot(data = time_series_by_vessel,
       mapping = aes(x = post, y = n)) +
  stat_summary(geom = "col", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0) +
  facet_wrap(~territory1, ncol = 2, scales = "free_y")
  
  
  
  
  
  
  
  








