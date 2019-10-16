###############################
#   spatial_redistribution    #
###############################


########################################################################
# This script calculates the change in proportion of allocation of
# fishing effort from TREATED vessels across the units of space that
# we defined when we rasterized the shapefiles.
########################################################################

#### SET UP ############################################################
# Load packages
library(here)
library(tidyverse)

# Monthly  for analyses
raster_month_region <- readRDS(file = here("data", "monthly_rasterized_effort_by_region.rds")) %>% 
  filter(treated == "Treated")


redistribution_model_data <-
  expand.grid(id = unique(raster_month_region$id),
              year = 2012:2018,
              month = 1:12) %>% 
  arrange(id, year, month) %>%
  as.tibble() %>%
  left_join(raster_month_region, by = c("id", "year", "month")) %>% 
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours, na.rm = t)) %>% 
  ungroup() %>% 
  group_by(year, month, id, total_hours) %>%
  summarize(h = sum(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(h_prop = h / total_hours) %>% 
  mutate(h_prop = ifelse(is.na(h_prop), 0, h_prop)) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")),
         post = year >= 2015,
         year_c = as.factor(year)) %>% 
  group_by(id) %>% 
  mutate(max_p = max(h_prop)) %>% 
  ungroup() %>% 
  filter(max_p > 0.07)

model <- lm(h_prop ~ year_c * id, data = redistribution_model_data)


## FIGURES #################################################################

##########################################################################
# Monthly relative allocation of fishing effort by PIPA-fishing vessels
# before and after PIPA by region. Colors indicate the pre-post period.
# Solid line across points shows local mean with a region-specific loess
# smoother. Solid line indicates PIPA closurePIPA closure.
###########################################################################


terms_order <- c("PIPA-PIPA",
                 "EEZ-KIR",
                 "HS-KIR",
                 "HS-HS")

plot <- redistribution_model_data %>% 
  mutate(id = str_remove(id, "\\s[:digit:]"),
         id = str_replace(id, "\\s" ,"-"),
         id = fct_relevel(id, terms_order),
         id_group = ) %>%
  ggplot(aes(x = date, y = h_prop)) +
  geom_point(aes(color = post)) +
  scale_color_brewer(palette = "Set1") +
  geom_smooth(method = "loess", color = "black") +
  geom_vline(xintercept = lubridate::date("2015/01/01"), linetype = "dashed") +
  facet_wrap(~id, scales = "free_y") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Date", y = "Percent of total hours per month")

ggsave(plot = plot,
       filename = here("docs", "img", "redist_trend.pdf"),
       height = 4,
       width = 7)





###########################################################################
# Coefficient estimates for the redistribution regression. Each pannel
# shows the region-specific coefficients, with heteroskedastick-robust
# standard errors as error bars. The horizontal dashed line represents 0
# chenge relative to the 2012 region-specific levels.
###########################################################################

terms_order2 <- c("PIPA PIPA 1",
                  "EEZ KIR 1",
                  "EEZ KIR 2",
                  "EEZ KIR 3",
                  "HS KIR 1",
                  "HS KIR 2",
                  "HS KIR 3",
                  "HS HS 1")

tval <- qt(p = 0.975, df = model$df.residual)

plot2 <- commarobust::commarobust_tidy(model) %>%
  filter(str_detect(term, ":")) %>% 
  mutate(ci = est - tval * se,
         CI = est + tval * se,
         year = as.numeric(str_extract(term, "[:digit:]+")),
         term = str_extract(term, pattern = "[:alpha:]+ [:alpha:]+ [:digit:]"),
         p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"),
         term = str_replace(term, "id", ""),
         term = fct_relevel(term, terms_order2)) %>% 
  ggplot(aes(x = year, y = est, color = p, group = term)) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = ci, ymax = CI + se), width = 0, size = 1) +
  geom_line(color = "black") +
  geom_point(size = 2) +
  cowplot::theme_cowplot() +
  theme(strip.background = element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none") +
  guides(color = guide_legend(title = "p-value")) +
  facet_wrap(~term, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Date", y = "Coefficient Estimate") +
  scale_x_continuous(breaks = c(2013, 2015, 2017))

ggsave(plot = plot2,
       filename = here("docs", "img", "mean_change.pdf"),
       height = 3.5, width = 6)


######## TABLE ##########################################################################

##############################################################
# Coefficient estimates for the interaction of year and region.
# Each row represents a region, each color a year. Numbers 
# in parentheses are heteroskedatic-robust standard errors.
# R2 = 0.47 (F(95, 1056) = 9.99; p < 0.001). * < 0.1; ** p < 0.05; ***p < 0.01.
##############################################################

commarobust::commarobust_tidy(model) %>%
  filter(str_detect(term, ":")) %>%
  mutate(year = as.numeric(str_extract(term, "[:digit:]+")),
         term = str_extract(term, pattern = "[:alpha:]+ [:alpha:]+ [:digit:]"),
         term = str_replace(term, "id", ""),
         term = fct_relevel(term, terms_order2)) %>% 
  mutate(est = formatC(est, digits = 3, format = "f"),
         se = formatC(se, digits = 3, format = "f"),
         p1 = ifelse(p < 0.1, "*", ""),
         p2 = ifelse(p < 0.05, "*", ""),
         p3 = ifelse(p < 0.01, "*", ""),
         p = paste0(p1, p2, p3),
         est = paste0(est, " (", se, ")", p)) %>% 
  select(year, term, est) %>% 
  spread(year, est) %>% 
  rename(Coefficient = term) %>% 
  knitr::kable(format = "latex",
               caption = "\\label{tab:mean_change}Coefficient estimates for the interaction of year and region. Each row represents a region, each color a year. Numbers in parentheses are heteroskedatic-robust standard errors.",
               booktabs = T) %>% 
  cat(file = here::here("docs", "tab", "mean_change.tex"))
















