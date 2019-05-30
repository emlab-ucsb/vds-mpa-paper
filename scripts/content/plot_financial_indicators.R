##################################
#   plot_financial_indicators    #
#################################

##################################################################
# This script produces plots of financial indicators
# related to each PNA country. The data com from the
# FFA (https://www.ffa.int/node/2050). The PDF report is in the
# /reports folder.
##################################################################

####### SET UP #########################

# Load libraries
library(cowplot)
library(here)
library(tidyverse)

financial_data <- read.csv(file = here("data", "financial_data.csv")) %>% 
  mutate(country = fct_relevel(country, "KIR"))

# Plot licenses
p1 <- financial_data %>% 
  drop_na(revenue) %>% 
  ggplot(mapping = aes(x = year, y = revenue, fill = country)) +
  geom_line(size = 0.5) +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  # facet_wrap(~country, scales = "free_y") +
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
        legend.position = c(0, 1.05)) +
  guides(fill = guide_legend(ncol = 2,
                             title = "Country"))

# inferred vs reported
# Load data
# Set up a vector of PNA countries
PNA_countries <- c("FSM",
                   "MHL",
                   "NRU",
                   "PLW",
                   "PNG",
                   "SLB",
                   "TUV",
                   "TKL",
                   "KIR")

#annual vds prices come from the FFA brochure
vds_price_per_year <- data.frame(
  year = c(2012,
           2013,
           2014,
           2015,
           2016,
           2017,
           2018),
  price = c(5000,
            5000,
            6000,
            8000,
            9000,
            12500,
            12500)
)


# Load vessel activity data
vessel_activity <- readRDS(file = here("raw_data",
                                       "activity_by_vessel_year_eez.rds")) %>% 
  filter(best_vessel_class == "tuna_purse_seines",
         eez_iso3 %in% PNA_countries) %>% 
  mutate(days = hours_length / 24) %>% 
  group_by(year, eez_iso3) %>% 
  summarize(days = sum(days)) %>% 
  left_join(vds_price_per_year, by = "year") %>% 
  mutate(price = ifelse(year <= 2013 & eez_iso3 == "TKL", NA, price),
    inferred_revenue = price * days / 1e6,
         days = days / 1000) %>% 
  left_join(financial_data, by = c("year", "eez_iso3" = "country")) %>% 
  drop_na(inferred_revenue, revenue) %>% 
  rename(country = eez_iso3) %>% 
  mutate(country = fct_relevel(country, "KIR"))

revenue_FFA_GFW <-
  ggplot(vessel_activity, aes(x = inferred_revenue, y = revenue)) +
  geom_point(aes(fill = country, size = days),
             shape = 21,
             # size = 3,
             alpha = 0.7) +
  geom_smooth(method = "lm",
              linetype = "dashed",
              color = "black",
              se = F) +
  geom_abline(intercept = 0, slope = 1) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  theme_cowplot()  +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = "Country", ncol = 1),
         size = guide_legend(title = "Vessel-days (1,000)", ncol = 3)) +
  labs(x = "Inferred revenue\n(million USD)",
       y = "Reported revenue\n(million USD)") +
  scale_size_continuous(breaks = c(1, 5, 10))

log_revenue_FFA_GFW <-
  ggplot(vessel_activity, aes(x = log10(inferred_revenue), y = log10(revenue))) +
  geom_point(aes(fill = country, size = days),
             shape = 21,
             # size = 3,
             alpha = 0.7) +
  geom_smooth(method = "lm",
              linetype = "dashed",
              color = "black",
              se = F) +
  geom_abline(intercept = 0, slope = 1) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  theme_cowplot()  +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  guides(fill = guide_legend(title = "Country", ncol = 1),
         size = guide_legend(title = "Vessel-days (1,000)", ncol = 3)) +
  labs(x = expression(atop(paste(paste("log" [10]), "(Inferred revenue)"), "(Million USD)")),
       y = expression(atop(paste(paste("log" [10]), "(Reported revenue)"), "(Million USD)"))) +
  scale_size_continuous(breaks = c(1, 5, 10), range = c(1, 4)) +
  guides(fill = F)

revenue_FFA_effort_GFW <-
  ggplot(vessel_activity, aes(x = days, y = revenue)) +
  geom_point(aes(fill = country, size = days),
             shape = 21,
             # size = 3,
             alpha = 0.7) +
  geom_smooth(method = "lm",
              linetype = "dashed",
              color = "black",
              se = F) +
  scale_fill_brewer(palette = "Set1", guide = F) +
  theme_cowplot()  +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(title = "Country", ncol = 1),
         size = guide_legend(title = "Vessel-days (1,000)", ncol = 3)) +
  labs(x = "Vessel-days (1,000)",
       y = "Reported revenue\n(million USD)") +
  scale_size_continuous(breaks = c(1, 5, 10))

# Put together
p <- plot_grid(p1,
               log_revenue_FFA_GFW,
               ncol = 1,
               labels = "AUTO")

# Save plot
ggsave(p,
       filename = here("docs", "img", "revenues.pdf"),
       width = 3.4,
       height = 5.25)

ggsave(revenue_FFA_GFW,
       filename = here("docs", "img", "revenue_FFA_GFW_linear.pdf"),
       width = 5.5,
       height = 3.4)

ggsave(revenue_FFA_effort_GFW,
       filename = here("docs", "img", "revenue_FFA_effort_GFW.pdf"),
       width = 5.5,
       height = 3.4)

# Annual revenues total PNA
annual_revenues <- drop_na(financial_data) %>%
  group_by(year) %>%
  summarize(revenue = sum(revenue, na.rm = T)) %>%
  ungroup()

p2 <- annual_revenues %>% 
  ggplot(aes(x = year, y = revenue)) +
  geom_point(shape = 21,
             color = "black",
             fill = "steelblue",
             size = 4,
             alpha = 0.8) + 
  geom_line(linetype = "dashed",
            size = 1) +
  labs(x = "Year",
       y = "Revenue from licenses\n(Million USD)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())


ggsave(plot = p2,
       filename = here("docs", "img", "total_PNA_revenues.pdf"),
       width = 6,
       height = 4)

# Plot for catches
p2 <- ggplot(data = financial_data,
             mapping = aes(x = year, y = catches/1000, fill = country)) +
  geom_line() +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  geom_vline(xintercept = 2015,
             linetype = "dashed") +
  theme_cowplot() +
  scale_fill_brewer(palette = "Set1") +
  guides(legend = guide_legend(title = "Country")) +
  labs(x = "Year",
       y = "Catches in EEZ\n(Thousand Tonnes)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.justification = c(0, 1),
        legend.position = c(0, 1.1)) +
  guides(fill = guide_legend(title = "Country", ncol = 2))

# Plot for value
p3 <- ggplot(data = financial_data,
             mapping = aes(x = year, y = value, fill = country)) +
  geom_line() +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  geom_vline(xintercept = 2015,
             linetype = "dashed") +
  theme_cowplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Value of catches in EEZ\n(Million USD)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Country", ncol = 2))

# Put together
p <- plot_grid(p2, p3, ncol = 1, labels = "AUTO")

# Save plot
ggsave(p, filename = here("docs", "img", "catches.pdf"),
       width = 3.4,
       height = 5.2)














