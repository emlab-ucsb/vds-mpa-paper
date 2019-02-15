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
  geom_line() +
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
        legend.position = "none")

# Plot for catches
p2 <- ggplot(data = financial_data,
             mapping = aes(x = year, y = catches/1000, fill = country)) +
  geom_line() +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  # facet_wrap(~country, scales = "free_y") +
  geom_vline(xintercept = 2015,
             linetype = "dashed") +
  theme_cowplot() +
  scale_fill_brewer(palette = "Set1") +
  guides(legend = guide_legend(title = "Country")) +
  labs(x = "Year",
       y = "Catches in EEZ\n(Thousand Tonnes)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1.15)) +
  guides(fill = guide_legend(title = "Country", ncol = 2))

# Plot for value
p3 <- ggplot(data = financial_data,
             mapping = aes(x = year, y = value, fill = country)) +
  geom_line() +
  geom_point(shape = 21,
             color = "black",
             size = 2) +
  # facet_wrap(~country, scales = "free_y") +
  geom_vline(xintercept = 2015,
             linetype = "dashed") +
  theme_cowplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = "Value of catches in EEZ\n(Million USD)") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  guides(fill = guide_legend(title = "Country", ncol = 2))

# Put together
p <- plot_grid(p1, p2, ncol = 1, labels = "AUTO")

# Save plot
ggsave(p, filename = here::here("docs", "img", "financial_indicators.pdf"),
       width = 3.4,
       height = 5.2)
















