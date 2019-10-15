###################
#   KIR_sp_corr   #
###################

################################################################
# Generates some measures of crowding like the bumber of cells
# with BOTH treated and control vessels and the spatial correlation
# between treated and control for a given month.
# 
# Number of cells that had treated and control vessels (A) and
# spatial correlation in the presence-absence of treated and
# control vessels per cell (B).
################################################################

#### SET UP ######################################################

# Load packages
library(lubridate)
library(here)
library(raster)
library(tidyverse)

# Custom functions
# Extract AIC from the models
my_aic <- function(x){round(AIC(x), 3)}

## Load data

# Find files
files <- list.files(path = here("data",
                                "spatial",
                                "KIR_monthly_rasterized_effort_by_region"),
                    pattern = "*.tif")

# Stack files
rs <- stack(here("data", "spatial", "KIR_monthly_rasterized_effort_by_region", files))

# Nino4 index
nino4 <- read.csv(here("data", "all_indices.csv")) %>% 
  select(year, month = month_n, nino4anom)


##### CALCULATIONS ######################################################

# Put into a data.frame
corr <- raster::as.data.frame(rs, xy = T) %>%
  gather(group, hours, -c(x, y)) %>%
  mutate(year = as.numeric(
    str_extract(group, "[:digit:]+")),
    month = as.numeric(str_replace_all(str_extract(group, "\\.[:digit:]+\\."), "\\.", "")),
    group = str_extract(group, "[:alpha:]+"),
    hours = ifelse(hours > 0, 1, 0)) %>%
  spread(group, hours, fill = 0) %>% 
  mutate(both = (KIRControl > 0 & KIRTreated > 0) * 1,
         post = year > 2014,
         date = date(paste(year, month, 01, sep = "-")),
         sate1 = (date > date("2014-06-01")) * 1,
         sate2 = (date > date("2015-12-31")) * 1)

# Calculate number of cells with both vessels
n_both <- corr %>%
  filter(both > 0) %>%
  group_by(year, month, date, post, sate1, sate2) %>% 
  summarize(n = sum(both)) %>% 
  ungroup() %>% 
  complete(year, nesting(month), fill = list(n = 0, post = F, sate1 = 0, sate2 = 0)) %>%
  mutate(date = date(paste(year, month, 1, sep = "-")),
         dif = interval(date("2015-01-01"), date)%/% months(1)) %>% 
  left_join(nino4, by = c("year", "month"))


# Calculate spatial correlation
corr_both <- corr %>%
  drop_na() %>%
  group_by(year, month, date, post, sate1, sate2) %>%
  summarize(cor = cor(KIRControl, KIRTreated)) %>% 
  ungroup() %>% 
  mutate(dif = interval(date("2015-01-01"), date)%/% months(1)) %>% 
  left_join(nino4, by = c("year", "month")) %>% 
  replace_na(replace = list(cor = 0))



#### FIT THE MODELS ######################################################
n_model <- lm(n ~ nino4anom, data = n_both)
n_model1 <- lm(n ~ dif + I(dif^2) + I(dif^3) + I(dif^4), data = n_both)
n_model2 <- lm(n ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + nino4anom, data = n_both)
n_model3 <- lm(n ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + sate1 + sate2, data = n_both)
n_model4 <- lm(n ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + sate1 + sate2 + nino4anom, data = n_both)


corr_model <- lm(cor ~ nino4anom, data = corr_both)
corr_model1 <- lm(cor ~ dif + I(dif^2) + I(dif^3) + I(dif^4), data = corr_both)
corr_model2 <- lm(cor ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + nino4anom, data = corr_both)
corr_model3 <- lm(cor ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + sate1 + sate2, data = corr_both)
corr_model4 <- lm(cor ~ dif + I(dif^2) + I(dif^3) + I(dif^4) + sate1 + sate2 + nino4anom, data = corr_both)


models <- list(n_model1,
               n_model2,
               n_model3,
               n_model4,
               n_model,
               corr_model1,
               corr_model2,
               corr_model3,
               corr_model4,
               corr_model)

stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     t.auto = T,
                     p.auto = T,
                     digits = 2,
                     intercept.bottom = F,
                     covariate.labels = c("Constant",
                                          "M",
                                          "M $^2$",
                                          "M $^3$",
                                          "M $^4$",
                                          "NINO4",
                                          "$\\sigma_1$",
                                          "$\\sigma_2$"),
                     dep.var.caption = "",
                     dep.var.labels = c("Number of cells",
                                        "Pearson's correlation coefficient"),
                     column.sep.width = "0.1pt",
                     font.size = "footnotesize",
                     type = "latex",
                     omit = c("flag", "month"),
                     add.lines = list(
                       c("NINO4", rep(c("No", "Yes", "No", "Yes", "Yes"), 2)),
                       c("Satellites", c("No", "No", "Yes", "Yes", "No")),
                       c("AIC", map_dbl(models, my_aic))),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     title = "\\label{tab:KIR_sp_corr}Coefficient estimates for a fourth-degree polynomial fit to the measures of crowding for Kiribati EEZ only. The first five columns represent different specifications for number of cells with presence of both fleets. Columns 6 - 10 show coefficients for the spatial correlation for presence / absence of displaced and non-displaced vessels. The explanatory variable is the number of months before or after implementation of PIPA. Numbers in parentheses are heteroskedastic-robust standard errors. The last column of each group presents fits with only NINO4 anomaly index as an explanatory variable.",
                     out = here("docs", "tab", "KIR_sp_corr.tex"))


#### PLOT THEM #########################################################

# Create DF with predictions
# n_both_pred <- n_both %>% 
#   mutate(predicted1 = predict(n_model1),
#          predicted2 = predict(n_model2),
#          predicted3 = predict(n_model3),
#          predicted4 = predict(n_model4)) %>% 
#   select(date, predicted1, predicted2, predicted3, predicted4) %>% 
#   gather(model, prediction, -date) %>% 
#   mutate(groups = group_indices(., model)) %>% 
#   left_join(tibble(groups = c(1:4),
#                    model_2 = c("Month",
#                                "Month + nino4",
#                                "Month + satellites",
#                                "Month + satellites + nino4")),
#             by = "groups")
# 
# corr_both_pred <- corr_both %>% 
#   mutate(predicted1 = predict(corr_model1),
#          predicted2 = predict(corr_model2),
#          predicted3 = predict(corr_model3),
#          predicted4 = predict(corr_model4)) %>% 
#   select(date, predicted1, predicted2, predicted3, predicted4) %>% 
#   gather(model, prediction, -date) %>% 
#   mutate(groups = group_indices(., model)) %>% 
#   left_join(tibble(groups = c(1:4), model_2 = c("Month", "Month + satellites", "Month + satellites + nino4", "Month + nino4")), by = "groups")

# Plot for number of cells with vessels from BOTH groups
n_both_plot <- ggplot(data = n_both) +
  geom_vline(xintercept = date("2015-01-01"),
             linetype = "dashed", color = "black", size = 1) +
  geom_point(aes(x = date, y = n, fill = nino4anom),
             size = 4,
             alpha = 0.5,
             color = "black",
             shape = 21) +
  geom_line(aes(x = date, y = predict(n_model1)),
            size = 1) +
  cowplot::theme_cowplot() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.justification = c(0,1),
        legend.position = c(0, 1.08)) +
  guides(fill = guide_colorbar(title = "NINO4 Anomally",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  labs(x = "Year", y = "Number of cells") +
  scale_fill_gradientn(colors = colorRamps::matlab.like(10))

# Plot for correlations
corr_both_plot <- ggplot(data = corr_both) +
  geom_vline(xintercept = date("2015-01-01"),
             linetype = "dashed",
             color = "black",
             size = 1) +
  geom_point(aes(x = date, y = cor, fill = nino4anom),
             size = 4,
             alpha = 0.5,
             color = "black",
             shape = 21) +
  geom_line(aes(x = date, y = predict(corr_model1)),
            size = 1) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  labs(x = "Year", y = "Pearson's correlation") + 
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_fill_gradientn(colors = colorRamps::matlab.like(10))

# Export plot
plot <- cowplot::plot_grid(n_both_plot, corr_both_plot,
                           ncol = 1,
                           labels = "AUTO")

ggsave(plot,
       filename = here("docs", "img", "KIR_sp_corr.pdf"),
       height = 5.5,
       width = 3.4)


