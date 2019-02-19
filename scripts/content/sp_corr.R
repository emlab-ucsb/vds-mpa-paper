###############
#   sp_corr   #
###############

################################################################
# Generates some measures of crowding like the bumber of cells
# with BOTH treated and control vessels and the spatial correlation
# between treated and control for a given month.
# 
# Number of cells that had treated and control vessels (A) and
# spatial correlation in the presence-absence of treated and
# control vessels per cell (B).
################################################################

#### SET UP ######################################################################################

# Load packages
library(raster)
library(tidyverse)


## Load data

# Find files
files <- list.files(path = here::here("data", "spatial", "monthly_rasterized_effort_by_region"), pattern = "*.tif")

# Stack files
rs <- stack(here::here("data", "spatial", "monthly_rasterized_effort_by_region", files))


##### CALCULATIONS #########################################################################

# Put into a data.frame
corr <- raster::as.data.frame(rs, xy = T) %>%
  gather(group, hours, -c(x, y)) %>%
  mutate(year = as.numeric(
    str_extract(group, "[:digit:]+")),
    month = as.numeric(str_replace_all(str_extract(group, "\\.[:digit:]+\\."), "\\.", "")),
    group = str_extract(group, "[:alpha:]+"),
    hours = ifelse(hours > 0, 1, 0)) %>%
  spread(group, hours, fill = 0) %>% 
  mutate(both = (Control > 0 & Treated > 0) * 1,
         post = year > 2014,
         date = lubridate::date(paste(year, month, 01, sep = "-")))

# Calculate number of cells with both vessels
n_both <- corr %>%
  filter(both > 0) %>% 
  group_by(year, month, date, post) %>% 
  summarize(n = sum(both)) %>% 
  ungroup() %>% 
  mutate(dif = lubridate::interval(lubridate::date("2015-01-01"), date)%/% months(1))


# Calculate spatial correlation
corr_both <- corr %>%
  drop_na() %>%
  group_by(year, month, date, post) %>%
  summarize(cor = cor(Control, Treated)) %>% 
  ungroup() %>% 
  mutate(dif = lubridate::interval(lubridate::date("2015-01-01"), date)%/% months(1))



#### FIT THE MODELS ######################################################################

n_model <- lm(n ~ dif + I(dif^2) + I(dif^3) + I(dif^4), data = n_both)
corr_model <- lm(cor ~ dif + I(dif^2) + I(dif^3) + I(dif^4), data = corr_both)


models <- list(n_model, corr_model)

stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     t.auto = T,
                     p.auto = T,
                     single.row = T,
                     intercept.bottom = F,
                     covariate.labels = c("Constant", "Months", "Months $^2$", "Months $^3$", "Months $^4$"),
                     dep.var.caption = "",
                     dep.var.labels.include = F,
                     column.sep.width = "1pt",
                     font.size = "footnotesize",
                     type = "latex",
                     omit = c("flag", "month"),
                     add.lines = list(
                       c("Month FE", rep("Yes", 10)),
                       c("Flag FE", rep("Yes", 10))),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     # float.env = "sidewaystable",
                     title = "\\label{tab:main_DID}Coefficient estimates for a third-polinomial fit to the measures of crowding. The first column shows coefficients for the number of cells with treated and control vessels during the same month. The second column shows coefficients for the spatial correlation for presence / absence of treated and control vessels. The explanatory variable is the number of months before implementation of PIPA. Numbers in parentheses are heteroskedastic-robust standard errors.",
                     out = here::here("docs", "tab", "sp_corr.tex"))


#### PLOT THEM ##########################################################################

# Plot for number of cells with vessels from BOTH groups
n_both_plot <- ggplot(data = n_both, aes(x = date, y = n)) +
  geom_vline(xintercept = lubridate::date("2015-01-01"),
             linetype = "dashed", color = "black", size = 1) +
  geom_point(size = 4,
             alpha = 0.5,
             fill = "#377EB8",
             color = "black",
             shape = 21) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2) + I(x^3) + I(x^4),
              color = "black",
              se = F) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  labs(x = "Year", y = "Number of cells")

# Plot for correlations
corr_both_plot <- ggplot(data = corr_both, aes(x = date, y = cor)) +
  geom_vline(xintercept = lubridate::date("2015-01-01"),
             linetype = "dashed",
             color = "black",
             size = 1) +
  geom_point(size = 4,
             alpha = 0.5,
             fill = "#377EB8",
             color = "black",
             shape = 21) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2) + I(x^3) + I(x^4),
              color = "black",
              se = F) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  labs(x = "Year", y = "Pearson's correlation") + 
  scale_y_continuous(limits = c(0, 0.75))

# Export plot
plot <- cowplot::plot_grid(n_both_plot, corr_both_plot,
                           ncol = 1,
                           labels = "AUTO")

ggsave(plot, filename = here::here("docs", "img", "sp_corr.pdf"), height = 5.5, width = 3.4)


