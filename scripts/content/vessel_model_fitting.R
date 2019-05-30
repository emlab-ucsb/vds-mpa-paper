#############################
#   vessel_model_fitting    #
#############################

###################################################
# This is the script that fits all the models for
# my panels of behavioral change.
###################################################

# Load packages
library(here)
library(magrittr)
library(tidyverse)

# Custom functions
source(here("scripts", "functions", "model_data_prep.R"))
source(here("scripts", "functions", "did.R"))
source(here("scripts", "functions", "did_quarter.R"))
source(here("scripts", "functions", "did_yearmonth.R"))
source(here("scripts", "functions", "termplot.R"))

# Nino4 index
nino4 <- read.csv(here("data", "all_indices.csv")) %>% 
  select(year, month = month_n, nino4anom)

#### FISHING AND NON FISHING HOURS ################################################################
# Load my data
effort_by_vessel <- readRDS(file = here("data", "panels", "daily_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines")

## Prepare a base data
# For fishing hours
fishing_hours <- effort_by_vessel %>% 
  filter(fishing) %>% 
  model_data_prep() %>% 
  left_join(nino4, by = c("year", "month"))

# For non-fishing hours
nonfishing_hours <- effort_by_vessel %>% 
  filter(!fishing) %>% 
  model_data_prep() %>% 
  left_join(nino4, by = c("year", "month"))


## Fit the models
# For each set of data I fit simple did, quarterly and year-month
# Fishing hours
fish_did <- did(fishing_hours)
fish_didq <- did_quarter(fishing_hours)
fish_didym <- did_yearmonth(fishing_hours)

#Non-fishing hours
nonfish_did <- did(nonfishing_hours)
nonfish_didq <- did_quarter(nonfishing_hours)
nonfish_didym <- did_yearmonth(nonfishing_hours)




#### PROPORTION FISHING HOURS ################################################################
# Load my data
prop_fishing_by_vessel <- readRDS(file = here("data",
                                              "panels",
                                              "daily_prop_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  model_data_prep(prop_fishing) %>% 
  left_join(nino4, by = c("year", "month"))

#Fit themdoels
prop_fish_did <- did(prop_fishing_by_vessel)
prop_fish_didq <- did_quarter(prop_fishing_by_vessel)
prop_fish_didym <- did_yearmonth(prop_fishing_by_vessel)




#### DISTANCE TRAVELED ################################################################
# Read data
distance_traveled <- readRDS(file = here("data",
                                               "panels",
                                               "daily_distance_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines",
         dist < quantile(dist, probs = 0.99)) %>%  #Need to figure out this vessels here and in the fig_all_panels script
  model_data_prep(dist) %>% 
  left_join(nino4, by = c("year", "month"))

# Fit the models
dist_did <- did(distance_traveled)
dist_didq <- did_quarter(distance_traveled)
dist_didym <- did_yearmonth(distance_traveled)





#### DISTANCE FROM PORT AND SHORE ############################################################
# Read the data
distance_port_shore <- readRDS(file = here("data",
                                                 "panels",
                                                 "distance_from_port_shore_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines")

# Prep data
# Distance from port
distance_port <- distance_port_shore %>% 
  model_data_prep(mean_dist_port) %>% 
  left_join(nino4, by = c("year", "month"))

# Distance from shore
distance_shore <- distance_port_shore %>% 
  model_data_prep(mean_dist_shore) %>% 
  left_join(nino4, by = c("year", "month"))

# Fit the models
# Distance from port
dist_port_did <- did(distance_port)
dist_port_didq <- did_quarter(distance_port)
dist_port_didym <- did_yearmonth(distance_port)

# Distance from shore
dist_shore_did <- did(distance_shore)
dist_shore_didq <- did_quarter(distance_shore)
dist_shore_didym <- did_yearmonth(distance_shore)





#### DISTANCE FROM PORT AND SHORE FOR FISHING EVENTS ONLY ###########################################
# Read the data
distance_port_shore_fishing <- readRDS(file = here("data",
                                                         "panels",
                                                         "distance_from_port_shore_fishing_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines")

# Prep data
# Distance from port
distance_port_fishing <- distance_port_shore_fishing %>% 
  model_data_prep(mean_dist_port) %>% 
  left_join(nino4, by = c("year", "month"))

# Distance from shore
distance_shore_fishing <- distance_port_shore_fishing %>% 
  model_data_prep(mean_dist_shore) %>% 
  left_join(nino4, by = c("year", "month"))

# Fit the models
# Distance from port
dist_port_fishing_did <- did(distance_port_fishing)
dist_port_fishing_didq <- did_quarter(distance_port_fishing)
dist_port_fishing_didym <- did_yearmonth(distance_port_fishing)

# Distance from shore
dist_shore_fishing_did <- did(distance_shore_fishing)
dist_shore_fishing_didq <- did_quarter(distance_shore_fishing)
dist_shore_fishing_didym <- did_yearmonth(distance_shore_fishing)





##### PROPORTION FISHING IN KIR ########################################################
# Load the data
kir_fishing <- readRDS(file = here("data", "panels", "KIR_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-"))) %>% 
  model_data_prep(kir_hours) %>% 
  left_join(nino4, by = c("year", "month"))


# Fit the models
prop_kir_did <- did(kir_fishing)
prop_kir_didq <- did_quarter(kir_fishing)
prop_kir_didym <- did_yearmonth(kir_fishing)




##### PROPORTION FISHING IN VDS ########################################################
# Load the data
vds_fishing <- readRDS(file = here("data", "panels", "VDS_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-"))) %>% 
  model_data_prep(vds_hours) %>% 
  left_join(nino4, by = c("year", "month"))


# Fit the models
prop_vds_did <- did(vds_fishing)
prop_vds_didq <- did_quarter(vds_fishing)
prop_vds_didym <- did_yearmonth(vds_fishing)





##### FISHING IN HS ########################################################
# Load the data
hs_fishing <- readRDS(file = here("data", "panels", "HS_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-"))) %>% 
  model_data_prep(hs_hours) %>% 
  left_join(nino4, by = c("year", "month"))


# Fit the models
hs_did <- did(hs_fishing)
hs_didq <- did_quarter(hs_fishing)
hs_didym <- did_yearmonth(hs_fishing)

######### EXPORT THE MODELS #############################################################

# Extract the largest specification of each basid did and send to stargazer
models <- list(
  fish_did,
  nonfish_did,
  prop_fish_did,
  dist_did,
  # dist_port_did,
  # dist_shore_did,
  dist_port_fishing_did,
  dist_shore_fishing_did,
  prop_kir_did,
  prop_vds_did,
  hs_did) %>% 
  purrr::map(extract2, 4)

stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     t.auto = T,
                     p.auto = T,
                     intercept.bottom = F,
                     covariate.labels = c("Constant",
                                          "Post",
                                          "Displaced",
                                          "NINO4",
                                          "Post $\\times$ Displaced"),
                     dep.var.caption = "",
                     dep.var.labels.include = F,
                     column.sep.width = "1pt",
                     font.size = "footnotesize",
                     type = "latex",
                     omit = c("flag", "month"),
                     add.lines = list(
                       c("Month FE", rep("Yes", 9)),
                       c("Flag FE", rep("Yes", 9))),
                     omit.stat = c("adj.rsq", "f", "ser"),
                     header = F,
                     # float.env = "sidewaystable",
                     title = "\\label{tab:main_DID}Difference-in-differences estimates for our 9 variables of interest: 1) Daily fishing hours, 2) Daily non-fishing at-sea hours, 3) Daily proportion of fishing hours to total at-sea hours, 4) Daily distance traveled, 5) Daily mean distance from port for fishing events, 6) Daily mean distance from shore for fishing events, 7) Monthly fishing hours spent in Kiribati waters, 8) Monthly fishing hours spent in PNA waters, and 9) Monthly fishing hours in the high seas. Numbers in parentheses are heteroskedastic-robust standard errors.",
                     out = here("docs", "tab", "main_DID.tex"))

######## ALTERNATIVE SPECIFICATIONS PLOT ###################################################

models_extra <- c(
  fish_did,
  nonfish_did,
  prop_fish_did,
  dist_did,
  # dist_port_did,
  # dist_shore_did,
  dist_port_fishing_did,
  dist_shore_fishing_did,
  prop_kir_did,
  prop_vds_did,
  hs_did)

# Extract all parameters, calculate robust SE, and plot
plot <- map_df(models_extra,
       commarobust::commarobust_tidy, .id = "model") %>%
  filter(term == "post:treated") %>%
  mutate(model = as.numeric(model),
         variable = case_when(model < 5 ~ "fishing hours",
                              model > 4 & model < 9 ~ "non-fishing hours",
                              model > 8 & model < 13 ~ "proportion fishing",
                              model > 12 & model < 17 ~ "distance traveled",
                              model > 16 & model < 21 ~ "distance from port fishing",
                              model > 20 & model < 25 ~ "distance from shore fishing",
                              model > 24 & model < 29 ~ "hours fishing in KIR",
                              model > 28 & model < 33 ~ "hours fishing in PNA",
                              T ~ "hours fishing in HS"),
         variable = fct_relevel(variable,
                                "fishing hours",
                                "non-fishing hours",
                                "proportion fishing",
                                "distance traveled",
                                "distance from port fishing",
                                "distance from shore fishing",
                                "hours fishing in KIR",
                                "hours fishing in PNA"),
         spec = case_when(model %in% seq(1, 36, by = 4) ~ "No FE",
                          model %in% seq(2, 36, by = 4) ~ "Month FE",
                          model %in% seq(3, 36, by = 4) ~ "Month + Flag FE",
                          T ~ "Month + Flag FE + NINO4"),
         spec = fct_relevel(spec, "No FE", "Month FE", "Month + Flag FE")) %>% 
  ggplot(aes(x = spec, y = est)) +
  geom_errorbar(aes(ymin = est - se, ymax = est + se),
                size = 1,
                width = 0,
                color = "black") +
  geom_point(aes(color = spec), size = 3, alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        text = element_text(size = 10),
        axis.text = element_text(size = 10)) +
  labs(x = "Model Specification", y = "Estimate") +
  ggExtra::rotateTextX()

# Export figure
ggsave(plot,
       filename = here("docs", "img", "other_specifications.pdf"),
       width = 6.1,
       height = 8)






