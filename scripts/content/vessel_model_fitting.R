#############################
#   vessel_model_fitting    #
#############################

###################################################
# This is the script that fits all the models for
# my panels of behavioral change.
###################################################

# Load packages
library(tidyverse)

# Custom functions
source(here::here("scripts", "functions", "model_data_prep.R"))
source(here::here("scripts", "functions", "did.R"))
source(here::here("scripts", "functions", "did_quarter.R"))
source(here::here("scripts", "functions", "did_yearmonth.R"))
source(here::here("scripts", "functions", "termplot.R"))

#### FISHING AND NON FISHING HOURS ################################################################
# Load my data
effort_by_vessel <- readRDS(file = here::here("data",
                                              "panels",
                                              "daily_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines")

## Prepare a base data
# For fishing hours
fishing_hours <- effort_by_vessel %>% 
  filter(fishing) %>% 
  model_data_prep()

# For non-fishing hours
nonfishing_hours <- effort_by_vessel %>% 
  filter(!fishing) %>% 
  model_data_prep()


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
prop_fishing_by_vessel <- readRDS(file = here::here("data",
                                              "panels",
                                              "daily_prop_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines") %>% 
  model_data_prep()

#Fit themdoels
prop_fish_did <- did(prop_fishing_by_vessel)
prop_fish_didq <- did_quarter(prop_fishing_by_vessel)
prop_fish_didym <- did_yearmonth(prop_fishing_by_vessel)




#### DISTANCE TRAVELED ################################################################
# Read data
distance_traveled <- readRDS(file = here::here("data",
                                               "panels",
                                               "daily_distance_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines",
         !mmsi %in% c(345050700,412328731,416238800, 512000089)) %>% 
  model_data_prep(dist)

# Fit the models
dist_did <- did(distance_traveled)
dist_didq <- did_quarter(distance_traveled)
dist_didym <- did_yearmonth(distance_traveled)





#### DISTANCE FROM PORT AND SHORE ############################################################
# Read the data
distance_port_shore <- readRDS(file = here::here("data",
                                                 "panels",
                                                 "distance_from_port_shore_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines")

# Prep data
# Distance from port
distance_port <- distance_port_shore %>% 
  model_data_prep(mean_dist_port)

# Distance from shore
distance_shore <- distance_port_shore %>% 
  model_data_prep(mean_dist_shore)

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
distance_port_shore_fishing <- readRDS(file = here::here("data",
                                                         "panels",
                                                         "distance_from_port_shore_fishing_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines")

# Prep data
# Distance from port
distance_port_fishing <- distance_port_shore_fishing %>% 
  model_data_prep(mean_dist_port)

# Distance from shore
distance_shore_fishing <- distance_port_shore_fishing %>% 
  model_data_prep(mean_dist_shore)

# Fit the models
# Distance from port
dist_port_fishing_did <- did(distance_port_fishing)
dist_port_fishing_didq <- did_quarter(distance_port_fishing)
dist_port_fishing_didym <- did_yearmonth(distance_port_fishing)

# Distance from shore
dist_port_fishing_did <- did(distance_shore_fishing)
dist_shore_fishing_didq <- did_quarter(distance_shore_fishing)
dist_shore_fishing_didym <- did_yearmonth(distance_shore_fishing)





##### PROPORTION FISHING IN KIR ########################################################
# Load the data
kir_fishing <- readRDS(file = here::here("data", "panels", "KIR_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-"))) %>% 
  model_data_prep(prop_hours)


# Fit the models
prop_kir_did <- did(kir_fishing)
prop_kir_didq <- did_quarter(kir_fishing)
prop_kir_didym <- did_yearmonth(kir_fishing)




##### PROPORTION FISHING IN VDS ########################################################
# Load the data
vds_fishing <- readRDS(file = here::here("data", "panels", "VDS_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-"))) %>% 
  model_data_prep(prop_hours)


# Fit the models
prop_vds_did <- did(vds_fishing)
prop_vds_didq <- did_quarter(vds_fishing)
prop_vds_didym <- did_yearmonth(vds_fishing)

######### EXPORT THE MODELS #############################################################

# Extract the largest specification of each basid did and send to stargazer
models <- list(
  fish_did,
  nonfish_did,
  prop_fish_did,
  dist_did,
  dist_port_did,
  dist_shore_did,
  dist_port_fishing_did,
  dist_port_fishing_did,
  prop_kir_did,
  prop_vds_did
) %>% 
  purrr::map(extract2, 1)

stargazer::stargazer(models,
                     se = commarobust::makerobustseslist(models),
                     t.auto = T,
                     p.auto = T,
                     intercept.bottom = F,
                     covariate.labels = c("Constant", "Post", "Treated", "Post $\\times$ Treated"),
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
                     title = "\\label{tab:did}Difference-in-differences estimates for our 10 variables of interest: 1) Daily fishing hours, 2) Daily non-fishing at-sea hours, 3) Daily proportion of fishing hours to total at-sea hours, 4) Daily distance traveled, 5) Daily mean distance from port, 6) Daily mean distance from shore, 7) Daily mean distance from port for fishing events, 8) Daily mean distance from shore for fishing events, 9) Monthly proportion of hours spent in Kiribati waters, 10) Monthly proportion of fishing hours spent in PNA waters. Numbers in parentheses are heteroskedastic-robust standard errors.",
                     out = here::here("docs", "tab", "main_DID.tex"))








