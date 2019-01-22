#############################
#   vessel_model_fitting    #
#############################

###################################################
# This is the script that fits all the models for
# my panels of behavioral change.
###################################################

# Load packages
library()
library(tidyverse)

# Custom functions
source(here::here("scripts", "did.R"))
source(here::here("scripts", "did_quarter.R"))
source(here::here("scripts", "did_yearmonth.R"))
source(here::here("scripts", "termplot.R"))

# Load my data
effort_by_vessel <- readRDS(file = here::here("data", "panels", "daily_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines")


model_base <- effort_by_vessel %>% 
  mutate(post = post * 1,
         treated = treated * 1,
         year2 = year * year,
         year_month_c = as.character(year_month),
         quarter_c = as.character(quarter),
         flag_fe = !flag %in% c("JPN", "MEX", "NIC", "NZL", "PHL", "USA", "KIR", "PNG"),
         flag_fe_oth = flag_fe & !flag == "OTH",
         sate = case_when(date < lubridate::date("2014/06/01") ~ "1",
                          date > lubridate::date("2015/12/31") ~ "3",
                          T ~ "2"))

## Controls are PNA
experiment1 <- model_base %>% 
  filter(experiment1)

## Controls are everything BUT Chinese
experiment2 <- model_base %>% 
  filter(experiment3)

## Controls are Taiwanese
experiment3 <- effort_by_vessel_JPN %>% 
  select(-PNA) %>% 
  mutate(post = post * 1,
         treated = treated * 1,
         year2 = year * year,
         year_month_c = as.character(year_month),
         quarter_c = as.character(quarter),
         flag_fe = F,
         flag_fe_oth = F,
         sate = case_when(date < lubridate::date("2014/06/01") ~ "1",
                          date > lubridate::date("2015/12/31") ~ "3",
                          T ~ "2"),
         baci_strict = T,
         experiment1 = F,
         experiment2 = F,
         experiment3 = F,
         flag = "JPN",
         baci_relaxed = T) %>% 
  rbind(filter(model_base, treated == 1))

## Fit the models



## Simple dids for each data
models_exp0_did <- model_base %>% did()
models_exp1_did <- experiment1 %>% did()
models_exp2_did <- experiment2 %>% did()
models_exp3_did <- experiment3 %>% did(flag_fe = F)

# Quarterly did
models_exp0_did_q <- model_base %>% did_quarter()
models_exp1_did_q <- experiment1 %>% did_quarter()
models_exp2_did_q <- experiment2 %>% did_quarter()
models_exp3_did_q <- experiment3 %>% did_quarter()

# Monthly did
models_exp0_did_ym <- model_base %>% did_yearmonth()
models_exp1_did_ym <- experiment1 %>% did_yearmonth()
models_exp2_did_ym <- experiment2 %>% did_yearmonth()
models_exp3_did_ym <- experiment3 %>% did_yearmonth()