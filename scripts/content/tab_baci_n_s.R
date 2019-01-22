#################
#   baci_n_s    #
#################

#######################################################
# Number of fishing vessels and mean daily fishing
# hours by group before and after PIPA implementation.
#######################################################


# Load packages
library(startR)
library(tidyverse)



# Source local functions
source(here::here("scripts", "my_kable.R"))


effort_by_vessel <- readRDS(file = here::here("data", "panels", "daily_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2018,
         gear == "tuna_purse_seines")

hours <- effort_by_vessel %>%
  group_by(treated, post, date, mmsi) %>% 
  summarize(hours = sum(hours, na.rm  = T)) %>% 
  ungroup() %>% 
  group_by(treated, post) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  spread(post, hours) %>% 
  mutate(change = `TRUE` / `FALSE`)

effort_by_vessel %>% 
  group_by(treated, mmsi) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(treated) %>% 
  summarize(boats = n()) %>% 
  ungroup() %>% 
  left_join(hours, by = c( "treated")) %>% 
  mutate(treated = ifelse(treated, "Treatment", "Control")) %>% 
  knitr::kable(col.names = c("Group", "n" ,"Before", "After", "Change (A / B)"),
               caption = "\\label{tab:baci_n_s}Number of fishing vessels and mean daily fishing hours by group before and after PIPA implementation.",
               # label = "baci_n_s",
               booktabs = T,
               digits = 2,
               format = "latex") %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position") %>% 
  cat(file = here::here("docs", "tab", "baci_n_s.tex"))
