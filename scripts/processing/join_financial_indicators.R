##################################
#   join_financial_indicators    #
#################################

##################################################################
# This script produces combines financial indicators
# related to each PNA country. The data com from the
# FFA (https://www.ffa.int/node/2050). The PDF report is in the
# /reports folder.
##################################################################

####### SET UP #########################

# Load libraries
library(countrycode)
library(here)
library(tidyverse)

# List of PNA countries
PNA <- c("FSM",
         "KIR",
         "MHL",
         "NRU",
         "PLW",
         "PNG",
         "SLB",
         "TUV",
         "TKL")

####  PLOT VALUE AND CATCHES ####

# Load catch data
catches <- read_csv(here("raw_data", "FFA", "catches_by_eez.csv"),
                    col_types = cols()) %>% 
  gather(year, catches, -country) %>% 
  mutate(year = as.numeric(year))

# Load value data and join to catch data
catches_and_value <- read_csv(here("raw_data", "FFA", "value_of_catches_by_eez.csv"),
                              col_types = cols()) %>% 
  gather(year, value, -country) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(catches, by = c("country", "year")) %>% 
  select(year, country, catches, value) %>% 
  mutate(country = countrycode(sourcevar = country,
                               origin = "country.name",
                               destination = "iso3c"),
         country = fct_relevel(country, "KIR"),
         value = value / 1e6) %>% 
  filter(country %in% PNA)

licenses <- read_csv(here("raw_data", "FFA", "revenue_from_fees.csv"),
                     col_types = cols()) %>% 
  gather(year, revenue, -Country) %>% 
  rename(country = Country) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(country %in% PNA)

financial_data <- left_join(catches_and_value, licenses, by = c("country", "year"))

write.csv(financial_data, file = here("data", "financial_data.csv"))
