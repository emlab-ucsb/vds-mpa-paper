#######################################
#     vessel_tracks_2012_2018_baci    #
#######################################

##################################################################################
# The objective of this script is to clean and formalize the
# vessel_tracks_2012_2018 data.
#
# That data was obtained from Google BigQuery, and I need to adjust it to ensure
# that the panel data conforms to a proper BACI design. I also need to keep only
# the fishing vessels we care about (purse seiners and longliners), so I will
# subset by those gears. I then create a number of indicator variables that I can
# use to further subset the data down the line. These include different definitions
# of control and treatment groups, as well as additional counterfactuals. I then
# export the dataset to use as main input for when I want to calculate other
##################################################################################

# Load packages
library(here) 
library(tidyverse)
library(magrittr)

# Set a large memory size for when this code runs on Windows
if(unname(Sys.info()[1] == "Windows")){
  memory.limit(size = 8e6)
}

# Read the tracks
vessel_tracks <- readRDS(file = here("raw_data", "vessel_tracks.rds")) %>% 
  mutate(fishing = ifelse(is.na(fishing), FALSE, fishing)) # Values of nnet_score are NULL imply fishing)

# Identify vessels suitable for BACI
# I want to make sure my control does not include vessels that appear after 2015
# or that my treatment has vessels that disappeared after 2015. We use Sept 1 as
# a way to break the treatment / control to account for the BlueParadox described
# by McDermott et al 2018 PNAS

# This gets "treated" vessels before
tb <- vessel_tracks %>% 
  filter(fishing, treated, !post, date < lubridate::date("2014/09/01")) %$%
  mmsi %>%
  unique()

# This gets "treated" vessels after
ta <- vessel_tracks %>% 
  filter(fishing, treated, post) %$%
  mmsi %>%
  unique()

# This gets "control" vessels after
cb <- vessel_tracks %>% 
  filter(fishing, !treated, !post, date < lubridate::date("2014/09/01")) %$%
  mmsi %>%
  unique()

# This gets "control" vessels after
ca <- vessel_tracks %>% 
  filter(fishing, !treated, post) %$%
  mmsi %>%
  unique()

# I have two measures of BACI

## BACI STRICT
#Keep in TA only vessels that were there before AND after
#Keep in CA only vessels that were before AND after
mmsi_baci_strict <- c(ta[ta %in% tb],
                      ca[ca %in% cb])
# BACI RELAXED
# This is a more flexible one just in case, but I use the
# strict one in the analysis. The "treated" are only the ones before
# and it doesn't matter if they disappear afterwards. I don't like
# this one because if vessels leave the fishery then I have trouble
# estimating the effects later (i.e. vessels that only appear in part of the panel)
mmsi_baci_relaxed <- c(tb, ca[ca %in% cb])

# I now create some new variables that I use.
# These are the experiment# variables, which include
# other options for plausible counterfactuals:
# - experiment1 uses only PNA-owned vessels
# - experiment2 is all VDS-owned vessels
# - experiment3 excludes Chinese vessels from the control
vessel_tracks_baci <- vessel_tracks %>%
  mutate(flag = ifelse(flag == "" | is.na(flag), "OTH", flag),
         baci_strict = mmsi %in% mmsi_baci_strict,
         baci_relaxed = mmsi %in% mmsi_baci_relaxed)

# Save data
saveRDS(vessel_tracks_baci,
        file = here("data", "vessel_tracks_2012_2018_baci.rds"))

