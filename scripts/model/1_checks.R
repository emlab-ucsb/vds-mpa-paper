# Clean the environment
rm(list = ls())

# Load libraries
library(startR)
library(here)
library(cowplot)
library(tidyverse)

# Load functions
source(here("scripts", "model", "grow.R"))
source(here("scripts", "model", "E_vec_fxn.R"))
source(here("scripts", "model", "pi_of_E.R"))
source(here("scripts", "model", "max_Ei.R"))
source(here("scripts", "model", "solve_pi.R"))
source(here("scripts", "model", "PNA_with_trading.R"))
source(here("scripts", "model", "PNA_no_trading.R"))



## Parameters
# Set up spatial stuff
# Equal distribution across n patches
n_patches <- 10
fvec <- rep(1 / n_patches, n_patches)
# Arbitrary fvec
prop_hs <- 0.1
fvec <- c(rep(((1 - prop_hs) / (n_patches - 1)), (n_patches - 1)), prop_hs)
n_patches <- length(fvec) # Just as saftey

# Assign fvector based on historical effort distribution
# Create vector with VDS country iso3 codes
VDS_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV", "TKL")

# Load historical effort received and summarize
act <- readRDS(here("raw_data", "activity_by_vessel_year_eez.rds")) %>%
  filter(
    eez_iso3 %in% VDS_countries,
    best_vessel_class == "tuna_purse_seines"
  ) %>%
  group_by(year, eez_iso3) %>%
  summarize(h = sum(hours) / 24) %>%
  arrange(desc(h)) %>%
  mutate(
    h_prop = h / (sum(h) / (1 - prop_hs)),
    c_h_prop = cumsum(h_prop)
  )

catches <- read.csv(here("raw_data", "FFA", "ps_skj_catches_spatial_timeseries.csv"), stringsAsFactors = F) %>%
  gather(year, catches, -eez_iso3) %>%
  mutate(year = as.numeric(str_remove(year, "X"))) %>%
  filter(year > 2010) %>%
  mutate(
    PNA = eez_iso3 %in% VDS_countries,
    eez_iso3 = ifelse(PNA, eez_iso3, "HS")
  ) %>%
  group_by(year, eez_iso3) %>%
  summarize(catches = sum(catches))

catches_prop <- catches %>%
  group_by(eez_iso3) %>%
  summarize(catches = sum(catches)) %>%
  mutate(
    c_prop = catches / sum(catches),
    eez_iso3 = fct_relevel(eez_iso3, "HS", after = Inf),
    eez_iso3 = fct_relevel(eez_iso3, "KIR", after = 0)
  ) %>%
  arrange(eez_iso3)

fvec_c <- catches_prop$c_prop
names(fvec_c) <- levels(catches_prop$eez_iso3)

cpue <- left_join(act, catches, by = c("year", "eez_iso3")) %>%
  drop_na() %>%
  group_by(eez_iso3) %>%
  summarize(
    catches = sum(catches),
    hours = sum(h)
  ) %>%
  mutate(cpue = catches / hours) %>%
  arrange(desc(cpue)) %>%
  mutate(
    cpue_prop = cpue / (sum(cpue) / (1 - prop_hs)),
    cumsum_cpue_prop = cumsum(cpue_prop),
    eez_iso3 = fct_relevel(eez_iso3, "KIR")
  ) %>%
  arrange(eez_iso3)

fvec_cpue <- c(cpue$cpue_prop, prop_hs)
names(fvec_cpue) <- c(levels(cpue$eez_iso3), "HS")

# Reserve properties
theta <- 1
R <- 0

# Bio
MSY <- 1875600 # 50th percentile from MSY in table 8 of SA (https://www.wcpfc.int/node/27490)
Bmsy <- 1628000 # 50th percentile from SBmsy in table 8 of SA (https://www.wcpfc.int/node/27490)
K <- 6876526 # 50th percentile from SB_f0 in table 8 of SA (https://www.wcpfc.int/node/27490)
Bc_B0 <- 0.51 # 50th percentile from SBlatest/SB_fo in table 8 of SA (https://www.wcpfc.int/node/27490)
Cnow <- 1679444 # Catches now
Bnow <- K * Bc_B0 # current Biomass (2012 - 2015 average)
r <- 0.57 # From fishbase: Prior r = 0.57, 95% CL = 0.41 - 0.78 (https://www.fishbase.in/summary/107#)

# Economic
beta <- 1.3
p <- 1100 # (1447 + 1467) / 2 #mean between thailand and japan values (Value of WCPFC-CA tuna fisheries 2017 report)
E <- 45000
q <- 12 / (0.1 * Bnow) # 2 * (0.8 * Cnow) / (E * 0.8 * Bnow)
c <- 180 # 340

# Chose a source for the fvec
bvec <- fvec_cpue # From CPUE data
# fvec <- fvec_h # From effort data
# Initial biomass distribution should be based on CPUE
# Then, biomass distribution vector should be based on area by EEZ (10 % to HS)
Xvec <- bvec * Bnow
names(fvec) <- names(bvec)

# Is there a value of E for the given possible ranges of Biomass?
tibble(X = seq(1, Bnow, length.out = 100)) %>%
  rowwise() %>%
  mutate(E = sum(max_Ei(p = p, q = q, Xvec = X, theta = theta, R = R, beta = beta, c = c))) %>%
  ggplot(aes(x = X, y = E)) +
  geom_line() +
  geom_vline(xintercept = Bmsy) +
  geom_hline(yintercept = E)

# What's the aggregate demand for a given value of pi with Bmsy?
# FIrst we need to establish the maximum chargable price to plot all ranges from 0 to max_pi
max_pi <- max((p * q * Bnow * fvec))

check_E_pi <- tibble(pi = seq(0, max_pi, length.out = 100)) %>%
  rowwise() %>%
  mutate(E = list(
    tibble(
      E = E_vec_fxn(p = p, q = q, Xvec = Xvec, pi = pi, theta = theta, R = R, beta = beta, c = c),
      cty = names(fvec)
    )
  )) %>%
  unnest() %>%
  filter(!cty == "HS")

ggplot(data = check_E_pi, mapping = aes(x = E, y = pi, color = cty)) +
  geom_line(size = 1) +
  geom_vline(xintercept = E, color = "red")

check_E_pi %>%
  group_by(pi) %>%
  summarise(E = sum(E)) %>%
  ggplot(aes(x = E, y = pi)) +
  geom_line() +
  geom_vline(xintercept = E, color = "red") +
  geom_vline(xintercept = sum(max_Ei(p = p, q = q, Xvec = Xvec, theta = theta, R = R, beta = beta, c = c)[1:(n_patches - 1)]))