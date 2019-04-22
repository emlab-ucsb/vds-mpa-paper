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

# Benchmark results
Benchmark_results <-
  PNA_with_trading(
    fvec = fvec,
    theta = theta,
    R = R,
    r = r,
    K = K,
    p = p,
    q = q,
    Xvec = Xvec,
    beta = beta,
    c = c,
    E = E,
    save_plot = T
  ) %>%
  select(
    Country,
    b_harvest = Harvest,
    b_vessel_days = Vessel_Days,
    b_stock = Stock,
    b_stock_i = Stock_i,
    b_vdsprice = VDSprice,
    b_vdsrevenue = VDSrevenue,
    b_vdsrevenue_all = VDSrevenue_all,
    b_vdsrevenue_notKIR = VDSrevenue_notKIR
  )

Rvec <- seq(0, 0.8, 0.05)
thetavec <- seq(0, 1, 0.1)

DF_results <- tibble(
  Country = NA,
  Reserve = NA,
  Movement = NA,
  Harvest = NA,
  Vessel_Days = NA,
  Stock = NA,
  Stock_i = NA,
  VDSprice = NA,
  VDSrevenue = NA,
  VDSrevenue_all = NA,
  VDSrevenue_notKIR = NA
)

for (i in 1:length(Rvec)) {
  R_i <- Rvec[i]
  for (j in 1:length(thetavec)) {
    theta_j <- thetavec[j]
    
    E_m <- E # * theta + ((1 - theta)*(1-R))
    
    print(paste("Now running R =", R_i, "and theta = ", theta_j))
    
    DFtmp <-
      PNA_with_trading(
        fvec = fvec,
        theta = theta_j,
        R = R_i,
        r = r,
        K = K,
        p = p,
        q = q,
        Xvec = Xvec,
        beta = beta,
        c = c,
        E = E_m,
        save_plot = F
      )
    DF_results <- rbind(DF_results, DFtmp)
  }
}


DF_results <- DF_results %>%
  drop_na()


## NO TRADING
Evec <- Benchmark_results$b_vessel_days

Benchmark_results2 <-
  PNA_no_trading(
    fvec = fvec,
    theta = theta,
    R = R,
    r = r,
    K = K,
    p = p,
    q = q,
    Xvec = Xvec,
    beta = beta,
    c = c,
    Evec = Evec
  ) %>%
  select(
    Country,
    b_harvest = Harvest,
    b_vessel_days = Vessel_Days,
    b_stock = Stock,
    b_stock_i = Stock_i,
    b_vdsprice = VDSprice,
    b_vdsrevenue = VDSrevenue,
    b_vdsrevenue_all = VDSrevenue_all,
    b_vdsrevenue_notKIR = VDSrevenue_notKIR
  )


DF_results_no_trading <- tibble(
  Country = NA,
  Reserve = NA,
  Movement = NA,
  Harvest = NA,
  Vessel_Days = NA,
  Stock = NA,
  Stock_i = NA,
  VDSprice = NA,
  VDSrevenue = NA,
  VDSrevenue_all = NA,
  VDSrevenue_notKIR = NA
)

Rvec <- seq(0, 0.8, 0.05)
thetavec <- seq(0, 1, 0.1)

for (i in 1:length(Rvec)) {
  R_i <- Rvec[i]
  for (j in 1:length(thetavec)) {
    theta_j <- thetavec[j]
    
    E_m <- E # * theta + ((1 - theta)*(1-R))
    
    print(paste("Now running R =", R_i, "and theta = ", theta_j))
    
    DFtmp <-
      PNA_no_trading(
        fvec = fvec,
        theta = theta_j,
        R = R_i,
        r = r,
        K = K,
        p = p,
        q = q,
        Xvec = Xvec,
        beta = beta,
        c = c,
        Evec = Evec
      )
    DF_results_no_trading <- rbind(DF_results_no_trading, DFtmp)
  }
}

DF_results_no_trading <- DF_results_no_trading %>%
  drop_na()
