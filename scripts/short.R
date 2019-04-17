# Clean the environment
rm(list = ls())

# Load libraries
library(startR)
library(here)
library(cowplot)
library(tidyverse)

## List of functions

# Growth function
# This follows discrete logistic growth rate
grow <- function(e, r, K) {
  out <- e * exp(r * (1 - (e/K)))
  return(out)
}

# Effort allocation
# Given parameters (pi and X the most important ones), this function calculates
# the effort for each patch. If efforts were to be negative, a zero is returned instead.
E_vec_fxn <- function(p = NULL, q = NULL, Xvec = NULL, theta = NULL, R = NULL , pi = NULL, beta = NULL, c = NULL) {
  n_patches <- length(Xvec)
  
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta)*(1-R))
  
  pi <- c(rep(pi, n_patches - 1), 0)
  
  E_vec <- (((p * q * Xvec * omega) - pi)/(beta * c)) ^ (1 / (beta - 1))
  
  threshold <- p * q * Xvec* omega
  
  # Replace values that are above the threashold with zeros
  E_vec[threshold < pi] <- 0
  
  return(E_vec)
}

# Maximum effort
# This function calculates the MAXIMUM effort that could be attained by a given stock size. The value
# of Ebar (target level of effort) must always be bellow this point
max_Ei <- function(p = NULL, q = NULL, Xvec = NULL, theta = NULL, R = NULL, beta = NULL, c = NULL) {
  n_patches <- length(Xvec)
  
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta)*(1-R))
  
  E <- ((p * q * Xvec * omega) / (beta * c)) ^ (1 / (beta - 1))
  
  return(E)
}

pi_of_E <- function(p = NULL, q = NULL, X = NULL, theta = NULL, R = NULL, beta = NULL, c = NULL, E = NULL) {
  n_patches <- length(X)
  
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta)*(1-R))
  
  pi <- (p*q*X*omega) - (beta * c * ((E) ^ (beta - 1)))
  pi[n_patches] <- 0 #Price in the high seas is 0
  pi <- pmax(pi, 0) #Prices cannot be negative
  
  return(pi)
  
}


# Solve for pi
# This is the objective function used to find the equimarginal principle
# It is used to test (via optim) many different values of pi such that the
# resulting sum(Ei) = Ebar.
solve_pi <- function(par, others) {
  pi <- par[1]
  
  E <- others[[1]]
  p <- others[[2]]
  q <- others[[3]]
  Xvec <- others[[4]]
  theta <- others[[5]]
  R <- others[[6]]
  beta <- others[[7]]
  c <- others[[8]]
  
  n_patches <- length(Xvec)
  
  E_vec <- E_vec_fxn(p = p, q = q, Xvec = Xvec, theta = theta, R = R, pi = pi, beta = beta, c = c)
  
  E_calc <- sum(E_vec[1:(n_patches-1)])
  
  err <- (E - E_calc) ^ 2
  # print(paste("error was", err))
  
  # if(any(E_vec < 0) | any(is.nan(E_vec))){err <- 1e6}
  
  return(err)
}

### START OPTIMIZATION
# Set up initial conditions that make sense

Sim <- function(fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, Xvec = NULL, beta = NULL, c = NULL, E = NULL, save_plot = F) {
  
  n_patches <- length(fvec)
  
  prog <- tibble(
    ct = 1,
    X = sum(Xvec),
    E = 0,
    pi = 0,
    H = 0,
    e = 0,
    Xnew = 0,
    diff = 0,
    err = 0
  )
  
  tol <- 0.001 * Bnow
  diff <- (tol*2)^2
  
  ct <- 1
  
  while (diff > tol) {
    
    # OPTIMIZATION FOR PI
    ## set up
    others <- list("E" = E,
                   "p" = p,
                   "q" = q,
                   "Xvec" = Xvec,
                   "theta" = theta,
                   "R" = R,
                   "beta" = beta,
                   "c" = c)
    
    ## run optin to get pi for given Xvec
    opt <- nlminb(start = 1000,
                  objective = solve_pi,
                  others = others,
                  lower = 0,
                  upper = 150000,
                  control = list(
                    iter.max = 1e6,
                    abs.tol = 10,
                    step.min = 10,
                    step.max = 1000
                  )
    )
    ## extract pi
    pi <- opt$par[1]
    
    # plot <- plot +
    #   geom_hline(yintercept = pi)
    
    
    # CALCULATE HARVESST AND BIOMASS
    ## total biomass before harvesting
    Xnow <- sum(Xvec)
    
    ## Calculate the Ei with the optimized pi
    Evec <- E_vec_fxn(p = p, q = q, Xvec = Xvec, pi = pi, theta = theta, R = R, beta = beta, c = c)
    
    ## Calculate harvest vector
    Hvec <- (q * Evec * Xvec)
    Hvec <- pmin(Hvec, Xvec)
    
    evec <- Xvec - Hvec
    e_all <- sum(evec)
    Xnew <- grow(e = e_all, r = r, K = K)
    
    diff <- (Xnow - Xnew) ^ 2
    
    # Print every 50 iterations to check what's going on
    if((ct %% 50) == 0){
      print(paste("difference is", sqrt(diff)))
    }
    
    
    # Update counter
    ct <- ct + 1
    
    prog_i <- tibble(
      ct = ct,
      X = sum(Xvec),
      E = sum(Evec[1 : (n_patches -1)]),
      pi = pi,
      H = sum(Hvec),
      e = e_all,
      Xnew = grow(e = e_all, r = r, K = K),
      diff = diff,
      err = opt$objective
    )
    
    prog <- rbind(prog, prog_i)
    
    Xvec <- fvec * Xnew
    
    # Check pi
    # print(pi)
  }
  
  print(sqrt(opt$objective))
  
  if(save_plot) {
    my_plot <- drop_na(prog, ct) %>%
      select(ct, pi, X, E, H, e, Xnew, diff, err) %>%
      gather(variable, value, -ct) %>%
      ggplot(aes(x = ct, y = value)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      ggtitle(label = paste("R = ", R, "theta = ", theta)) +
      startR::ggtheme_plot()
    
    ggsave(my_plot,
           filename = here::here("scripts", "model", "conv_img",
                                 paste("R = ", R, "theta = ", theta, ".png")))
  }
  
  X_ss_vec <- Xvec
  E_ss_vec <- Evec
  H_ss_vec <- Hvec
  # When high seas are present
  # VDSprice_ss_vec <- c(rep(pi, length(Xvec) - 1), 0)
  VDSprice_ss_vec <- c(rep(pi, n_patches - 1), 0)
  
  VDSrevenue_ss_vec <- VDSprice_ss_vec * Evec
  
  DF <- data.frame(Country = LETTERS[1:n_patches],
                   Reserve = R,
                   Movement = theta,
                   Harvest = H_ss_vec,
                   Vessel_Days = E_ss_vec,
                   Stock = sum(X_ss_vec),
                   Stock_i = X_ss_vec,
                   VDSprice = VDSprice_ss_vec,
                   VDSrevenue = VDSrevenue_ss_vec,
                   VDSrevenue_all = sum(VDSrevenue_ss_vec),
                   VDSrevenue_notKIR = sum(VDSrevenue_ss_vec[2:n_patches]))
  
  return(DF)
}


## Parameters
# Set up spatial stuff
# Equal distribution across n patches
n_patches <- 10
fvec <- rep(1/n_patches, n_patches)
# Arbitrary fvec
prop_hs <- 0.1
fvec <- c(rep(((1 - prop_hs) / (n_patches - 1)), (n_patches - 1)), prop_hs)
n_patches <- length(fvec) #Just as saftey

# Assign fvector based on historical effort distribution
# Create vector with VDS country iso3 codes
PNA_countries <- c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")

# Load historical effort received and summarize
act <- readRDS(here("raw_data", "activity_by_vessel_year_eez.rds")) %>% 
  filter(eez_iso3 %in% VDS_countries,
         best_vessel_class == "tuna_purse_seines") %>%
  group_by(year, eez_iso3) %>%
  summarize(h = sum(hours) / 24) %>%
  arrange(desc(h)) %>%
  mutate(h_prop = h / (sum(h) / (1 - prop_hs)),
         c_h_prop = cumsum(h_prop))

fvec_h <- c(act$h_prop, prop_hs)

catches <- read.csv(here("raw_data", "FFA", "ps_skj_catches_spatial_timeseries.csv"), stringsAsFactors = F) %>%
  gather(year, catches, -eez_iso3) %>%
  mutate(year = as.numeric(str_remove(year, "X"))) %>%
  filter(year > 2010) %>% 
  mutate(PNA = eez_iso3 %in% VDS_countries,
         eez_iso3 = ifelse(PNA, eez_iso3, "HS")) %>%
  group_by(year, eez_iso3) %>%
  summarize(catches = sum(catches)) %>% 
  arrange(desc(catches)) %>%
  mutate(catches_prop = catches / sum(catches),
         c_catches_prop = cumsum(catches_prop))

fvec_c <- c(catches$catches_prop[-2], catches$catches_prop[2])

cpue <- left_join(act, catches, by = c("year", "eez_iso3")) %>% 
  mutate(cpue = catches / h) %>%
  group_by(eez_iso3) %>%
  summarize(cpue_mean = mean(cpue, na.rm = T)) %>% 
  arrange(desc(cpue_mean)) %>%
  mutate(cpue_prop = cpue_mean / (sum(cpue_mean) / 0.9),
         cumsum_cpue_prop = cumsum(cpue_prop),
         eez_iso3 = fct_relevel(eez_iso3, "KIR")) %>% 
  arrange(eez_iso3)

fvec_cpue <- c(cpue$cpue_prop, 0.1)

# Reserve properties
theta <- 1
R <- 0

# Bio
MSY <- 1875600 #50th percentile from MSY in table 8 of SA (https://www.wcpfc.int/node/27490)
Bmsy <- 1628000 #50th percentile from SBmsy in table 8 of SA (https://www.wcpfc.int/node/27490)
K <- 6876526 #50th percentile from SB_f0 in table 8 of SA (https://www.wcpfc.int/node/27490)
Bc_B0 <- 0.51 #50th percentile from SBlatest/SB_fo in table 8 of SA (https://www.wcpfc.int/node/27490)
Cnow <- 1679444 #Catches now
Bnow <- K * Bc_B0 #current Biomass (2012 - 2015 average)
r <- 0.57 # From fishbase: Prior r = 0.57, 95% CL = 0.41 - 0.78 (https://www.fishbase.in/summary/107#)

# Economic
beta <- 1.3
p <- 1100# (1447 + 1467) / 2 #mean between thailand and japan values (Value of WCPFC-CA tuna fisheries 2017 report)
E <- 45000
q <- 12 / (0.1 * Bnow) #2 * (0.8 * Cnow) / (E * 0.8 * Bnow)
c <- 180# 340

# Chose a source for the fvec
fvec <- fvec_cpue # From CPUE data
# fvec <- fvec_h # From effort data
Xvec <- fvec * Bnow

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
      cty = letters[1:n_patches]
    )
  )) %>% 
  unnest() %>% 
  filter(!cty == letters[n_patches])

ggplot(data = check_E_pi, mapping = aes(x = E, y = pi, color = cty)) +
  geom_line() +
  geom_vline(xintercept = E, color = "red") +
  geom_hline(yintercept = pi_of_E(p = p, q = q, X = Xvec, theta = theta, R = R, beta = beta, c = c, E = E))

check_E_pi %>% 
  group_by(pi) %>% 
  summarise(E = sum(E)) %>% 
  ggplot(aes(x = E, y = pi)) +
  geom_line() +
  geom_vline(xintercept = E, color = "red") +
  geom_vline(xintercept = sum(max_Ei(p = p, q = q, Xvec = Xvec, theta = theta, R = R, beta = beta, c = c)[1:(n_patches-1)]))


Benchmark_results <- Sim(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E, save_plot = T) %>% 
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

DF_results <- tibble(Country = NA,
                     Reserve = NA,
                     Movement = NA,
                     Harvest = NA,
                     Vessel_Days = NA,
                     Stock = NA,
                     Stock_i = NA,
                     VDSprice = NA,
                     VDSrevenue = NA,
                     VDSrevenue_all = NA,
                     VDSrevenue_notKIR = NA)

for (i in 1:length(Rvec)) {
  R_i <- Rvec[i]
  for (j in 1:length(thetavec)){
    theta_j <- thetavec[j]
    
    E_m <- E # * theta + ((1 - theta)*(1-R))
    
    print(paste("Now running R =", R_i, "and theta = ", theta_j))
    
    DFtmp <- Sim(fvec = fvec, theta = theta_j, R = R_i, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E_m, save_plot = F)
    DF_results <- rbind(DF_results, DFtmp)
  }
}

DF_results <- DF_results %>% 
  drop_na()


# Revenue in Kiribati
DF_1 <- DF_results %>%
  filter(Country == "A") %>% 
  left_join(Benchmark_results, by = "Country") %>% 
  mutate(change = VDSrevenue - b_vdsrevenue,
         rel_change = change / b_vdsrevenue,
         change_notKIR = VDSrevenue_notKIR - b_vdsrevenue_notKIR,
         rel_change_notKIR = change_notKIR / b_vdsrevenue_notKIR,
         change_all = VDSrevenue_all - b_vdsrevenue_all,
         rel_change_all = change_all / b_vdsrevenue_all)

kir_rel_change <- ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = rel_change,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Relative change in revenue") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "None")

not_kir_rel_change <- ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = rel_change_notKIR,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "None")

pna_rel_change <- ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = rel_change_all,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 0.95)) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black"))

plot_grid(kir_rel_change,
          not_kir_rel_change,
          pna_rel_change,
          ncol = 3,
          labels = "AUTO")

ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = VDSprice,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab(quo("Vessel-day Price ("~pi~")")) +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot()


DF_results %>% 
  # filter(!Country == "J") %>% 
  ggplot() +
  geom_line(aes(x = Reserve,
                y = Vessel_Days,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Vessel-days per patch") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot() +
  facet_wrap(~Country, scales = "free_y")

DF_results %>% 
  filter(!Country == "J") %>%
  left_join(Benchmark_results, by = "Country") %>% 
  mutate(rel_VDSrevenue = (VDSrevenue - b_vdsrevenue) / b_vdsrevenue) %>% 
  ggplot() +
  geom_line(aes(x = Reserve,
                y = rel_VDSrevenue,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab(quo("change in VDS Revenue")) +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Country, scales = "free_y")

DF_results %>% 
  filter(!Country == "J") %>% 
  group_by(Reserve, Movement) %>%
  summarize(VDS = sum(Vessel_Days)) %>% 
  ggplot(aes(x = VDS)) +
  geom_density()

















