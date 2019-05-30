######################################
rm(list = ls()) #This clears the workspace

## Load libraries
library(startR)
library(here)
library(sf)
library(tidyverse)

# Custom functions
source(here("scripts", "functions", "st_rotate.R"))

# Set up spatial stuff

eezs <- read_sf(dsn = here("data", "spatial", "PNA_EEZ"),
                layer = "PNA_EEZ") %>%
  select(eez_iso3 = ISO_Ter1) %>% 
  st_simplify(dTolerance = 0.01) %>%
  st_rotate() %>%
  st_union(by_feature = T)

skj <- read_sf(dsn = here("raw_data", "spatial", "SKJ"),
               layer = "SPC-SKJCWPAC-1950-2012-PONS") %>%
  select(stockid) %>% 
  st_simplify(dTolerance = 0.01) %>%
  st_rotate() %>%
  st_union(by_feature = T)

overlap <- st_intersection(x = eezs, y = skj) %>%
  mutate(area = st_area(.)) %>% 
  group_by(eez_iso3) %>% 
  summarize(area = sum(area))

##############

# Data on PNA (rough)
Countries <- c("KIR", "FSM", "MHL", "NRU", "PLW", "PNG", "SLB", "TKL", "TUV", "HS")
# Efforts <- c(9902, 11897, 10752, 2210, 2548, 11951, 13493, 1989, 4664, 10000)
Efforts <- c(14881, 5942, 5695, 1880, 99.1, 4234, 1959, 685, 2655, 15000) #observed vessel days in 2015

# Parameters
p <- 2500 #price per MT
beta <- 1.3
phi <- 0.188
Bmsy <- 2e6
MSY <- 1.6e6
K <- (phi + 1) ^ (1 / phi) * Bmsy
g <- MSY / K * (phi + 1) ^ (1 / phi)
Fmsy <- g
q <- MSY / (sum(Efforts) * Bmsy)
c <- 180 #340
# fvec <- c(0.11000000,
#           0.11577139,
#           0.11255024,
#           0.07306064,
#           0.07589643,
#           0.11591784,
#           0.11991738,
#           0.07103786,
#           0.08938853,
#           0.1164597)

# spatial stuff
X_hs <- 0.1*Bmsy
X_eez <- Bmsy - X_hs
prop_eez_area <- as.numeric(overlap$area / sum(overlap$area))

# fvec determined by EEZ area
fvec <- c(prop_eez_area * 0.9, 0.1)

# fvec determined by proportional efforts
# fvec <- Efforts / sum(Efforts)

Xstart <- Bmsy

exponent <- function(a, pow) {
  (abs(a)^pow)*sign(a)
  }

Grow <- function(e) {
  out <- e + (phi + 1) / phi * g * e * (1 - (e / K) ^ phi)
  return(out)
}

E_vec_fxn <- function(p, q, Xvec, omega, pi, beta, c) {
  # Maximum profitable vessel-day price
  # rev <- p*q*Xvec[1:9]*omega[1:9]
  pis <- rep(pi, 9)
  # Vector of vessel-day prices
  pi <- c(pis, 0)
  
  a <- ((p*q*Xvec*omega - pi)/(beta * c))
  pow <- 1 / (beta - 1)
  
  # E_vec <- pmax(exponent(a = a, pow = pow), a)
  E_vec <- a ^ pow
  
  return(E_vec)
}

solve_pi <- function(par, others) {
  pi <- par[1]
  # print(pi)
  # c <- par[2]
  
  E <- others[[1]]
  p <- others[[2]]
  q <- others[[3]]
  Xvec <- others[[4]]
  omega <- others[[5]]
  beta <- others[[6]]
  c <- others[[7]]
  
  E_vec <- E_vec_fxn(p, q, Xvec, omega, pi, beta, c)
  
  E_calc <- sum(E_vec[1:9])
  
  err <- (E - E_calc) ^ 2
  # print(paste("error was", err))

  if(any(E_vec < 0) | any(is.nan(E_vec))){err <- +Inf}
  
  return(err)
}

Sim1 <- function(R, theta, Xstart) {
  
  omega <- rep(1, 10)
  omega[1] <- theta + (1 - theta)*(1-R)
  
  Xvec <- Xstart * fvec
  # Xvec <- c(prop_eez_area * X_eez, X_hs)
  # Xvec <- rep(0.1 * Xstart, 10)
  tol <- 10 ^ 2
  diff <- 2 * tol
  
  ct <- 1
  
  while (diff > tol) {
    Xnow <- sum(Xvec)
    others <- list("E" = 45000,
                   "p" = p,
                   "q" = q,
                   "Xvec" = Xvec,
                   "omega" = omega,
                   "beta" = beta,
                   "c" = c)
    
    opt <- nlminb(start = 3000,
                  objective = solve_pi,
                  others = others,
                  # method = "Nelder-Mead",
                  lower = 0,
                  upper = 20000,
                  control = list(trace = TRUE,
                                 iter.max = 100000,
                                 abs.tol = 1e6,
                                 step.min = 0.01,
                                 step.max = 0.1))
    
    pi <- opt$par[1]
    # c <- opt$par[2]

    Evec <- E_vec_fxn(p, q, Xvec, omega, pi, beta, c)
    
    LXnow <- sum(Xvec)
    Hvec <- q * Evec * Xvec

    evec <- Xvec - Hvec
    e_all <- sum(evec)
    Xnew <- Grow(e = e_all)
    
    # diff <- (Xnow - Xnew)^2
    
    diff <- 10
    
    # Print every 50 iterations to check what's going on
    if((ct %% 50) == 0){
      print(paste("difference is", sqrt(diff)))
      }
    
    Xvec <- fvec * Xnew
    
    # Update counter
    ct <- ct + 1
  }
  
  print(paste("difference in Biomass was", sqrt(diff)))
  print(paste("difference in vessel-days was", sum(Evec[1:9] - others[[1]])))
  print(paste("the final pi was", pi))
  
  X_ss_vec <- Xvec
  E_ss_vec <- Evec
  H_ss_vec <- Hvec
  VDSprice_ss_vec <- c(rep(pi, length(Xvec) - 1), 0)
  
  VDSrevenue_ss_vec <- VDSprice_ss_vec * Evec
  
  DF <- data.frame(Country = Countries,
                   Reserve = R,
                   Movement = theta,
                   Harvest = H_ss_vec,
                   Vessel_Days = E_ss_vec,
                   Stock = sum(X_ss_vec),
                   Stock_i = X_ss_vec,
                   VDSprice = VDSprice_ss_vec,
                   VDSrevenue = VDSrevenue_ss_vec,
                   VDSrevenue_all = sum(VDSrevenue_ss_vec),
                   VDSrevenue_notKIR = sum(VDSrevenue_ss_vec[2:9]))
  
  return(DF)
}


Rvec <- seq(0, 1, 0.05)
thetavec <- seq(0, 1, 0.1)

# Benchmark
Benchmark_results <- Sim1(R = 0, theta = 0, Xstart = Xstart) %>% 
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
  R <- Rvec[i]
  for (j in 1:length(thetavec)){
    theta <- thetavec[j]
    
    print(paste("Now running R =", R, "and theta = ", theta))
    
    DFtmp <- Sim1(R = R, theta = theta, Xstart = Xstart)
    DF_results <- rbind(DF_results, DFtmp)
  }
}

DF_results <- DF_results %>%
  mutate_at(vars(VDSrevenue,
                 VDSrevenue_all,
                 VDSrevenue_notKIR),
            function(x){x/1e6}) # convert all revenues to millions


# Revenue in Kiribati
DF_1 <- DF_results %>%
  filter(Country == "KIR") %>% 
  left_join(Benchmark_results, by = "Country") %>% 
  mutate(change = VDSrevenue - b_vdsrevenue,
         rel_change = change / b_vdsrevenue,
         change_all = VDSrevenue_notKIR - min(VDSrevenue_notKIR))

ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = rel_change,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Change in revenue\nin Kiribati (million USD)") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  ggtheme_plot() +
  scale_y_continuous(labels = scales::percent)

ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = VDSrevenue_notKIR,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Change in revenue\nin other PNA (million USD)") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8))

tibble(country = Countries, Xvec = Xvec) %>%
  filter(!country == "HS") %>% 
  mutate(pi = list(seq(1, 15000, by = 100))) %>%
  unnest() %>%
  mutate(E = ((p*q*Xvec*omega - pi)/(beta * c))^((1)/(beta - 1))) %>%
  drop_na() %>% 
  ggplot(aes(x = E, y = pi, color = country)) + 
  geom_line(size = 1) +
  ggtheme_plot() +
  geom_hline(yintercept = 5107.1, linetype = "dashed") +
  labs(x = quo(E[i]), y = quo(pi)) +
  scale_color_brewer(palette = "Set1")
