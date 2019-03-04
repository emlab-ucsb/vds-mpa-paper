######################################
rm(list = ls()) #This clears the workspace

## Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(gtable)
library(stringr)
library(scales)
library(rstudioapi)
library(xtable)
library(gridExtra)

# Data on PNA (rough)
Countries <- c("KIR", "FSM", "MHL", "NRU", "PLW", "PNG", "SLB", "TKL", "TUV", "HS")
Efforts <- c(9902, 11897, 10752, 2210, 2548, 11951, 13493, 1989, 4664, 10000)
# Efforts <- c(14881, 5942, 5695, 1880, 99.1, 4234, 1959, 685, 2655, 7189) #observed vessel days in 2015

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
c <- 180#340
fvec <- c(0.11000000,
          0.11577139,
          0.11255024,
          0.07306064,
          0.07589643,
          0.11591784,
          0.11991738,
          0.07103786,
          0.08938853,
          0.1164597)

Xstart<- Bmsy

Grow <- function(e) {
  out <- e + (phi + 1) / phi * g * e * (1 - (e / K) ^ phi)
  return(out)
}

Sim1 = function(R, theta) {
  Xvec <- Xstart * fvec
  Evec <- Efforts
  tol <- 10
  diff <- 2 * tol
  
  while (diff > tol) {
    Xnow <- sum(Xvec)
    Evec[10] <- ((p * q * Xvec[10]) / (beta * c)) ^ (1 / (beta - 1))
    Hvec <- q * Evec * Xvec
    Hvec[1] <- q * Evec[1] * Xvec[1] * (theta + (1 - theta) * (1 - R))
    
    evec <- Xvec - Hvec
    e_all <- sum(evec)
    Xnew <- Grow(e = e_all)
    diff <- abs(Xnew - Xnow)
    Xvec <- fvec * Xnew
  }
  
  X_ss_vec <- Xvec
  E_ss_vec <- Evec
  H_ss_vec <- Hvec
  VDSprice_ss_vec <- pmax(seq(0, 0, length.out = length(Xvec)),
                         p * q * Xvec - beta * c * Evec ^ (beta - 1))
  VDSprice_ss_vec[1] <- pmax(0,
                            p * q * Xvec[1] * (theta + (1 - theta) * (1 - R)) - beta * c * Evec[1] ^ (beta - 1))
  
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
thetavec <- seq(0.05, 1, 0.1)

DF_results <- Sim1(R = 0, theta = 0)


for (i in 1:length(Rvec)) {
  R <- Rvec[i]
  for (j in 1:length(thetavec)){
    theta <- thetavec[j]
    DFtmp <- Sim1(R = R, theta = theta)
    DF_results <- bind_rows(DF_results, DFtmp)
    # print(i)
    # print(j)
  }
}

DF_results <- DF_results %>% 
  mutate_at(vars(VDSrevenue, VDSrevenue_all, VDSrevenue_notKIR), function(x){x/1e6}) # convert all revenues to millions

# Revenue in Kiribati
DF_1 = DF_results %>%
  filter(Country == "KIR") %>% 
  mutate(change = VDSrevenue - max(VDSrevenue),
         change_all = VDSrevenue_notKIR - min(VDSrevenue_notKIR))

Plot1 = ggplot(data = DF_1) +
  geom_line(aes(x = Reserve,
                y = change,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Change in revenue\nin Kiribati (million USD)") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")

Plot3 <- ggplot(data = DF_1)+
  geom_line(aes(x = Reserve,
                y = change_all,
                group = Movement,
                color = Movement),
            size = 1) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Change in revenue in other\nPNA countries (million USD)") +
  scale_color_viridis_c() +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.justification = c(0, 1),
        legend.position = c(0, 1.1), legend.box = "horizontal") +
  guides(color = guide_colorbar(title = quo(Movement~(theta)),
                                ticks.colour = "black",
                                frame.colour = "black"))

PNA_model <- plot_grid(Plot1, Plot3, ncol = 1, labels = "AUTO")

# Save plot
ggsave(plot = PNA_model,
       filename = here("docs", "img", "PNA_model.pdf"),
       width = 3.4,
       height = 5.2)

# Stock
DF_4 = DF_results %>%
  filter(Country == "KIR")
Plot4 = ggplot(data = DF_4) +
  geom_line(aes(
    x = Reserve,
    y = Stock,
    group = Movement,
    color = Movement
  ), size = 1.5) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Total Stock")

Plot4

# VDS in HS
DF_5 = DF_results %>%
  filter(Country == "HS")

Plot5 = ggplot(data = DF_5) +
  geom_line(aes(
    x = Reserve,
    y = Vessel_Days,
    group = Movement,
    color = Movement
  ), size = 1.5) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Vessel Days in High seas")

Plot5

# Revenue loss
DF_6 = DF_results %>%
  filter(Country == "KIR") %>% 
  mutate(max_rev = max(VDSrevenue),
         loss = VDSrevenue - max_rev)

Plot6 = ggplot(data = DF_6) +
  geom_line(aes(
    x = Reserve,
    y = loss,
    group = Movement,
    color = Movement
  ), size = 1.5) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Forgone profits")

Plot6

# VDS price for KIR
DF_7 = DF_results %>%
  filter(Country == "KIR")

Plot7 = ggplot(data = DF_7) +
  geom_line(aes(
    x = Reserve,
    y = VDSprice,
    group = Movement,
    color = Movement
  ), size = 1.5) +
  xlab("Portion of patch as reserve (R)") +
  ylab("VDS price")

Plot7


# ggsave("../Fig1.pdf",Plot1)
# ggsave("../Fig2.pdf",Plot2)
# ggsave("../Fig3.pdf",Plot3)
# ggsave("../Fig4.pdf",Plot4)
