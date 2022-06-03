######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)



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

X_i <- Bnow / 10
# Pis no frading
pi_mkt <- (p * q * X_i) - (beta * c * 5000 ^ (beta - 1))
pi_R <- (p * q * X_i * 0.9) - (beta * c * 5000 ^ (beta - 1))

data <- tibble(E = seq(1, 1e4, by = 10)) %>% 
  mutate(pi = (p * q * X_i) - (beta * c * E ^ (beta - 1)),
         pi2 = (p * q * 0.9 * X_i) - (beta * c * E ^ (beta - 1)))


p1 <- ggplot(data = data) +
  geom_line(aes(x = E, y = pi), color = "steelblue") +
  geom_segment(x = 5000, xend = 5000, y = 0, yend = pi_mkt, linetype = "dashed") +
  geom_segment(x = 0, xend = 5000, y = pi_mkt, yend = pi_mkt, linetype = "dashed", color = "steelblue") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1e4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(8e3, 13030)) +
  labs(x = "Effort (Vessel-days)",
       y = "Vessel-day price ($)") +
  startR::ggtheme_plot()

p2 <- p1 +
  geom_line(data = data, aes(x = E, y = pi2), color = "red") +
  geom_segment(x = 0, xend = 5000, y = pi_R, yend = pi_R, linetype = "dashed", color = "red")


others <- list(
  "E" = E,
  "p" = p,
  "q" = q,
  "Xvec" = rep(X_i, 10),
  "theta" = 0,
  "R" = 0.1,
  "beta" = beta,
  "c" = c
)

opt <- nlminb(
  start = 1000,
  objective = solve_pi,
  others = others,
  lower = 0,
  upper = 150000,
  control = list(
    iter.max = 1e6,
    abs.tol = 1,
    step.min = 0.1,
    step.max = 1000
  )
)
## extract pi
pi_trade <- opt$par[1]
E2 <- (((p * q * c(0.9 * X_i, X_i)) - pi_trade) / (beta * c))^(1 / (beta - 1))


p3 <- p2 +
  geom_segment(x = 0, xend = E2[2], y = pi_trade, yend = pi_trade, linetype = "dashed") +
  geom_segment(x = E2[1], xend = E2[1], y = 0, yend = pi_trade, linetype = "dashed") +
  geom_segment(x = E2[2], xend = E2[2], y = 0, yend = pi_trade, linetype = "dashed")
  


ggsave(plot = p1,
       filename = here("docs", "diss_img", "demand_curves_1.png"),
       width = 8,
       height = 4)

ggsave(plot = p2,
       filename = here("docs", "diss_img", "demand_curves_2.png"),
       width = 8,
       height = 4)

ggsave(plot = p3,
       filename = here("docs", "diss_img", "demand_curves_3.png"),
       width = 8,
       height = 4)






# No reserves
pi1 <- (p * q * X_i) - (beta * c * 5000 ^ (beta - 1))
E1 <- (((p * q * X_i) - pi1) / (beta * c))^(1 / (beta - 1))
(p * q * X_i) - (beta * c * E1 ^ (beta - 1))



E_vec_fxn(p = p, q = q, Xvec = rep(X_i, 10), theta = theta, R = R, pi = pi1, beta = beta, c = c)[1:9] %>% sum()


