


Evec <- Benchmark_results$b_vessel_days

Benchmark_results2 <-  PNA_no_trading(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, Evec = Evec) %>%
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
    
    DFtmp <- PNA_no_trading(fvec = fvec, theta = theta_j, R = R_i, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, Evec = Evec)
    DF_results_no_trading <- rbind(DF_results_no_trading, DFtmp)
  }
}

DF_results_no_trading <- DF_results_no_trading %>%
  drop_na()

# Revenue in Kiribati
DF_1_no_trading <- DF_results_no_trading %>%
  filter(Country == "KIR") %>%
  left_join(Benchmark_results_no_trading, by = "Country") %>%
  mutate(
    cost = b_vdsrevenue - VDSrevenue,
    rel_cost = cost / b_vdsrevenue
  )

costs_no_trading_plot <- ggplot(data = DF_1_no_trading) +
  geom_line(aes(
    x = Reserve,
    y = rel_cost,
    group = Movement,
    color = Movement
  ),
  size = 1
  ) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Costs (% of case with no reserve)") +
  scale_color_viridis_c() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_colorbar(title = quo("Movement ("~theta~")"), ticks.colour = "black", frame.colour = "black")) +
  theme(legend.justification = c(0, 0.9),
        legend.position = c(0, 1))

shorter_no_trading <- DF_1_no_trading %>% 
  select(Country, Reserve, Movement, Vessel_Days, VDSprice, VDSrevenue, cost, rel_cost)

shorter_trading <- DF_1_with_trading %>% 
  select(Country, Reserve, Movement, Vessel_Days_trading = Vessel_Days, pi_trading = VDSprice, VDSrevenue_trading = VDSrevenue, cost_trading = cost, rel_cost_trading = rel_cost)


both <- left_join(shorter_no_trading, shorter_trading, by = c("Country", "Reserve", "Movement")) %>% 
  mutate(delta_cost = cost - cost_trading,
         rel_delta_cost = delta_cost / cost)


delta_costs_plot <- ggplot(data = both) +
  geom_line(aes(
    x = Reserve,
    y = rel_delta_cost,
    group = Movement,
    color = Movement
  ),
  size = 1
  ) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Costs avoided by trading") +
  scale_color_viridis_c() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "None")


cowplot::plot_grid(costs_no_trading_plot,
                   costs_with_trading_plot,
                   delta_costs_plot, ncol = 1)




