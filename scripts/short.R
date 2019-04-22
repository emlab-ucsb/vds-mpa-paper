Benchmark_results <- PNA_with_trading(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E, save_plot = T) %>%
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

    DFtmp <- PNA_with_trading(fvec = fvec, theta = theta_j, R = R_i, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E_m, save_plot = F)
    DF_results <- rbind(DF_results, DFtmp)
  }
}

DF_results <- DF_results %>%
  drop_na()

# Revenue in Kiribati
DF_1_with_trading <- DF_results %>%
  filter(Country == "KIR") %>%
  left_join(Benchmark_results, by = "Country") %>%
  mutate(
    vessel_days_sold = b_vessel_days - Vessel_Days,
    sales_revenue = vessel_days_sold * VDSprice,
    all_VDSrevenue = VDSrevenue + sales_revenue,
    cost = b_vdsrevenue - all_VDSrevenue,
    rel_cost = cost / b_vdsrevenue
  )

costs_with_trading_plot <- ggplot(data = DF_1_with_trading) +
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
  theme(legend.position = "None")


ggplot(data = DF_1_with_trading) +
  geom_line(aes(
    x = Reserve,
    y = VDSprice,
    group = Movement,
    color = Movement
  ),
  size = 1
  ) +
  xlab("Portion of patch as reserve (R)") +
  ylab(quo("Vessel-day Price ("~pi~")")) +
  scale_color_viridis_c() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  startR::ggtheme_plot() +
  guides(color = guide_colorbar(title = quo("Movement ("~theta~")"),
                                ticks.colour = "black",
                                frame.colour = "black"))
  


DF_results %>%
  # filter(!Country == "J") %>%
  ggplot() +
  geom_line(aes(
    x = Reserve,
    y = Vessel_Days,
    group = Movement,
    color = Movement
  ),
  size = 1
  ) +
  xlab("Portion of patch as reserve (R)") +
  ylab("Vessel-days per patch") +
  scale_color_viridis_c() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  startR::ggtheme_plot() +
  facet_wrap(~Country, scales = "free_y")

DF_results %>%
  filter(!Country == "HS") %>%
  left_join(Benchmark_results, by = "Country") %>%
  mutate(rel_VDSrevenue = (VDSrevenue - b_vdsrevenue) / b_vdsrevenue) %>%
  ggplot() +
  geom_line(aes(
    x = Reserve,
    y = rel_VDSrevenue,
    group = Movement,
    color = Movement
  ),
  size = 1
  ) +
  xlab("Portion of patch as reserve (R)") +
  ylab(quo("change in VDS Revenue")) +
  scale_color_viridis_c() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) +
  startR::ggtheme_plot() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Country, scales = "free_y")

DF_results %>%
  filter(!Country == "HS") %>%
  group_by(Reserve, Movement) %>%
  summarize(VDS = sum(Vessel_Days)) %>%
  ggplot(aes(x = VDS)) +
  geom_density()


#####################################
# Vessel day-rule allocations thing
#####################################


# Allocation function
allocate <- function(alloc_input, alpha, year) {

  # Get the maximum year
  max_year <- year

  alloc_output <- alloc_input %>%
    filter(year > (max_year - 4)) %>%
    group_by(year) %>%
    mutate(
      Xtot = sum(Xi),
      Etot = sum(Ei)
    ) %>%
    ungroup() %>%
    group_by(country) %>%
    summarize(
      Xi = mean(Xi),
      Ei = mean(Ei)
    ) %>%
    mutate(
      Ei = Ei / E,
      Xi = Xi / K,
      alloc_rule = (alpha * Ei) + ((1 - alpha) * Xi),
      alloc_prop = alloc_rule / sum(alloc_rule),
      Ea = 45000 * alloc_prop
    ) %>%
    select(country, Ea)

  alloc_input %>%
    filter(year == 1) %>%
    select(-c(Ea, year)) %>%
    left_join(alloc_output, by = "country") %>%
    mutate(year = max_year + 1) %>%
    select(year, country, Ea, Ei, Xi, pi)
}

no_reserve <- PNA_with_trading(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E) %>%
  mutate(
    year = 0,
    Ea = Vessel_Days,
  ) %>%
  select(
    year,
    country = Country,
    Ea,
    Ei = Vessel_Days,
    Xi = Stock_i,
    pi = VDSprice
  )

# Reserve PNA_with_tradingilar to PIPA in size
reserve <- PNA_with_trading(fvec = fvec, theta = 0.5, R = 0.1, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E) %>%
  mutate(
    year = 1,
    Ea = no_reserve$Ea
  ) %>%
  select(
    year,
    country = Country,
    Ea,
    Ei = Vessel_Days,
    Xi = Stock_i,
    pi = VDSprice
  )


alloc_input <- rbind(no_reserve, reserve) %>%
  filter(!country == "HS")

allocations <- alloc_input %>%
  filter(year == 100)

for (alpha in seq(0, 1, by = 0.05)) {
  alloc_input_alpha <- alloc_input %>%
    mutate(alpha = alpha)
  for (i in 1:8) {
    alloc_i <- allocate(
      alloc_input = alloc_input_alpha,
      alpha = alpha,
      year = i
    )
    alloc_input_alpha <- rbind(alloc_input_alpha, alloc_i)
  }
  allocations <- rbind(allocations, alloc_input_alpha)
}


test <- allocations %>%
  filter(country == "KIR") %>%
  mutate(
    rev_dir = Ei * pi,
    deltaE = Ea - Ei,
    rev_sells = deltaE * pi,
    rev_tot = rev_dir + rev_sells,
    fct = delta^year
  ) %>%
  mutate(rev_tot = rev_tot * fct)


ggplot(test, aes(x = year, y = Ea, group = alpha, color = alpha)) + geom_line()

best_rev <- test %>% filter(alpha == 0) %$% sum(rev_tot)

test %>%
  group_by(alpha) %>%
  summarize(rev = sum(rev_tot)) %>%
  mutate(cost = (best_rev - rev) / best_rev) %>%
  ggplot(aes(x = alpha, y = cost)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = quo(alpha), y = quo("Cost (relative to" ~ alpha ~ "= 0)"))

##################################
## Testing
##################################



check_losses <- function(alpha, R, theta) {
  no_reserve <- PNA_with_trading(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E) %>%
    mutate(
      year = 0,
      Ea = Vessel_Days,
    ) %>%
    select(
      year,
      country = Country,
      Ea,
      Ei = Vessel_Days,
      Xi = Stock_i,
      pi = VDSprice
    )


  reserve <- PNA_with_trading(fvec = fvec, theta = 0.5, R = 0.1, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E) %>%
    mutate(
      year = 1,
      Ea = no_reserve$Ea
    ) %>%
    select(
      year,
      country = Country,
      Ea,
      Ei = Vessel_Days,
      Xi = Stock_i,
      pi = VDSprice
    )

  alloc_input <- rbind(no_reserve, reserve) %>%
    filter(!country == "HS")

  allocations <- alloc_input %>%
    filter(year == 100)

  for (i in 1:8) {
    alloc_i <- allocate(
      alloc_input = alloc_input,
      alpha = alpha,
      year = i
    )
    alloc_input <- rbind(alloc_input, alloc_i)
  }

  return(alloc_input)
}


Rs <- c(0.1, 0.3)
thetas <- seq(0, 1, by = 0.2)
alphas <- seq(0, 1, by = 0.1)

delta <- 1 / 1.1

plan(multiprocess)

jc <- expand.grid(R = Rs, theta = thetas, alpha = alphas) %>%
  as_tibble() %>%
  mutate(resutls = furrr::future_pmap(.l = list(alpha = alpha, R = R, theta = theta), .f = check_losses)) %>%
  unnest() %>%
  filter(country == "KIR") %>%
  mutate(
    rev_dir = Ei * pi,
    deltaE = Ea - Ei,
    rev_sells = deltaE * pi,
    rev_tot = rev_dir + rev_sells,
    fct = delta^year
  ) %>%
  mutate(rev_tot = rev_tot * fct)

no_reserve <- PNA_with_trading(fvec = fvec, theta = theta, R = R, r = r, K = K, p = p, q = q, Xvec = Xvec, beta = beta, c = c, E = E) %>%
  mutate(
    year = 0,
    Ea = Vessel_Days,
  ) %>%
  select(
    year,
    country = Country,
    Ea,
    Ei = Vessel_Days,
    Xi = Stock_i,
    pi = VDSprice
  )

best_rev <- check_losses(alpha = 0, R = 0, theta = 1) %>%
  filter(country == "KIR") %>%
  mutate(
    rev_dir = Ei * pi,
    deltaE = Ea - Ei,
    rev_sells = deltaE * pi,
    rev_tot = rev_dir + rev_sells,
    fct = delta^year
  ) %>%
  mutate(rev_tot = rev_tot * fct) %$%
  sum(rev_tot)

jc %>%
  group_by(alpha, R, theta) %>%
  summarize(rev = sum(rev_tot)) %>%
  ungroup() %>%
  mutate(
    cost = (best_rev - rev) / best_rev,
    R = paste("R = ", R)
  ) %>%
  ggplot(aes(x = alpha, y = cost, color = theta, group = theta)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = quo(alpha), y = quo("Cost (relative to" ~ alpha ~ "= 0)")) +
  facet_wrap(~R, ncol = 1) +
  scale_color_viridis_c() +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  ggtheme_plot()


##############################
## NO TRADING
##############################
