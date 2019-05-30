#' PNA model with trading
#' 
#' @description This is essentially a wrapper function. For a given set of parameters, including a spatial closure, the function solves for equilibrium price, biomass, and effort allocation.
#'
#' @param fvec Vector of biomass distribution
#' @param theta Movement parameter
#' @param R Reserve size
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @param p price per ton
#' @param q catchability
#' @param Xvec Biomass vector
#' @param beta beta exponent for costs
#' @param c costs
#' @param E total allowable effort
#' @param save_plot Save a convergence plot? Boolean
#'
#' @return A tibble containing the harvest, vessel-days, stock size, vessel-day price and vessel-day revenue for each country.
#' @export
#'
PNA_with_trading <- function(fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, Xvec = NULL, beta = NULL, c = NULL, E = NULL, save_plot = FALSE) {
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
  
  tol <- 0.0001 * Bnow
  diff <- (tol * 2)^2
  
  ct <- 1
  
  while (diff > tol) {
    
    # OPTIMIZATION FOR PI
    ## set up
    others <- list(
      "E" = E,
      "p" = p,
      "q" = q,
      "Xvec" = Xvec,
      "theta" = theta,
      "R" = R,
      "beta" = beta,
      "c" = c
    )
    
    ## run optin to get pi for given Xvec
    opt <- nlminb(
      start = 1000,
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
    
    # CALCULATE HARVESST AND BIOMASS
    ## total biomass before harvesting
    Xnow <- sum(Xvec)
    
    ## Calculate the Ei with the optimized pi
    Evec <-
      E_vec_fxn(
        p = p,
        q = q,
        Xvec = Xvec,
        pi = pi,
        theta = theta,
        R = R,
        beta = beta,
        c = c
      )
    
    ## Calculate harvest vector
    Hvec <- (q * Evec * Xvec)
    # Harvest cannot be more than available biomass
    Hvec <- pmin(Hvec, Xvec)
    
    # Escapement is biomass - harvests
    evec <- Xvec - Hvec
    # Total escapement is sum of patch-level escapement
    e_all <- sum(evec)
    
    # Fish grow
    Xnew <- grow(e = e_all, r = r, K = K)
    
    # Calculate the square difference 
    # to test for convergence of
    # equilibrium biomass.
    diff <- (Xnow - Xnew)^2
    
    # Print every 50 iterations to check what's going on
    if ((ct %% 50) == 0) {
      print(paste("difference is", sqrt(diff)))
    }
    
    
    # Update counter
    ct <- ct + 1
    
    # Build tibble for convergence plot
    prog_i <- tibble(
      ct = ct,
      X = sum(Xvec),
      E = sum(Evec[1:(n_patches - 1)]),
      pi = pi,
      H = sum(Hvec),
      e = e_all,
      Xnew = grow(e = e_all, r = r, K = K),
      diff = diff,
      err = opt$objective
    )
    
    prog <- rbind(prog, prog_i)
    
    # New vector of biomass
    Xvec <- fvec * Xnew
  }
  
  # Print result of objective function while debugging
  # print(sqrt(opt$objective))
  
  # Save convergence plot
  if (save_plot) {
    my_plot <- drop_na(prog, ct) %>%
      select(ct, pi, X, E, H, e, Xnew, diff, err) %>%
      gather(variable, value, -ct) %>%
      ggplot(aes(x = ct, y = value)) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      ggtitle(label = paste("R = ", R, "theta = ", theta)) +
      startR::ggtheme_plot()
    
    ggsave(my_plot,
           filename = here::here(
             "scripts", "model", "conv_img",
             paste("R = ", R, "theta = ", theta, ".png")
           )
    )
  }
  
  X_ss_vec <- Xvec
  E_ss_vec <- Evec
  H_ss_vec <- Hvec
  
  # Vessel-day price is pi, but 0 on the high seas
  VDSprice_ss_vec <- c(rep(pi, n_patches - 1), 0)
  
  # Revenue to countries is vessel-day price times effort applied
  VDSrevenue_ss_vec <- VDSprice_ss_vec * Evec
  
  DF <- tibble(
    Country = names(fvec),
    Harvest = H_ss_vec,
    Vessel_Days = E_ss_vec,
    Stock = sum(X_ss_vec),
    Stock_i = X_ss_vec,
    VDSprice = VDSprice_ss_vec,
    VDSrevenue = VDSrevenue_ss_vec,
    VDSrevenue_all = sum(VDSrevenue_ss_vec),
    VDSrevenue_notKIR = sum(VDSrevenue_ss_vec[2:n_patches])
  ) #%>% 
    # mutate(Country = fct_relevel(Country, "KIR"))
  
  return(DF)
}