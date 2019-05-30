move_in_time <- function(fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, Xvec = NULL, beta = NULL, c = NULL, E = NULL, Ea = NULL, year = NULL){
  
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
      abs.tol = 5,
      step.min = 10,
      step.max = 1000
    )
  )
  ## extract pi
  pi <- opt$par[1]
  
  # CALCULATE HARVESST AND BIOMASS

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
  
  Xvec <- fvec * Xnew
  
  pis <- c(rep(pi, length(fvec) - 1), 0)

  next_time <- tibble(
    year = year,
    country = names(fvec),
    Ea = Ea,
    Ei  = Evec,
    Xi = Xvec,
    pi = pis)
  
  return(next_time)
}