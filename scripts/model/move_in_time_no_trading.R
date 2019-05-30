move_in_time_no_trading <- function(fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, Xvec = NULL, beta = NULL, c = NULL, Evec = NULL, year = NULL) {
  
  omega <- theta + (1 - theta) * (1 - R)
  
  pis <- pmax(0,
              (p * q * Xvec) - (beta * c * (Evec ^ (beta - 1))))
  
  pis[1] <- max(0,
                (p * q * Xvec[1] * omega) - (beta * c * (Evec[1] ^ (beta - 1))))
  
  pis[10] <- 0
  
  ## Calculate harvest vector
  # Initial harvest vector based on Evec
  Hvec <- (q * Evec * Xvec)
  # Modify harvest in patch 1 due to closure
  Hvec[1] <- q * Evec[1] * Xvec[1] * omega
  # Make sure that harvest does not exceed available biomass
  Hvec <- pmin(Hvec, Xvec)
  
  ## Escapement
  # Calculate patch-level escapement
  evec <- Xvec - Hvec
  # Calculate total escapement
  e_all <- sum(evec)
  
  ## Growth
  # Make fish grow
  Xnew <- grow(e = e_all, r = r, K = K)
  
  # Generate Xvec
  Xvec <- fvec * Xnew
  
  next_time <- tibble(
    year = year,
    country = names(fvec),
    Ea = Evec,
    Ei  = Evec,
    Xi = Xvec,
    pi = pis)
  
  return(next_time)
  
}
