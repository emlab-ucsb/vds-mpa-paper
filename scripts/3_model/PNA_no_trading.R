#' PNA model with no trading
#' 
#' @description This is essentially a wrapper function. For a given set of parameters, including a spatial closure, the function solves for equilibrium price, biomass, and effort allocation. In this case, pi is solved at a patch-level, as opposed to a global pi.
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
#' @param Evec A vector of initial effort allocations
#'
#' @return A tibble containing the harvest, vessel-days, stock size, vessel-day price and vessel-day revenue for each country.
#' @export
#'
PNA_no_trading <- function(fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, Xvec = NULL, beta = NULL, c = NULL, Evec = NULL) {
  n_patches <- length(fvec)
  
  omega <- theta + (1 - theta) * (1 - R)
  
  tol <- 0.001 * Bnow
  diff <- (tol * 2)^2
  
  ct <- 1
  
  while (diff > tol) {
    
    # CALCULATE HARVESST AND BIOMASS
    ## total biomass before harvesting
    Xnow <- sum(Xvec)
    
    # Modify Evec in the high seas (leakage)
    Evec[10] <- ((p * q * Xvec[10]) / (beta * c)) ^ (1 / (beta - 1))
    
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
    
    ## Check convergence
    # Check difference in biomass, squared
    diff <- (Xnow - Xnew)^2
    
    # Print every 50 iterations to check what's going on
    if ((ct %% 50) == 0) {
      print(paste("difference is", sqrt(diff)))
    }
    
    # Update counter
    ct <- ct + 1
    Xvec <- fvec * Xnew
  }
  
  # Print result of objective function while debugging
  # print(sqrt(opt$objective))
  
  X_ss_vec <- Xvec
  E_ss_vec <- Evec
  H_ss_vec <- Hvec
  VDSprice_ss_vec <- pmax(0,
                          (p * q * Xvec) - (beta * c * (Evec ^ (beta - 1))))
  
  VDSprice_ss_vec[1] <- max(0,
                            (p * q * Xvec[1] * omega) - (beta * c * (Evec[1] ^ (beta - 1))))
  VDSprice_ss_vec[10] <- 0
  
  
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
  )# %>% 
    # mutate(Country = fct_relevel(Country, "KIR"))
  
  return(DF)
}
