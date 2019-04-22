#' pi of E
#' 
#' @description Calculates vessel day price for a given effort restriction
#'
#' @param p Price per ton
#' @param q Catchability
#' @param X Total biomass
#' @param theta Movement parameter (between 0 and 1)
#' @param R Percent of patch as reserve (between 0 and 1)
#' @param beta beta coefficient in costs
#' @param c costs
#' @param E Total allowable effort
#'
#' @return The vessel-day price
#' @export
#'
pi_of_E <- function(p = NULL, q = NULL, X = NULL, theta = NULL, R = NULL, beta = NULL, c = NULL, E = NULL) {
  n_patches <- length(X)
  
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta) * (1 - R))
  
  pi <- (p * q * X * omega) - (beta * c * ((E)^(beta - 1)))
  
  pi[n_patches] <- 0 # Price in the high seas is 0
  
  pi <- pmax(pi, 0) # Prices cannot be negative
  
  return(pi)
}