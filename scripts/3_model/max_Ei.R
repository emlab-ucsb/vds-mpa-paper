#' Maximum effort
#' 
#' @description # This function calculates the MAXIMUM effort that could be attained by
#' a given stock size. The value of Ebar (target level of effort) must always be bellow this point
#'
#' @param p Price per ton
#' @param q Catchability
#' @param Xvec Biomass vector
#' @param theta Movement parameter (between 0 and 1)
#' @param R Percent of patch as reserve (between 0 and 1)
#' @param beta beta coefficient in costs
#' @param c costs
#'
#' @return A numeric vector showing the maximum attainable efforts assuming pi = 0
#' @export
#'
max_Ei <- function(p = NULL, q = NULL, Xvec = NULL, theta = NULL, R = NULL, beta = NULL, c = NULL) {
  # Get number of patches
  n_patches <- length(Xvec)
  
  # Create an omega term that "scales" biomass to
  # "fishable" biomass based on R and theta
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta) * (1 - R))
  
  # Calculate the maximum Ei assuming no vessel-day price
  E <- ((p * q * Xvec * omega) / (beta * c))^(1 / (beta - 1))
  
  # Returnt the vector
  return(E)
}