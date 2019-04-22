#' Effort allocation
#'
#' @description Given parameters (pi and X the most important ones),
#' this function calculates the effort for each patch. If efforts were to
#' be negative, a zero is returned instead.
#'
#' @param p Price per ton
#' @param q Catchability
#' @param Xvec Biomass vector
#' @param theta Movement parameter (between 0 and 1)
#' @param R Percent of patch as reserve (between 0 and 1)
#' @param pi Vessel day price
#' @param beta beta coefficient in costs
#' @param c costs
#'
#' @return a numeric vector of length Xvec containing the effort allocation to each parcel
#' @export
#'
E_vec_fxn <- function(p = NULL, q = NULL, Xvec = NULL, theta = NULL, R = NULL, pi = NULL, beta = NULL, c = NULL) {
  
  # Get the number of patches
  n_patches <- length(Xvec)
  
  # Create an omega term that "scales" biomass to
  # "fishable" biomass based on R and theta
  omega <- rep(1, n_patches)
  omega[1] <- theta + ((1 - theta) * (1 - R))
  
  # Create a vector of prices, wherte the high seas haven price (pi = 0)
  pi <- c(rep(pi, n_patches - 1), 0)
  
  # Calculate the attainable level of effort in each patch
  E_vec <- (((p * q * Xvec * omega) - pi) / (beta * c))^(1 / (beta - 1))
  
  # These are the revenues. Prices cannot be higher than this, or would
  # imply negative efforts
  threshold <- p * q * Xvec * omega
  
  # Replace values that are above the threashold with zeros
  E_vec[threshold < pi] <- 0
  
  # Return the vector
  return(E_vec)
}