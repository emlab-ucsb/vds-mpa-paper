#' Solve for pi
#' 
#' @description This is the objective function used to find the equimarginal principle
#' It is used to test (via optim) many different values of pi such that the
#' resulting sum(Ei) = Ebar.
#'
#' @param par Initial value of the parameter of interest, pi
#' @param others A named list of parameters, containing E, p, q, Xvec, theta, R, beta, and c.
#'
#' @return The error: squared difference between total allowable effort and predicted effort
#' @export
#'
solve_pi <- function(par, others) {
  # Extract pi
  pi <- par[1]
  
  # Extract all other parameters
  E <- others[[1]]
  p <- others[[2]]
  q <- others[[3]]
  Xvec <- others[[4]]
  theta <- others[[5]]
  R <- others[[6]]
  beta <- others[[7]]
  c <- others[[8]]
  
  # Calculate number of patches
  n_patches <- length(Xvec)
  
  # Create a vector of efforts calling the E_vec_fxn
  E_vec <- E_vec_fxn(p = p, q = q, Xvec = Xvec, theta = theta, R = R, pi = pi, beta = beta, c = c)
  
  # Calculate total effort, excluding the high seas
  E_calc <- sum(E_vec[1:(n_patches - 1)])
  
  # Calculate error as the squared difference between E,
  # the total allowable effort
  # and E_calc, the predicted total effort
  err <- (E - E_calc)^2
  
  # Return the error
  return(err)
}