#' Growth function
#' 
#' This follows discrete logistic growth rate
#'
#' @param e Escapement (Biomass - harvest)
#' @param r Intrinsic growth rate
#' @param K Carrying capacity
#'
#' @return a number indicating the new biomass
#' @export
#'

grow <- function(e, r, K) {
  out <- e * exp(r * (1 - (e / K)))
  return(out)
}