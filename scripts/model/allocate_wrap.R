#' Allocate wrap
#' 
#' @description This is a wrapper function to call allocate multiple
#' times and estimate different scenarios of the input parameters
#'
#' @param alpha 
#' @param R 
#' @param theta 
#' @param trade 
#'
#' @return
#' @export
#'
#' @examples
allocate_wrap <- function(alpha, R, theta, trade = NULL, fvec = NULL, r = NULL, K = NULL, p = NULL, q = NULL, beta = NULL, c = NULL, E = NULL, Xvec = NULL) {
  
  # Calculate year 0, with no reserve and trading to get optimal allocations
  no_reserve <-
    PNA_with_trading(
      fvec = fvec,
      theta = 1,
      R = 0,
      r = r,
      K = K,
      p = p,
      q = q,
      Xvec = Xvec,
      beta = beta,
      c = c,
      E = E
    ) %>%
    mutate(year = 0,
           Ea = Vessel_Days) %>%
    select(
      year,
      country = Country,
      Ea,
      Ei = Vessel_Days,
      Xi = Stock_i,
      pi = VDSprice
    )
  
  # Xvec <- no_reserve$Xi
  # Ea <- no_reserve$Ea
  # no_reserve <-
  #   move_in_time(
  #     fvec = fvec,
  #     theta = theta,
  #     R = R,
  #     r = r,
  #     K = K,
  #     p = p,
  #     q = q,
  #     Xvec = Xvec,
  #     beta = beta,
  #     c = c,
  #     E = E,
  #     Ea = Ea,
  #     year = 0
  #   )
  
  if(trade) {

    Xvec <- no_reserve$Xi
    Ea <- no_reserve$Ea
    reserve <-
      move_in_time(
        fvec = fvec,
        theta = theta,
        R = R,
        r = r,
        K = K,
        p = p,
        q = q,
        Xvec = Xvec,
        beta = beta,
        c = c,
        E = E,
        Ea = Ea,
        year = 1
      )
    
  } else {
    # Extract optimal allocation vector of efforts
    Evec <- no_reserve$Ei
    
    # Calculate effort received per patch on every year
    reserve <- 
      no_reserve %>%
      mutate(year = 1,
             pi = pmax(0, (p * q * Xi * (theta + (1 - theta) * (1 - R))) - (beta * c * (Ea ^ (beta - 1))))) %>%
      select(
        year,
        country,
        Ea,
        Ei,
        Xi,
        pi
      )
  }
  
  alloc_input <- rbind(no_reserve, reserve)
  
  for (i in 1:50) {
    alloc_i <- allocate(
      alloc_input = alloc_input,
      alpha = alpha,
      this_year = i,
      fvec = fvec,
      theta = theta,
      R = R,
      r = r,
      K = K,
      p = p,
      q = q,
      beta = beta,
      c = c,
      E = E,
      trade = trade
    )
    alloc_input <- rbind(alloc_input, alloc_i)
  }
  
  return(alloc_input)
}