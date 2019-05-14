#' Allocation function
#' 
#' @description This function takes a data.frame of allocations, historical usage,
#' patch-specific stock size, and VDS price. It then calculates what would be next
#' year's allocation based on the \code{(alpha * Ei) + ((1 - alpha) * Xi} formula
#' which balances a 4-year running mean of biomass and catches
#'
#' @param alloc_input 
#' @param alpha 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
allocate <- function(alloc_input, alpha, this_year, fvec = NULL, theta = NULL, R = NULL, r = NULL, K = NULL, p = NULL, q = NULL, beta = NULL, c = NULL, E = NULL, trade = NULL) {
  
  allocations <- alloc_input %>% 
    filter(year > (this_year - 6),
           !country == "HS") %>% 
    group_by(country) %>%
    summarize(
      Xi = mean(Xi),
      Ei = mean(Ei)
    ) %>%
    ungroup() %>%
    mutate(
      Ei = Ei / E,
      Xi = Xi / K,
      alloc_rule = (alpha * Ei) + ((1 - alpha) * Xi),
      alloc_prop = alloc_rule / sum(alloc_rule),
      Ea = E * alloc_prop
    ) %>%
    select(country, Ea) %>% 
    mutate(country = fct_relevel(country, "KIR")) %>% 
    arrange(country)
  
  E_hs_last <- alloc_input$Ea[alloc_input$country == "HS" & alloc_input$year == this_year]
  Ea <- c(allocations$Ea, E_hs_last)
  Xvec <- alloc_input$Xi[alloc_input$year == this_year]
  
  if(trade) {
    out <-
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
        year = this_year + 1
      )
  } else {
    out <-
      move_in_time_no_trading(
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
        Evec = Ea,
        year = this_year + 1
      )
  }
  
  return(out)
}