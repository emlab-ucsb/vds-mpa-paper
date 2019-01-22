did <- function(data, filter_oth = F, flag_fe = TRUE){
   
  # Remove "OTH" vessels?
  if(filter_oth){
    data <- filter(data, flag_fe_oth)
  }
  
  # Remove data of vessels with not enough data for flag F-E
  flag_data <- data %>% 
    filter(flag_fe)
  
  m1 <- lm(log_var ~ post*treated, data = data)
  m2 <- lm(log_var ~ post*treated + month_c, data = data)
  m3 <- lm(log_var ~ post*treated + month_c + flag, data = flag_data)
  
  if(flag_fe){
    return(list(m1, m2, m3))
  } else {return(list(m1, m2))}
}
