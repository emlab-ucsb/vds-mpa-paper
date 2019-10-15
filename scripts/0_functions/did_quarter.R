did_quarter <- function(data, filter_oth = F){
  
  # Remove "OTH" vessels?
  if(filter_oth){
    data <- filter(data, flag_fe_oth)
  }
  
  # Remove data of vessels with not enough data for flag F-E
  flag_data <- data %>% 
    filter(flag_fe)
  
  m1 <- lm(log_var ~ quarter_c*treated, data = data)
  m2 <- lm(log_var ~ quarter_c*treated + flag, data = flag_data)
  
  return(list(m1, m2))
}
