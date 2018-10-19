did <- function(data, filter_oth = F, flag_fe = TRUE){
   
  # Remove "OTH" vessels?
  if(filter_oth){
    data <- filter(data, flag_fe_oth)
  }
  
  # Remove data of vessels with not enough data for flag F-E
  flag_data <- data %>% 
    filter(flag_fe)
  
  m1 <- lm(hours ~ post*treated + sate, data = data)
  m2 <- lm(hours ~ post*treated + sate + month_c, data = data)
  m3 <- lm(hours ~ post*treated + sate + month_c + flag, data = flag_data)
  
  if(flag_fe){
    return(list(m1, m2, m3))
  } else {return(list(m1, m2))}
}
