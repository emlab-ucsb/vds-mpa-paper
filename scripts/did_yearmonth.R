did_yearmonth <- function(data, filter_oth = F){
  
  # Remove "OTH" vessels?
  if(filter_oth){
    data <- filter(data, flag_fe_oth)
  }
  
  # Remove data of vessels with not enough data for flag F-E
  flag_data <- data %>% 
    filter(flag_fe)
  
  m1 <- lm(hours ~ year_month_c*treated + sate, data = data)
  m2 <- lm(hours ~ year_month_c*treated + sate + flag, data = flag_data)
  
  return(list(m1, m2))
}
