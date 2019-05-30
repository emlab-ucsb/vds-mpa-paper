#######################
#   model_data_prep   #
#######################

##############################################
# Takes a selected group of data and prepares
# it to have the shape (and variables) that
# I want for the did models.
##############################################

model_data_prep <- function(data, var = hours){
  var <- enquo(var)
  data %>% 
    mutate(post = post * 1,
           treated = treated * 1,
           year_c = as.character(year),
           year_c = fct_relevel(year_c, "2014"),
           year_month_c = as.character(year_month),
           year_month_c = fct_relevel(year_month_c, "2014-12-01"),
           quarter_c = as.character(quarter),
           quarter_c = fct_relevel(quarter_c, "2014.4"),
           flag_fe = !flag %in% c("JPN", "MEX", "NIC", "NZL", "PHL", "USA", "KIR", "PNG"),
           flag_fe_oth = flag_fe & !flag == "OTH",
           sate = case_when(date < lubridate::date("2014/06/01") ~ "1",
                            date > lubridate::date("2015/12/31") ~ "3",
                            T ~ "2"),
           log_var = log(!!var + sqrt(1 + ((!!var) ^ 2)))) #log-transformed (hyperbolic sine)
}
