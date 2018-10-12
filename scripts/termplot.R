# parsing a variable name: https://stackoverflow.com/questions/2309826/how-does-one-extract-the-name-of-a-variable-in-a-function-that-is-called-from-an

termplot<- function(model_list, q = F, title = deparse(substitute(model_list))){
  
  if(q){
    
    map_df(model_list, broom::tidy, .id = "model") %>%
      filter(str_detect(term, ":treat")) %>%
      mutate(year = as.numeric(str_extract_all(term, pattern = "[:digit:]+.[:digit:]"))) %>%
      ggplot(aes(x = year, y = estimate, color = model)) +
      geom_point() +
      geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate + std.error)) +
      ggtheme_plot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 2015, linetype = "dashed") +
      ggtitle(label = title)
    
  } else {
    map_df(model_list, broom::tidy, .id = "model") %>%
      filter(str_detect(term, ":treat")) %>%
      mutate(year = str_extract_all(term, pattern = "[:digit:]+-[:digit:]+-[:digit:]+"),
             year = lubridate::date(as.character(year))) %>%
      ggplot(aes(x = year, y = estimate, color = model)) +
      geom_point() +
      geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate + std.error)) +
      ggtheme_plot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = lubridate::date(paste(2015, 01, 01, sep = "/")), linetype = "dashed") +
      ggtitle(label = title)
    }
}

  