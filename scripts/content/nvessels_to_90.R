# nvessels to achieve 90% of activity per year
#
my_fun <- function(x) {
  tot <- nrow(x)
  x %>% mutate(c = cumsum(frac)) %>%
    filter(c < 90) %>%
    nrow() / tot * 100
}

PNA_without_KIR <- c("FSM",
                     "MHL",
                     "NRU",
                     "PLW",
                     "PNG",
                     "SLB",
                     "TUV",
                     "TKL")

PNA_countries <- c(PNA_without_KIR, "KIR")


vessel_data <- readRDS(file = here("raw_data",
                                   "activity_by_vessel_year_eez.rds")) %>% 
  filter(best_vessel_class == "tuna_purse_seines", 
         eez_iso3 %in% PNA_countries) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"),
         group = ifelse(is.na(treated), "Others", treated),
         days = hours / 24) %>%
  group_by(year, group, ssvid) %>%
  summarize(days = sum(days)) %>%
  arrange(year, desc(days)) %>%
  group_by(year) %>%
  mutate(total = sum(days)) %>%
  ungroup() %>%
  mutate(frac = days / total * 100) %>%
  group_by(year) %>%
  nest() %>% 
  mutate(percent = map_dbl(data, my_fun)) %>% 
  arrange(desc(percent))

vessel_data

