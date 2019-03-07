# nvessels to achieve 90% of activity per year
#
my_fun <- function(x) {
  tot <- nrow(x)
  
  x %>%
    arrange(desc(frac)) %>% 
    mutate(c = cumsum(frac)) %>%
    filter(c < 0.9) %>%
    nrow() / tot
}

add_groups <- function(x) {
  x %>% 
    mutate(groups = 1:nrow(.) / nrow(.),
           cumsum = cumsum(frac))
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
  mutate(frac = days / total) %>%
  group_by(year) %>%
  nest() %>% 
  mutate(data = map(data, add_groups)) %>% 
  mutate(percent = map_dbl(data, my_fun)) %>%
  arrange(desc(percent)) %>%
  unnest()

n_vessels <- ggplot(vessel_data,
       aes(x = groups, y = cumsum,
           color = as.character(year),
           group = year)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = 0.9,
             linetype = "dashed") +
  # geom_vline(aes(xintercept = percent, color = as.character(year))) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Fraction of vessels",
       y = "Percent of activity") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  guides(color = guide_legend(title = "Year"))


ggsave(plot = n_vessels,
       file = here("docs", "img", "nvessels_to_90.pdf"),
       width = 5,
       height = 4)


