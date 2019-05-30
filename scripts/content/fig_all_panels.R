###################
#   all_panels   #
###################


###################################################################
# Daily fishing hours for all vessels in our maintreatment-control
# groups. Red and blue lines show monthly avarages for each group.
# Vertical dashed lines indicates dates when satellites were added,
# solid line indicates PIPA closurePIPA closure.
##################################################################


#Load packages
library(here)
library(cowplot)
library(tidyverse)

# Varplot
varplot <- function(data, var, y_lab){
  var <- enquo(var)
  
  ggplot(data = data, mapping = aes(x = date, y = !!var, color = treated)) +
    # geom_jitter(shape = ".", height = 0, alpha = 0.1) +
    stat_summary(geom = "line", fun.y = mean, size = 1) +
    theme_cowplot() +
    scale_color_brewer(palette = "Set1", direction = -1) +
    guides(color = guide_legend(title = "Group")) +
    geom_vline(xintercept = lubridate::date("2015/01/01")) +
    geom_vline(xintercept = lubridate::date("2014/06/01"), linetype = "dashed") +
    geom_vline(xintercept = lubridate::date("2015/12/31"), linetype = "dashed") +
    theme(legend.position = "none",
          text = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    labs(x = "Date", y = y_lab)
}

## FISHING HOURS ##################################################################
# Load data
effort_by_vessel <- readRDS(file = here("data", "panels", "daily_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/")),
         treated = ifelse(treated, "Displaced", "Non-displaced"))

# Plot of fishing hours
fishing_hours <- effort_by_vessel %>% 
  filter(fishing) %>%
  varplot(hours, "hours") +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1.1))

# Plot of nonfishing hours
nonfishing_hours <- effort_by_vessel %>% 
  filter(!fishing) %>%
  varplot(hours, "hours")

## PROPORTIONFISHING HOURS ##################################################################
# Load data
prop_fishing_by_vessel <-
  readRDS(file = here("data", "panels", "daily_prop_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/")),
         treated = ifelse(treated, "Displaced", "Non-displaced")) %>%
  varplot(prop_fishing, "% hours") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

## DISTANCE TRAVELED ##################################################################
#Load data
distance_traveled <- readRDS(file = here("data",
                                                "panels",
                                                "daily_distance_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines",
         dist < quantile(dist, probs = 0.95)) %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/")),
         treated = ifelse(treated, "Displaced", "Non-displaced")) %>%
  varplot(dist, "dist (km)")


## DISTANCE FROM PORT AND SHORE ##################################################################
# Load data
distance_port_shore <- readRDS(file = here("data",
                                                 "panels",
                                                 "distance_from_port_shore_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-")),
         treated = ifelse(treated, "Displaced", "Non-displaced"))

# Distance from port
distance_from_port <- distance_port_shore %>%
  varplot(mean_dist_port, "dist (km)")

# Distance from shore
distance_from_shore <- distance_port_shore %>%
  mutate(mean_dist_shore = mean_dist_shore) %>%  # Just converting to Km
  varplot(mean_dist_shore, "dist (km)")




## DISTANCE FROM PORT AND SHORE FISHING ONLY ######################################################

# Load data
distance_port_shore_fishing <- readRDS(file = here("data",
                                                 "panels",
                                                 "distance_from_port_shore_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "-")),
         treated = ifelse(treated, "Displaced", "Non-displaced"))

# Distance from port
distance_from_port_fishing <- distance_port_shore_fishing %>%
  mutate(mean_dist_port = mean_dist_port / 1000) %>%  # Just converting to Km
  varplot(mean_dist_port, "dist (km)")

# Distance from shore
distance_from_shore_fishing <- distance_port_shore_fishing %>%
  mutate(mean_dist_shore = mean_dist_shore / 1000) %>%  # Just converting to Km
  varplot(mean_dist_shore, "dist (km)")

###### PROPORTION OF HOURS SPENT IN KIR ###################################################
# Load data
kir_fishing <- readRDS(file = here("data", "panels", "KIR_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/"))) %>% 
  varplot(kir_hours, "hours")

###### PROPORTION OF HOURS SPENT IN VDS ###################################################
# Load data
vds_fishing <- readRDS(file = here("data", "panels", "VDS_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/"))) %>% 
  varplot(vds_hours, "hours")

###### HOURS SPENT IN HS ###################################################
# Load data
hs_fishing <- readRDS(file = here("data", "panels", "HS_fishing_hours_by_vessel_panel.rds")) %>% 
  filter(year < 2019,
         gear == "tuna_purse_seines") %>% 
  mutate(date = lubridate::date(paste(year, month, 15, sep = "/"))) %>% 
  varplot(hs_hours, "hours")

# Combine into grid
plot <- cowplot::plot_grid(
  plotlist = list(
    fishing_hours,
    nonfishing_hours,
    prop_fishing_by_vessel,
    distance_traveled,
    distance_from_port_fishing,
    distance_from_shore_fishing,
    kir_fishing,
    vds_fishing,
    hs_fishing),
  labels = "AUTO",
  ncol = 2,
  label_size = 10
)

# Export figure
ggsave(plot = plot,
       filename = here("docs", "img", "all_panels.pdf"),
       width = 6.5,
       height = 8)
























