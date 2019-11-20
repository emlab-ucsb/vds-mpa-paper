# Load packages
library(startR)
library(here)
library(raster)
library(rmapshaper)
library(sf)
library(cowplot)
library(tidyverse)

# Load the shapefiles
# Load Palauan EEZ
plw_eez <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                   layer = "Palau_EEZ")

plw_coast <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                     layer = "MainIsland_Baselines")

# Load PNMS boundaries
pnms_new <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                    layer = "PNMS")

pnms_new_sp <- as_Spatial(pnms_new)

# Load fishing data
fishing_data <- readRDS(file = here("raw_data", "rasterized_effort_plw_by_gear.rds")) %>% 
  filter(year < 2019) %>% 
  group_by(year, vessel_class, lat_bin_center, lon_bin_center) %>% 
  summarize(vessel_days = sum(hours_length, na.rm = T) / 24) %>% 
  ungroup()


(plw_2018_ll <- fishing_data %>% 
  filter(vessel_class == "drifting_longlines",
         year == 2018) %>% 
  ggplot() +
  geom_tile(aes(x = lon_bin_center, y = lat_bin_center, fill = vessel_days)) +
  geom_sf(data = plw_eez, fill = "transparent", color = "black", size = 2) +
  geom_sf(data = pnms_new, fill = "transparent", color = "red", size = 1) +
  scale_fill_gradientn(colors = colorRamps::matlab.like(20),
                       trans = "log10",
                       limits = c(0.001, 15),
                       breaks = c(0.001, 0.01, 0.1, 1, 10),
                       labels = c(0.001, 0.01, 0.1, 1, 10)
                       ) +
  scale_x_continuous(breaks = seq(130, 136, by = 2)) +
  theme_minimal() +
  guides(fill = guide_colorbar(title = "Vessel-days",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  labs(x = "", y = ""))

# Plot purse seine fishing effort for 2018
(plw_2018_ps <- fishing_data %>% 
    filter(vessel_class == "tuna_purse_seines",
           year == 2018) %>% 
    ggplot() +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = vessel_days)) +
    geom_sf(data = plw_eez, fill = "transparent", color = "black", size = 2) +
    geom_sf(data = pnms_new, fill = "transparent", color = "red", size = 1) +
    scale_fill_gradientn(colors = colorRamps::matlab.like(20),
                         trans = "log10",
                         limits = c(0.0001, 1),
                         breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
                         labels = c("0.0001", 0.001, 0.01, 0.1, 1)
                         ) +
    scale_x_continuous(breaks = seq(130, 136, by = 2)) +
    theme_minimal() +
    guides(fill = guide_colorbar(title = "Vessel-days",
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
    labs(x = "", y = ""))

# Put plots together
plw_2018 <- plot_grid(plw_2018_ll,
                      plw_2018_ps,
                      ncol = 2,
                      labels = c("Longlines", "Purse seines"),
                      label_fontface = "plain",
                      label_size = 12)
# Export plot as tiff
ggsave(plw_2018,
       filename = here("docs", "img", "plw_2018_ED_Fig3.tiff"),
       width = 7,
       height = 3.5)

# Export plot as pdf
ggsave(plw_2018,
       filename = here("docs", "img", "plw_2018.pdf"),
       width = 7,
       height = 3.5)

########### TIME SERIES

pct_activity_ts <- function(df, shp) {
  years <- unique(df$year)
  
  results <- tibble(year = years, pct_activity = NA)
  
  ct <- 1
  
  for(i in years){
    #Rasterize ith year
    rast_i <- df %>% 
      filter(year == i) %>% 
      select(lon_bin_center, lat_bin_center, vessel_days) %>% 
      rasterFromXYZ(xyz = ., crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # Calculate proportion within PNMS
    pct_rast_i <- rast_i %>%
      mask(x = ., mask = shp) %>% 
      cellStats(sum) / cellStats(rast_i, sum)
    
    # Save into dataframe
    results$pct_activity[ct] <- pct_rast_i
    
    ct <- ct + 1
  }
  
  return(results)
}

ll_ts <- fishing_data %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  pct_activity_ts(shp = pnms_new_sp) %>% 
  mutate(gear = "Longlines")

ps_ts <- fishing_data %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  pct_activity_ts(shp = pnms_new_sp) %>% 
  mutate(gear = "Purse seines")

# Numbers that go in the text
rbind(ll_ts, ps_ts) %>%
  group_by(gear) %>%
  summarize(mean = mean(pct_activity),
            sd = sd(pct_activity))

rbind(ll_ts, ps_ts) %>% 
  ggplot(aes(x = year, y = pct_activity, fill = gear)) +
  geom_line() +
  geom_point(size = 3, shape = 21, color = "black") +
  labs(x = "Year", y = "Percent fishing activity within PNMS") +
  ggtheme_plot() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent, limits = c(0.75, 1)) +
  guides(fill = guide_legend(title = "Gear")) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1))

ggsave(filename = here("docs", "img", "plw_ts_plot.pdf"),
       width = 6,
       height = 4)

ggsave(filename = here("docs", "img", "plw_ts_plot_ED_Fig4.tiff"),
       width = 6,
       height = 4)
