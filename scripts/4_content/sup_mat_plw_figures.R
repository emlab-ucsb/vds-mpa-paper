############################
############################

########################################################
# This script takes the different zones from Palau,
# clips the exclusive econmic zone, and derives the 
# border for the new PNMS
########################################################

# Load packages
library(startR)
library(here)
library(raster)
library(rmapshaper)
library(sf)
library(tidyverse)

# Load the shapefiles
# Load Palauan EEZ
plw_eez <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                   layer = "Palau_EEZ")

# Load PNMS boundaries
pnms_new <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                    layer = "PNMS")
# Load boundaries for the CMZ
commercial <- st_read(dsn = here::here("raw_data", "spatial", "PLW_shapefiles"),
                      layer = "DFZ_Commercial_Fishing_Zone") %>% 
  st_zm(what = "ZM")

months <- tibble(month = c(1:12),
                 month_c = as.factor(month.abb)) %>% 
  mutate(month_c = fct_relevel(month_c, month.abb))

fishing_data <- readRDS(file = here("raw_data", "rasterized_effort_plw_by_gear.rds"))

# MONTHLY USE #######################################################
## BY LONGLINERS ALL VDS
fishing_data %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month, year) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(months, by = "month") %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = days)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  facet_wrap(~month_c) +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C", trans = "log10") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  ggtitle("Mean monthly vessel-days by longliners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im1.png"),
       width = 5,
       height = 5)
  
## BY LONGLINERS FISHING HOURS
fishing_data %>% 
  filter(fishing) %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(months, by = "month") %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  facet_wrap(~month_c) +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  ggtitle("Mean monthly fishing hours by longliners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im2.png"),
       width = 5,
       height = 5)

## BY PURSE SEINES ALL VDS
fishing_data %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month, year) %>% 
  summarize(days = sum(hours_length, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(months, by = "month") %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = days)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  facet_wrap(~month_c) +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  ggtitle("Mean monthly vessel-days by purse seiners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im3.png"),
       width = 5,
       height = 5)

## BY PURSE SEINES FISHING HOURS
fishing_data %>% 
  filter(fishing) %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(months, by = "month") %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  facet_wrap(~month_c) +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  ggtitle("Mean monthly fishing hours by purse seiners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im4.png"),
       width = 5,
       height = 5)

# GENERAL Mean #######################################################

## VESSEL DAYS BY LONGLINERS
fishing_data %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, year) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  filter(days < 50) %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = days)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  ggtitle("Mean vessel-days by longliners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im5.png"),
       width = 5,
       height = 5)

## FISHING HOURS BY LONGLINERS
fishing_data %>% 
  filter(fishing) %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, year) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  group_by(lat_bin_center, lon_bin_center) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  ggtitle("Mean fishing hours by longliners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im6.png"),
       width = 5,
       height = 5)

## VESSEL DAYS BY Purse seiners
fishing_data %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, year) %>% 
  summarize(days = sum(hours_length, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  filter(days < 50) %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = days)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  ggtitle("Mean vessel-days by purse seiners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im7.png"),
       width = 5,
       height = 5)

## FISHING HOURS BY Purse seiners
fishing_data %>% 
  filter(fishing) %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, year) %>% 
  summarize(hours = sum(hours_length, na.rm = T)) %>% 
  group_by(lat_bin_center, lon_bin_center) %>% 
  summarize(hours = mean(hours, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_sf(data = pnms_new, fill = "transparent", color = "blue") +
  geom_sf(data = commercial, fill = "transparent", color = "red") +
  ggtheme_map() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black"))+
  ggtitle("Mean fishing hours by purse seiners (2012 - 2019)",
          "2019 contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im8.png"),
       width = 5,
       height = 5)

## TIMESERIES STUFF NOW ########################### 

fishing_data_short <- fishing_data %>% 
  filter(between(year, 2014, 2019))

fishing_data_short %>% 
  group_by(month, year, vessel_class) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  left_join(months, by = "month") %>% 
  mutate(vessel_class = ifelse(vessel_class == "drifting_longlines",
                                    "Longlines",
                                    "Purse seines")) %>% 
  ggplot(aes(x = month_c, y = days, group = year)) +
  geom_line(size = 1, aes(color = as.factor(year))) +
  geom_point() +
  facet_wrap(~vessel_class, scales = "free_y", ncol = 1) +
  ggtheme_plot() +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Year")) +
  ggtitle("Monthly vessel-days by year",
          subtitle = "2019 data contains Jan - Jun") +
  labs(x = "Month", y = "Vessel-days")

ggsave(filename = here("docs", "img", "plw_nceas", "im9.png"),
       width = 6,
       height = 4)
  
fishing_data_short %>% 
  group_by(month, year, vessel_class) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  mutate(date = lubridate::date(paste(year, month, "1", sep = "-"))) %>% 
  mutate(vessel_class = ifelse(vessel_class == "drifting_longlines",
                               "Longlines",
                               "Purse seines")) %>% 
  ggplot(aes(x = date, y = days, group = year)) +
  geom_line(size = 1, aes(color = as.factor(year))) +
  geom_point() +
  facet_wrap(~vessel_class, scales = "free_y", ncol = 1) +
  ggtheme_plot() +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Year")) +
  labs(x = "Year", y = "Vessel-days") +
  ggtitle("Monthly vessel-days",
          subtitle = "2019 data contains Jan - Jun")

ggsave(filename = here("docs", "img", "plw_nceas", "im10.png"),
       width = 6,
       height = 4)

fishing_data_short %>% 
  group_by(vessel_class, year) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = days)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "steelblue", color = "black") +
  facet_wrap(~vessel_class, scales = "free_y", ncol = 1) +
  labs(x = "Year", y = "Vessel-days") +
  ggtheme_plot() +
  ggtitle("Annual vessel-days",
          subtitle = "2019 data contains Jan - June")

ggsave(filename = here("docs", "img", "plw_nceas", "im11.png"),
       width = 6,
       height = 4)

commercial_sp <- as_Spatial(commercial)

pct_activity_ts_month <- function(df, shp) {
  
  months <- unique(df$month)
  
  results <- tibble(month = months, pct_activity = NA)
  
  
  ct <- 1
  
  for(i in months){

    # this_year <- results$year[ct]
      #Rasterize ith year and jth month
      rast_i <- df %>% 
        filter(month == i) %>% 
        select(lon_bin_center, lat_bin_center, days) %>% 
        rasterFromXYZ(xyz = ., crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      # Calculate proportion within PNMS
      pct_rast_i <- rast_i %>%
        mask(x = ., mask = shp) %>% 
        cellStats(sum) / cellStats(rast_i, sum)
      
      # Save into dataframe for this year
      results$pct_activity[ct] <- pct_rast_i
      
      ct <- ct + 1
  }
  
  return(results)
}

ll_ts <- fishing_data %>% 
  filter(vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month, year) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  pct_activity_ts_month(shp = commercial_sp) %>% 
  mutate(gear = "Longlines")

ps_ts <- fishing_data %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(lat_bin_center, lon_bin_center, month, year) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  group_by(lat_bin_center, lon_bin_center, month) %>% 
  summarize(days = mean(days, na.rm = T)) %>% 
  ungroup() %>% 
  pct_activity_ts_month(shp = commercial_sp) %>% 
  mutate(gear = "Purse seines")


rbind(ll_ts, ps_ts) %>% 
  left_join(months, by = "month")  %T>% 
  write.csv(here("docs", "img", "plw_nceas", "monthly_activity_in_CFZ.csv"),
            row.names = F) %>% 
  ggplot(aes(x = month_c, y = pct_activity, fill = gear, group = gear)) +
  geom_line() +
  geom_point(size = 3, shape = 21, color = "black") +
  labs(x = "Month", y = "Vessel-days (% from total)") +
  ggtheme_plot() +
  ggtitle("Monthly activity within CFZ (2012 - 2019)",
          "2019 contains Jan - Jun") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.25))

ggsave(filename = here("docs", "img", "plw_nceas", "im12.png"),
       width = 6,
       height = 3)

###############

pct_activity_ts <- function(df, shp) {
  years <- unique(df$year)
  
  results <- tibble(year = years, pct_activity = NA)
  
  ct <- 1
  
  for(i in years){
    #Rasterize ith year
    rast_i <- df %>% 
      filter(year == i) %>% 
      select(lon_bin_center, lat_bin_center, days) %>% 
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
  group_by(year, lat_bin_center, lon_bin_center) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  pct_activity_ts(shp = commercial_sp) %>% 
  mutate(gear = "Longlines")

ps_ts <- fishing_data %>% 
  filter(!vessel_class == "drifting_longlines") %>% 
  group_by(year, lat_bin_center, lon_bin_center) %>% 
  summarize(days = sum(hours, na.rm = T) / 24) %>% 
  ungroup() %>% 
  pct_activity_ts(shp = commercial_sp) %>% 
  mutate(gear = "Purse seines")


rbind(ll_ts, ps_ts) %T>% 
  write.csv(here("docs", "img", "plw_nceas", "annual_activity_in_CFZ.csv"),
            row.names = F) %>% 
  ggplot(aes(x = year, y = pct_activity, fill = gear)) +
  geom_line() +
  geom_point(size = 3, shape = 21, color = "black") +
  labs(x = "Year", y = "Vessel-days (% from total)") +
  ggtheme_plot() +
  ggtitle("Annual activity within CFZ (2012 - 2019)",
          subtitle = "2019 contains Jan - June") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2))

ggsave(filename = here("docs", "img", "plw_nceas", "im13.png"),
       width = 6,
       height = 4)


