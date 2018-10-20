library(raster)
library(fasterize)
library(sf)
library(tidyverse)
source(here::here("scripts", "st_rotate.R"))

rasterize_df <- function(x, r, fun = "sum"){
  x2 <- x %>%
    select(lon, lat, hours) %>% 
    mutate(lon = ifelse(lon < 0, lon + 180, lon - 180) + 180) %>% 
    as.matrix()
  
  rasterize(x = x2[,1:2],
            y = r,
            field = x2[,3],
            fun = fun) %>% 
    return()
}

extract_raster <- function(r, year, month = NULL){
  as.data.frame(r, xy = T) %>%
    rename(hours = layer) %>% 
    mutate(year = year,
           month = month) %>% 
    return()
}

eezs_exclude <- c("HS MUS 1", "EEZ MUS 1", "EEZ MDG 1", "EEZ MDG 2", "HS MDG 1", "HS MDG 2", "HS MOZ 1", "EEZ MOZ 1")

regions <- read_sf(dsn = here::here("data", "spatial", "regions"),
                   layer = "regions") %>% 
  filter(!id %in% eezs_exclude)

regions_raster <- regions %>%
  mutate(unique = group_indices(., id)) %>%
  fasterize(sf = .,
            raster = raster(., res = 1),
            field = "unique",
            background = 0)

regions_with_HS <- regions %>%
  mutate(unique = group_indices(., id)) %>% 
  st_set_geometry(NULL) %>% 
  rbind(data.frame(id = "HS HS 1", source = "HS", PNA = 0, country = "HS", unique = 0))

regions_raster_df <- as.data.frame(regions_raster, xy = T) %>% 
  left_join(regions_with_HS, by = c("layer" = "unique"))

monthly_vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>% 
  filter(gear == "purse_seines",
         year < 2018,
         fishing,
         treated) %>% 
  drop_na(hours, fishing) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")))

# Vector of dates to iterate over
dates <- sort(unique(monthly_vessel_tracks$date))

# iniialize empty df
monthly_effort_raster <- data.frame(x = NULL,
                                    y = NULL,
                                    hours = NULL,
                                    year = NULL,
                                    month = NULL)

for(i in 1:length(dates)){
  # Rasterize the points for month i
  raster_i <- monthly_vessel_tracks %>% 
    filter(date == dates[i]) %>% 
    rasterize_df(r = regions_raster)
  
  # Export raster in case this thing breaks
  writeRaster(x = raster_i,
              filename = here::here("data",
                                    "spatial",
                                    "monthly_rasterized_effort_by_region",
                                    paste0(dates[i], ".tif")),
              overwrite = TRUE)
  
  # Convert raster into data.frame
  raster_i_df <- raster_i %>% 
    extract_raster(year = lubridate::year(dates[i]),
                   month = lubridate::month(dates[i])) %>% 
    drop_na(hours)
  
  # Allow data.frame to grow so that it has 72 months times nrow times ncol in the raster
  monthly_effort_raster <- rbind(monthly_effort_raster, raster_i_df)
}

# Combine the rasterized dataframe with the regions to match them
monthly_effort_raster <- monthly_effort_raster %>% 
  left_join(regions_raster_df, by = c("x", "y")) %>%
  group_by(year, month) %>% 
  mutate(max_hours = max(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_norm = hours / max_hours) %>% 
  filter(!id %in% c("HS PNG 1", "EEZ PNG 1", "HS MHL 1", "HS FSM 1", "HS NRU 1", "HS PNG 2", "HS SLB 1", "HS TUV 1")) %>%
  drop_na() %>%
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")),
         date_c = as.factor(date),
         year_c = as.factor(year),
         month_c = as.factor(month),
         quarter = as.factor(lubridate::quarter(date, with_year = T)))

saveRDS(monthly_effort_raster, file = here::here("data", "monthly_rasterized_effort_by_region.rds"))









































