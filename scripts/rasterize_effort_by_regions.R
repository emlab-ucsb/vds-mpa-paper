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

extract_raster <- function(r, year, treated){
  as.data.frame(r, xy = T) %>%
    rename(hours = layer) %>% 
    mutate(year = year,
           treated = treated) %>% 
    return()
}

# EEZs that have few points and are therefore excluded from the analyses and counted as HS
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
  rbind(data.frame(id = "HS", source = "HS", PNA = 0, country = "HS", unique = 0))

regions_raster_df <- as.data.frame(regions_raster, xy = T) %>% 
  left_join(regions_with_HS, by = c("layer" = "unique"))

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds")) %>% 
  filter(gear == "purse_seines",
         year < 2018,
         fishing) %>% 
  mutate(treated = ifelse(treated, "Treated", "Control"))

# Vector of dates to iterate over
years <- 2012:2017
# Vector of groups to iterate over
groups <- c("Treated", "Control")

# iniialize empty df
yearly_effort_raster <- data.frame(x = NULL,
                                   y = NULL,
                                   hours = NULL,
                                   year = NULL,
                                   treated = NULL)

for(j in groups){
  for(i in years){
    # Rasterize the points for month i
    raster_j_i <- vessel_tracks %>% 
      filter(year == i,
             treated == j) %>% 
      rasterize_df(r = regions_raster)
    
    # Export raster in case this thing breaks
    writeRaster(x = raster_j_i,
                filename = here::here("data",
                                      "spatial",
                                      "yearly_rasterized_effort_by_region",
                                      paste0(j, "_", i, ".tif")),
                overwrite = TRUE)
    
    # Convert raster into data.frame
    raster_j_i_df <- raster_j_i %>% 
      extract_raster(year = i,
                     treated = j) %>% 
      drop_na(hours)
    
    # Allow data.frame to grow so that it has 72 months times nrow times ncol in the raster
    yearly_effort_raster <- rbind(yearly_effort_raster, raster_j_i_df)
  }
}

# Combine the rasterized dataframe with the regions to match them
yearly_effort_raster <- yearly_effort_raster %>% 
  left_join(regions_raster_df, by = c("x", "y")) %>%
  group_by(year, treated) %>% 
  mutate(max_hours = max(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_norm = hours / max_hours)

saveRDS(yearly_effort_raster, file = here::here("data", "rasterized_effort_by_region.rds"))



















  




