###################################
#   rasterize_effort_by_regions   #
###################################


######################################################################
# This script takes the shapefile of EEZs and rasterizes it to a 1 deg
# raster. I then create a yearly raster for each group of vessels, 
# which I export as tiff.
######################################################################


#### SETUP ######################################################################

# Load packages
library(raster)
library(fasterize)
library(sf)
library(tidyverse)

# Create some functions that I only use here
# This one converts a data.frame into a raster
rasterize_df <- function(x, r, fun = "sum"){
  
  # First I "rotate" to center the image to the Pacific
  x2 <- x %>%
    select(lon, lat, hours) %>% 
    mutate(lon = ifelse(lon < 0, lon + 180, lon - 180) + 180) %>% 
    as.matrix()
  
  # I know rasterize the data
  rasterize(x = x2[,1:2],
            y = r,
            field = x2[,3],
            fun = fun)
}

# This function extracts the raster and converts it into a data.frame
# I need it because I want to convert it to DF and add some metadata 
# about the raster, like year and group to which vessels belong
extract_raster <- function(r, year, month = NULL, treated){
  as.data.frame(r, xy = T) %>%
    rename(hours = layer) %>% 
    mutate(year = year,
           month = month,
           treated = treated)
}

#### REGIONS RASTER ######################################################################

# Read my regions shapefile
regions <- read_sf(dsn = here::here("data", "spatial", "regions"),
                   layer = "regions")

# Rasterize regions shapefile
regions_raster <- regions %>%
  mutate(unique = group_indices(., id)) %>%
  fasterize(sf = .,
            raster = raster(., res = 1),
            field = "unique",
            background = 0)

# Export raster to include in Appendix
writeRaster(x = regions_raster,
            file = here::here("data",
                              "spatial",
                              "regions_raster.tif"),
            overwrite=TRUE)

# Add a HS term and convert into a data.frame used later
regions_with_HS <- regions %>%
  mutate(unique = group_indices(., id)) %>% 
  st_set_geometry(NULL) %>% 
  rbind(data.frame(id = "HS HS 1",
                   source = "HS",
                   PNA = 0,
                   country = "HS",
                   unique = 0)
        )

# This data.frame has columns for region ids and the corresponding raster values (HS is 0)
regions_raster_df <- as.data.frame(regions_raster, xy = T) %>% 
  left_join(regions_with_HS, by = c("layer" = "unique"))

# Read in the entire vessel tracks
monthly_vessel_tracks <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(gear == "tuna_purse_seines",
         year < 2018,
         fishing) %>% 
  drop_na(hours, fishing) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")),
         treated = ifelse(treated, "Treated", "Control"))

# Vector of dates to iterate over
dates <- sort(unique(monthly_vessel_tracks$date))
# Vector of groups to iterate over
groups <- c("Treated", "Control")

# iniialize empty df
monthly_effort_raster <- data.frame(x = NULL,
                                    y = NULL,
                                    treated = NULL,
                                    hours = NULL,
                                    year = NULL,
                                    month = NULL)
for(j in groups){
  for(i in 1:length(dates)){
    # Rasterize the points for month i
    raster_j_i <- monthly_vessel_tracks %>% 
      filter(date == dates[i],
             treated == j) %>% 
      rasterize_df(r = regions_raster)
    
    # Export raster in case this thing breaks
    writeRaster(x = raster_j_i,
                filename = here::here("data",
                                      "spatial",
                                      "monthly_rasterized_effort_by_region",
                                      paste0(j, "_", dates[i], ".tif")),
                overwrite = TRUE)
    
    # Convert raster into data.frame
    raster_j_i_df <- raster_j_i %>% 
      extract_raster(year = lubridate::year(dates[i]),
                     month = lubridate::month(dates[i]),
                     treated = j) %>% 
      drop_na(hours)
    
    # Allow data.frame to grow so that it has 72 months times nrow times ncol in the raster
    monthly_effort_raster <- rbind(monthly_effort_raster, raster_j_i_df)
  }
}

# Combine the rasterized dataframe with the regions to match them
monthly_effort_raster <- monthly_effort_raster %>% 
  left_join(regions_raster_df, by = c("x", "y")) %>%
  group_by(year, month, treated) %>% 
  mutate(max_hours = max(hours, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hours_norm = hours / max_hours) %>% 
  drop_na() %>%
  mutate(date = lubridate::date(paste(year, month, 1, sep = "-")),
         date_c = as.factor(date),
         year_c = as.factor(year),
         month_c = as.factor(month),
         quarter = as.factor(lubridate::quarter(date, with_year = T)))

saveRDS(monthly_effort_raster, file = here::here("data", "monthly_rasterized_effort_by_region.rds"))









































