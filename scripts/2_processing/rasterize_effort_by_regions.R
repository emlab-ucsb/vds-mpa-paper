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
extract_raster <- function(r, year, treated){
  as.data.frame(r, xy = T) %>%
    rename(hours = layer) %>% 
    mutate(year = year,
           treated = treated)
}

#### REGIONS RASTER ######################################################################
# Load the regions shapefile
regions <- read_sf(dsn = here::here("data", "spatial", "regions"),
                   layer = "regions")

# Rasterize regions
regions_raster <- regions %>%
  mutate(unique = group_indices(., id)) %>%
  fasterize(sf = .,
            raster = raster(., res = 1),
            field = "unique",
            background = 0)

# Add the High seas manually
regions_with_HS <- regions %>%
  mutate(unique = group_indices(., id)) %>% 
  st_set_geometry(NULL) %>% 
  rbind(data.frame(id = "HS",
                   source = "HS",
                   PNA = 0,
                   country = "HS",
                   unique = 0))

regions_raster_df <- as.data.frame(regions_raster, xy = T) %>% 
  left_join(regions_with_HS, by = c("layer" = "unique"))

vessel_tracks <- readRDS(file = here::here("data", "vessel_tracks_baci.rds")) %>% 
  filter(gear == "tuna_purse_seines",
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



#### CREATE GROUP - YEARLY LEVEL RASTERS OF EFFORT ###################################

# ITERATE ACROSS GROUPS
for(j in groups){
  
  # ITERATE ACROSS YEARS
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



















  




