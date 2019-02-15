###################
#   get_LSMPAs    #
###################

###############################################
# This script reads in the WDPA and keeps only
# MPAs larger than 250000 in marine area.
##############################################

# Load magrittr to have the pipe
library(magrittr)

# Load the database
sf::read_sf(here::here("raw_data", "spatial", "WDPA_Jan2019"),
            layer = "WDPA_Jan2019_marine-shapefile-polygons",
            quiet = T,
            stringsAsFactors = F) %>% 
  dplyr::filter(GIS_M_AREA > 30000) %>% #Keep large MPAs
  sf::st_write(dsn = here::here("data", "spatial", "LSMPAs", "LSMPAs.shp")) #Save to file

# END OF SCRIPT