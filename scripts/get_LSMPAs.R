library(dplyr)

sf::read_sf(here::here("raw_data", "spatial", "WDPA_Mar2018"),
            layer = "WDPA_Mar2018_marine-shapefile-polygons",
            quiet = T,
            stringsAsFactors = F) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(gis_m_area > 250000) %>% 
  sf::st_write(dsn = here::here("data", "spatial", "LSMPAs", "LSMPAs.shp"))
