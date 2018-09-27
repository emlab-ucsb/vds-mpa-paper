library(startR)
library(magrittr)
library(tidyverse)

st_rotate <- function(x){
  x2 <- (st_geometry(x) + c(360,90)) %% c(360) - c(0,90) 
  x3 <- st_wrap_dateline(st_set_crs(x2 - c(180,0), 4326)) + c(180,0)
  x4 <- st_set_crs(x3, 4326)
  
  x <- st_set_geometry(x, x4)
  
  return(x)
}

vessel_tracks <- readRDS(file = here::here("raw_data", "vessel_tracks.rds"))

country_groups <- c("COK", "FSM", "KIR", "MHL", "NRU", "PNG", "SLB", "TKL", "TUV")

eez <- read_sf(dsn = here::here("raw_data", "spatial", "EEZ"), layer = "eez_v10") %>% 
  filter(ISO_Ter1 %in% unique(vessel_tracks$eez_iso3)) %>% 
  mutate(PNA = ISO_Ter1 %in% c("FSM", "KIR", "MHL", "NRU", "PLW", "PNG", "SLB", "TUV")) %>% 
  st_rotate()

pipa <- read_sf(dsn = here::here("data", "spatial", "PIPA"),
                layer = "PIPA") %>% 
  mutate(inside = T) %>% 
  select(inside) %>% 
  st_rotate()
# Cuales estan dentro de PIPA

ps <- vessel_tracks %>% 
  filter(gear == "purse_seines",
         treated,
         fishing) %>% 
  st_as_sf(coords = c(7, 8), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_rotate() %>% 
  st_join(pipa) %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_set_geometry(value = NULL) %>%
  mutate(country = ifelse(eez_iso3 %in% country_groups, eez_iso3, "others"),
         country = ifelse(is.na(inside), country, "PIPA")) %>% 
  group_by(year, month) %>% 
  mutate(total_hours = sum(hours)) %>% 
  ungroup() %>% 
  group_by(year, month, country, total_hours) %>%
  summarize(h = sum(hours)) %>% 
  mutate(h_prop = h / total_hours) %>% 
  ungroup()

## Corregir los NA que deberian de unirse (sonhigh seas) del codigo de arriba al de abajo

displacement_data <- expand.grid(country = c(unique(ps$country), NA),
                                 year = 2012:2017,
                                 month = 1:12) %>% 
  arrange(country, year, month) %>%
  as.tibble() %>%
  left_join(ps, by = c("country", "year", "month")) %>% 
  mutate(h_prop = ifelse(is.na(h_prop), 0, h_prop)) %>% 
  mutate(date = lubridate::date(paste(year, month, 1, sep = "/")),
         country = fct_relevel(country, c("PIPA", "KIR")),
         country = fct_relevel(country, "others", after = Inf),
         post = year >= 2015)

ba_disp <- displacement_data %>% 
  group_by(country, post) %>% 
  summarize(h_prop_m = mean(h_prop)) %>% 
  spread(post, h_prop_m) %>% 
  mutate(change = (`TRUE` - `FALSE`) * 100) %>% 
  select(country, change)

# las lineas antes y despues deben de venir de ba_disp, no de geom_smooth
displacement_data %>% 
  ggplot(aes(x = date, y = h_prop, color = country, group = post)) +
  geom_smooth(method = "lm") +
  geom_point() +
  geom_vline(xintercept = lubridate::date(paste(2015, 1, 1, sep = "/")), linetype = "dashed") +
  scale_fill_brewer(palette = "Paired") +
  ggtheme_plot() +
  facet_wrap(~country, scales = "free_y")

displacement_data %>% 
  filter(!is.na(country)) %>% 
  lm(h_prop ~ post*country, data = .) %>% 
  summary()

# Juntar PIPA y EEZ en una solo shapefile
# Agregar poligono de "Others" y de High Seas

pipa_map_ba_disp <- pipa %>% 
  mutate(country = "PIPA") %>% 
  left_join(ba_disp, by = "country")

# Los colores del mapa deben estar dados por el Post:Country
eez %>% 
  left_join(ba_disp, by = c("ISO_Ter1" = "country")) %>%
  filter(!is.na(change)) %>% 
  ggplot() +
  geom_sf(aes(fill = change, linetype = PNA), size = 1, color = "black") +
  geom_sf(data = pipa_map_ba_disp, aes(fill = change), linetype = "dashed", color = "red") +
  ggtheme_plot() +
  scale_fill_gradientn(colors = colorRamps::matlab.like(10))













