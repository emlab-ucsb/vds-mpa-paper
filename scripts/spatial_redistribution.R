library(startR)
library(magrittr)
library(sf)
library(tidyverse)

ba_disp <- displacement_data %>% 
  group_by(country, post) %>% 
  summarize(h_prop_m = mean(h_prop)) %>% 
  spread(post, h_prop_m) %>% 
  mutate(change = (`TRUE` - `FALSE`) * 100) %>% 
  select(country, change)

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













