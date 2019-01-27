###############################
#   Map of Large-scale MPAs   #
###############################

##############################################################
# This script creates a map of all large-scale MPAs until now.
##############################################################


# Load packages
library(startR)
library(sf)
library(tidyverse)

# Source local functions
source(here::here("scripts", "functions", "st_rotate.R"))
source(here::here("scripts", "functions", "sfc_as_cols.R"))


mpas <- read_sf(dsn = here::here("data", "spatial", "LSMPAs"), layer = "LSMPAs") %>% 
  janitor::clean_names() %>% 
  filter(!wdpaid %in% c(555512002, 555512001)) %>% 
  st_transform(crs = 4326) %>% 
  st_rotate() %>%
  mutate(strict = ifelse(no_tk_area > 0 |
                           iucn_cat %in% c("Ia", "Ib") |
                           desig_eng == "Protected Area",
                         "No-Take", "Others"),
         strict = ifelse(is.na(strict), "Others", strict),
         strict = ifelse(wdpaid == 309888, "PIPA", strict),
         Legend = fct_relevel(strict, c("No-Take", "PIPA", "Others"))) %>% 
  filter(!name == "Longline")

coast <- map_data('world', wrap=c(-20,340), ylim=c(-85,75))%>%
  select(group, lat, long) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  mutate(a = 1) %>% 
  group_by(a) %>% 
  summarize() %>% 
  ungroup()

plot <- ggplot() +
  geom_sf(data = coast, fill = "#E3E3E3") +
  geom_sf(data = mpas, fill = "transparent", color = "black", size = 1) +
  geom_sf(data = mpas, aes(fill = Legend), color = "transparent") +
  ggtheme_map() +
  scale_fill_manual(values = c("steelblue", "red", "steelblue1")) +
  theme(legend.justification = c(0.5, 1),
        legend.position = c(0.5, 0.05),
        legend.box = "horizontal",
        legend.background = element_blank()) +
  guides(fill = guide_legend(ncol = 3))

ggsave(plot, filename = here::here("docs", "img", "LSMPAs_map.png"), width = 6, height = 3.5)
ggsave(plot, filename = here::here("docs", "img", "LSMPAs_map.pdf"), width = 6, height = 3.5)



