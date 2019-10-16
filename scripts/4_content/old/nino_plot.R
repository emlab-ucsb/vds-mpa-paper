####################
##    nino_plot   ##
####################

############################################################
# Create a map of the NINO4 region, and timeseries of the
# nino4 and nino4 anomaly indices. This is a suplpementary
# figure.
############################################################


# Load packages
library(startR)
library(lubridate)
library(cowplot)
library(rnaturalearth)
library(sf)
library(here)
library(tidyverse)

source(here("scripts", "functions", "st_rotate.R"))

# Load data
# Coastline
coast <- map_data('world', wrap=c(-20,340), ylim=c(-85,75))%>%
  select(group, lat, long) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>% 
  summarize(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  mutate(a = 1) %>% 
  group_by(a) %>% 
  summarize() %>% 
  ungroup() %>% 
  st_simplify()

# Polygon of NINO4 region (5S-5N and 160E-150W), rotated
nino4_region <- tibble(lon = c(160, 210, 210, 160),
                       lat = c(5, 5, -5, -5))

# NINO4 indices
nino4 <- read.csv(here("data", "all_indices.csv")) %>% 
  filter(year > 1980) %>% 
  select(year, month = month_n, nino4, nino4anom) %>% 
  mutate(date = date(paste(year, month, 1, sep = "-")))

map <- ggplot() +
  geom_polygon(data = nino4_region,
               mapping = aes(x = lon, y = lat),
               fill = "transparent",
               color = "red",
               size = 1) +
  geom_sf(data = coast,
          fill = "#E3E3E3",
          color = "black",
          size = 0.1) +
  ggtheme_map()

nino4anom_plot <- ggplot(data = nino4,
       aes(x = date, y = nino4anom)) +
  geom_rect(aes(xmin = date("2012-01-01"),
                xmax = date("2018-12-01"),
                ymin = -Inf, ymax = Inf),
            fill = "lightgray") +
  geom_line(size = 1, aes(color = nino4anom)) +
  geom_vline(xintercept = date("2015-01-01"),
             linetype = "dashed",
             color = "black",
             size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme(text = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  guides(color = guide_colorbar(title = "NINO4\nanomaly",
                                ticks.colour = "black",
                                frame.colour = "black")) +
  labs(x = "Date", y = "NINO4 anomaly") +
  scale_color_gradientn(colors = colorRamps::matlab.like(10))


nino_plot <- plot_grid(map,
                       nino4anom_plot,
                       ncol = 1,
                       labels = "AUTO")

ggsave(plot = nino_plot,
       file = here("docs", "img", "nino_plot.pdf"),
       width = 5,
       height = 5)

ggsave(plot = nino_plot,
       file = here("docs", "img", "nino_plot.png"),
       width = 5,
       height = 5)






















