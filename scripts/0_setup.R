##################
## setup_script ##
##################

##############################################
# This script should set up all the necesary
# R packages for you to reproduce our work
##############################################

# Install packages from CRAN
install.packages("here")        # For path management
install.packages("tidyverse")   # For data wrangling
install.packages("sf")          # For dealing with simple feature geometries (spatial data)
install.packages("raster")      # For dealing with raster (gridded) data
install.packages("rmapshaper")  # For simplifying overly-complex spatial geometries
install.packages("countrycode") # To deal with country names and ISO3 codes
install.packages("cowplot")     # To create panel figures
install.packages("furrr")       # For parallell functional programming (iterate across scenarios faster)
install.packages("ggrepel")     # To repel labels
install.packages("ggridges")    # To create ridgeplots (fancy density plots)
install.packages("janitor")     # Has useful data-cleaning functions
install.packages("knitr")       # To transform data.frames into latex tables


# Other dependencies
remotes::install_github("jcvdav/startR")                 # Personal package with helper functions
remotes::install_github("yutannihilation/ggsflabel")     # To repel labels in spatial objects
remotes::install_github("GlobalFishingWatch/fishwatchr") # To use GFW color palettes (may need special authorization)

# END OF SCRIPT