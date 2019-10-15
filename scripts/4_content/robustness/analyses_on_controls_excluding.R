#######################################
#   analyses_on_controls_excluding    #
#######################################

###################################################
# This is the script that fits all the models for
# my panels of behavioral change for ADDITIONAL CONTROLS
# when I:
# - Exclude Chinese vessels
# - Exclude PNS-owned vessels
# - Exclude TWN and USA vessels (american-american)
# - Exclude all of the above
# The script is simply a wrapper to call each of the
# four different scripts that fit the models for the
# abovementioned controls
###################################################

# Load libraries
library(here)

# Analysis without chinese vessels
source(here("scripts", "content", "robustness", "analyses_on_controls_excluding_CHN.R"))

# Analysis without PNA vessels
source(here("scripts", "content", "robustness", "analyses_on_controls_excluding_PNA.R"))

# Analysis without USA or TWN vessels
source(here("scripts", "content", "robustness", "analyses_on_controls_excluding_USA_TWN.R"))