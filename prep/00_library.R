#######################################
# Project: AV Parole
# File: library.R
# Authors: Mari Roberts
# Date last updated: May 5, 2023 (MAR)
# Description:
#    Load packages and custom functions
#######################################

##########
# Packages
##########

# download CSGJCR package
# devtools::install_github("CSGJusticeCenter/csgjcr@DEVELOP")
library(csgjcr)

# Highcharter download instructions:
# remove the existing highcharter package from your R session: remove.packages("highcharter")
# restart your R session
# install highcharter with the devtools package (NOT the remotes package):
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

# load other packages
library(dplyr)
library(ggplot2)
library(janitor)
library(highcharter)
library(tidyverse)
library(reactable)
library(sysfonts)
library(extrafont)
library(showtext)
library(htmlwidgets)


# CHANGE THIS TO YOUR PROJECT PATH
csg_set_project_path(
  project = "AVParole",
  sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - RES_Parole",
  force = TRUE)

# Save data path
sp_data_path <- csg_get_project_path("AVParole")

# Load fonts
font_add("Graphik",     regular = "fonts/Graphik.ttf")
font_add("GraphikBold", regular = "fonts/GraphikBold.ttf")

###################
# Colors
###################

# neutral colors
# neutralBkgndLight    <- "#F4F6F6"
neutralBkgndLight    <- "#e7e7e7"
neutralBkgndMedium   <- "#DADFDF"
neutralBkgndDisabled <- "#B2B9B9"
neutralDarkSubText   <- "#637070"
neutralBlackText     <- "#3E4B4B"

# https://community.holistics.io/t/launched-more-accessible-and-modern-color-palettes/746
# contrasting palette 1
blue     <- "#0061e4"
teal     <- "#00aba0"
darkblue <- "#003474"
purple   <- "#c376fb"
red      <- "#c60040"
yellow   <- "#ffaf00"
orange   <- "#ff6400"
