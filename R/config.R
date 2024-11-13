#######################################
# Project: AV Parole
# File: config.R
# Authors: Mari Roberts
# Date last updated: November 12, 2024 (MAR)
# Description:
#    This script is responsible for setting up the environment for the AV Parole project.
#    It includes the following tasks:
#    1. Selecting the year for analysis.
#    2. Setting the project path and data paths.
#    3. Providing installation instructions for R packages.
#    4. Loading all necessary R packages.
#    5. Adding custom fonts.
#    6. Configuring Highcharter options for data visualization.
#    7. Defining color schemes.
#
#    Usage:
#    - This script should be sourced at the beginning of your analysis scripts to ensure
#      all packages and settings are correctly initialized.
#
#    [ACTION REQUIRED]:
#    - Make sure to update the project and SharePoint paths as needed.
#    - Follow the installation instructions for any packages that are not already installed
#      on your system.
#######################################

#------------------------------------------------------------------------------#
# Package Installation Instructions
#------------------------------------------------------------------------------#

# [ACTION REQUIRED]
# Uncomment and run the following lines to install necessary packages if not already installed.
# Install remotes package to install CSGJCR package
# install.packages("remotes")
# library("remotes")
# remotes::install_github("CSGJCResearch/csgjcr")

# [ACTION REQUIRED]
# This version of highcharter has accessibility features that work
# Remove the existing highcharter package, restart your R session, and install it with devtools
# remove.packages("highcharter")
# install.packages("devtools")
# library(devtools)
# devtools::install_github("mrjoh3/highcharter")

#------------------------------------------------------------------------------#
# Load Packages
#------------------------------------------------------------------------------#

# [ACTION REQUIRED] Install packages if needed
required_packages <- c(
  "csgjcr", "dplyr", "ggplot2", "janitor", "tidyverse", "highcharter",
  "reactable", "reactablefmtr", "sysfonts", "extrafont", "showtext", "htmlwidgets",
  "htmltools", "sf", "jsonlite", "geojsonsf", "openxlsx", "broom",
  "broom.helpers", "sjPlot", "rmarkdown", "cowplot", "jsonlite",
  "ggtext", "scales", "base64enc", "glue", "haven", "png", "reshape2", "magick",
  "downloadthis", "readxl"
)

lapply(required_packages, library, character.only = TRUE)

#------------------------------------------------------------------------------#
# Configurations
#------------------------------------------------------------------------------#

# Set project path
# [ACTION REQUIRED] Change this to your project path
csg_set_project_path(
  project = "AVParole",
  sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole",
  force = TRUE
)

# Save Sharepoint data path
sp_data_path <- csg_get_project_path("AVParole")

# Save Sharepoint data analysis and deliverables folder
app_folder <- file.path(sp_data_path, "data", "analysis", "app")
deliverables_folder <- file.path(sp_data_path, "data", "deliverables", "key_findings")

# # Select years for analysis using NCRP data and BJS data
# select_year <- 2019
#
# # Year for population data (NCRP)
# pop_select_year <- 2019
#
# # Year for population data (BJS)
# bjs_data_year <- 2019

#------------------------------------------------------------------------------#
# Fonts
#------------------------------------------------------------------------------#

# Add custom fonts
font_add("Graphik",     regular = "fonts/Graphik.ttf")
font_add("GraphikBold", regular = "fonts/GraphikBold.ttf")



#------------------------------------------------------------------------------#
# Highcharter Options
#------------------------------------------------------------------------------#

# Set options so that y axis has comma separator on highcharts
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)



#------------------------------------------------------------------------------#
# Colors
#------------------------------------------------------------------------------#

# Colors by Eleventy
blue <- "#55b4e5"
teal <- "#49a7a1"
lightteal <- "#b1d4d5"
yellow <-  "#decf64"
red <- "#d97d68"
purple <- "#938ebf"
brown <- "#9e6c10"

# Assign numbers so colors can be changed universally
color1 <- red
color2 <- blue
color3 <- yellow
color4 <- teal
color5 <- purple
color6 <- lightteal

# Gradient for map
green1    = "#b1d4d5"
green2    = "#49a7a1"
green3    = "#176f6d"
green4    = "#104040"

# Gray colors
darkgray <- "#969696"
lightgray <- "#d7d7d7"



#------------------------------------------------------------------------------#
# Sources
#------------------------------------------------------------------------------#

# Format sources
ncrp_csg_source <- "National Corrections Reporting Program and CSG Justice Center estimates"
ncrp_source     <- "National Corrections Reporting Program"
bjs_source      <- "BJS Prisoners in the United States"

