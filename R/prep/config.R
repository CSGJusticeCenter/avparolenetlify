#######################################
# Project: AV Parole
# File: config.R
# Authors: Mari Roberts
# Date last updated: September 17, 2024 (MAR)
# Description:
#    This script is responsible for setting up the environment for the AV Parole project.
#    It includes the following tasks:
#    1. Selecting the year for analysis.
#    2. Setting the project path and data paths.
#    3. Providing installation instructions for critical packages.
#    4. Loading all necessary R packages.
#    5. Adding custom fonts.
#    6. Configuring Highcharter options for data visualization.
#    7. Defining color schemes.
#
#    Usage:
#    - This script should be sourced at the beginning of your analysis scripts to ensure
#      all packages and settings are correctly initialized.
#
#    Note:
#    - Make sure to update the project and SharePoint paths as needed.
#    - Follow the installation instructions for any packages that are not already installed
#      on your system.
#######################################

#------ Package Installation Instructions ------#

# Uncomment and run the following lines to install necessary packages if not already installed.
# Install remotes package to install CSGJCR package
# install.packages("remotes")
# library("remotes")
# remotes::install_github("CSGJCResearch/csgjcr")

# Remove the existing highcharter package, restart your R session, and install it with devtools
# remove.packages("highcharter")
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

#------ Load Packages ------#

required_packages <- c(
  "csgjcr", "dplyr", "ggplot2", "janitor", "tidyverse", "highcharter",
  "reactable", "reactablefmtr", "sysfonts", "extrafont", "showtext", "htmlwidgets",
  "htmltools", "sf", "jsonlite", "geojsonsf", "openxlsx", "broom",
  "broom.helpers", "sjPlot", "rmarkdown", "cowplot", "jsonlite",
  "ggtext", "scales", "base64enc", "glue", "haven", "png", "reshape2", "magick"
)

lapply(required_packages, library, character.only = TRUE)

#------ Configuration ------#

# Set project path
csg_set_project_path(
  project = "AVParole",
  sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole",
  force = TRUE
)

# Save Sharepoint data path
config <- list(
  sp_data_path = csg_get_project_path("AVParole")
)

# Save Sharepoint data analysis and deliverables folder
app_folder <- file.path(config$sp_data_path, "data", "analysis", "app")
deliverables_folder <- file.path(config$sp_data_path, "data", "deliverables", "key_findings")

# Most recent year of NCRP data
select_year <- 2020

# Choose alignment for content (left or center)
# For now we like things centered
alignment <- "center"

#------ Fonts ------#

# Add custom fonts
font_add("Graphik",     regular = "fonts/Graphik.ttf")
font_add("GraphikBold", regular = "fonts/GraphikBold.ttf")

#------ Highcharter Options ------#

# Set options so that y axis has comma separator on highcharts
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

#------ Colors ------#

# Colors by Eleventy
blue <- "#55b4e5"
teal <- "#49a7a1"
lightteal <- "#b1d4d5"
yellow <-  "#decf64"
red <- "#d97d68"
purple <- "#938ebf"
brown <- "#9e6c10"
color1 <- red
color2 <- blue
color3 <- yellow
color4 <- teal
color5 <- purple
color6 <- lightteal
green1    = "#b1d4d5"
green2    = "#49a7a1"
green3    = "#176f6d"
green4    = "#104040"

# DARKER COLORS
# teal <- "#176f6d"
# yellow <-  "#decf64"
# brown <- "#9e6c10"
# blue <- "#2a5a99"
# red <- "#ac532f"
# purple <- "#948ebf"
# green1    = "#b1d4d5"
# green2    = "#49a7a1"
# green3    = "#176f6d"
# green4    = "#104040"
# color1 <- red
# color2 <- blue
# color3 <- brown
# color4 <- teal
# color5 <- purple

darkgray <- "#969696"
lightgray <- "#d7d7d7"

