#######################################
# Project: AV Parole
# File: config.R
# Authors: Mari Roberts
# Date last updated: July 18, 2024 (MAR)
# Description:
#    This script is responsible for setting up the environment for the AV Parole project.
#    It includes the following tasks:
#    1. Selecting the year for analysis.
#    2. Setting the project path and data path.
#    3. Providing installation instructions for critical packages.
#    4. Loading all necessary R packages.
#    5. Adding custom fonts for visual consistency in plots.
#    6. Configuring Highcharter options for data visualization.
#    7. Defining color schemes for consistent and visually appealing graphics.
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
  "ggtext", "scales", "base64enc", "shadowtext", "leaflet", "tidycensus", "sf",
  "tigris", "stringr", "readxl"
)

lapply(required_packages, library, character.only = TRUE)



#------ Configuration ------#

# Set project path
csg_set_project_path(
  project = "JRWV",
  sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - Documents/JR_WV",
  force = TRUE
)

# Save Sharepoint data path
config <- list(
  sp_data_path = csg_get_project_path("JRWV")
)

# Sharepoint save location
savefolder <- paste0(config$sp_data_path, "/data/analysis/")

# JRI official colors
lightgray <- "#d0cece"

darkblue  <- "#263C4B"
blue      <- "#3F95B0"
lightblue <- "#d7ebf1"

lightgreen <- "#dbedde"
green     <- "#50A25D"
darkgreen <- "#28512f"

darkred <- "#811d14"
red       <- "#e25448"
lightred <- "#f9ddda"

lightorange <- "#f9e4d6"
orange    <- "#E17630"
darkorange <- "#773a11"


# Load Franklin Gothic Book - JRI official font
font_add(family  = "Franklin Gothic Book",
         regular = "FRABK.ttf",
         italic  = "FRABKIT.ttf",
         bold    = "FRADM.ttf")
showtext_auto()

