#######################################
# Project: AV Parole
# File: config.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
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
  "ggtext", "scales"
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

# Most recent year of NCRP data
analysis_year <- 2020


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

# Michael colors
# colors <- list(
#   lightgray = "#d7d7d7",
#   darkgray  = "#969696",
#   purple    = "#938ebf", # primary color
#   red       = "#d46c55", # primary color
#   blue      = "#55b4e5", # primary color
#   yellow    = "#decf64", # primary color
#   green1    = "#b1d4d5",
#   green2    = "#49a7a1",
#   green3    = "#176f6d", # primary color
#   green4    = "#104040",
#   brown     = "#9e6c10"  # primary color
# )

colors <- list(
  lightgray = "#d7d7d7",
  darkgray  = "#969696",
  purple    = "#938ebf", # primary color
  red       = "#F05039", # primary color
  blue      = "#1F449C", # primary color
  yellow    = "#decf64", # primary color
  green1    = "#b1d4d5",
  green2    = "#49a7a1",
  green3    = "#176f6d", # primary color
  green4    = "#104040",
  brown     = "#9e6c10"  # primary color
)


# Usage Example:
# ggplot(data, aes(x, y)) +
#   geom_line(color = colors$blue)
