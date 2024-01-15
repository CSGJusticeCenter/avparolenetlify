#######################################
# Project: AV Parole
# File: library.R
# Authors: Mari Roberts
# Date last updated: October 31, 2023 (MAR)
# Description:
#    Load packages, colors, fonts
#######################################

#------ Action Required ------#

# select year for analysis
select_year <- 2020

# csgjcr installation instructions:
# Install remotes package in order to install CSGJCR package
# uncomment the three lines of code below for installation.
# install.packages("remotes")
# library("remotes")
# remotes::install_github("CSGJCResearch/csgjcr")
# In your Renviron (usethis::edit_r_environ(), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token"
library(csgjcr)

# highcharter installation instructions:
# remove the existing highcharter package from your R session: remove.packages("highcharter")
# restart your R session
# install highcharter with the devtools package (NOT the remotes package):
# install.packages("devtools")
# devtools::install_github("mrjoh3/highcharter")

# change the sp_folder to your project path:
csg_set_project_path(
  project = "AVParole",
  sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole",
  force = TRUE)

# save data path:
sp_data_path <- csg_get_project_path("AVParole")

# ----------------------------#

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
library(htmltools)
library(sf)
library(jsonlite)
library(geojsonsf)
library(openxlsx)
library(broom)
library(broom.helpers)
library(sjPlot) # missing data
library(rmarkdown) # render pages

# load fonts
font_add("Graphik",     regular = "fonts/Graphik.ttf")
font_add("GraphikBold", regular = "fonts/GraphikBold.ttf")

# set options so that y axis has comma separator on highcharts
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


###################
# Colors
###################

# neutral colors
neutralBkgndLight    <- "#e7e7e7"
neutralBkgndMedium   <- "#DADFDF"
neutralBkgndDisabled <- "#B2B9B9"
neutralDarkSubText   <- "#637070"
neutralBlackText     <- "#3E4B4B"

# official colors
darkblue    <- "#001e36"
blue        <- "#0f5a9e"
teal        <- "#00aba0"
yellow      <- "#f8ac00"
red         <- "#7b3014"
orange      <- "#D25E2D"
purple      <- "#6f4e7b"



