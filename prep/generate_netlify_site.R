############################################
# Project: AV Parole Project
# File: generate_state_reports.R
# Last updated: September 26, 2023 (MAR)
# Author: Mari Roberts

# Generate html documents for each state
# based on _state_report_template.qmd
############################################

# load packages for generating state report QMDs
library(rmarkdown)
library(tidyverse)

# run code
source("prep/library.R")
source("prep/functions.R")
# source("prep/import.R")
# source("prep/dataframes.R")
# source("prep/page_national_trends.R")
# source("prep/tab_parole_eligibility.R")
# source("prep/tab_prison_population.R")
# source("prep/tab_releases_from_prison.R")
# source("prep/tab_disparities.R")
# source("prep/page_missing_data.R")
# source("prep/rri.R")


# save working directory
wd <- getwd()

# get list of 50 states
# states <- c("Georgia")
states <- state.name

# read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")

# replacement values
states_qmd <- as.character(states)

# function to replace place-holder text in orig qmd with our replacement values
# and write out to qmd - name of qmd should include replacement value
replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_") # replace spaces with underscores
  str_replace(orig_qmd, "change_me", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)

# # render all QMDs
# quarto::quarto_render()
