############################################
# Project: AV Parole Project
# File: generate_state_reports.R
# Last updated: July 5, 2023
# Author: Mari Roberts

# Generate html documents for each state
# based on _state_report_template.qmd
############################################

# load packages for generating state report QMDs
library(rmarkdown)
library(tidyverse)

# save working directory
wd <- getwd()

# get list of 50 states
states <- state.name

# read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")

# replacement values
states_qmd <- as.character(states)

# function to replace place-holder text in orig qmd with our replacement values
# and write out to qmd - name of qmd should include replacement value
replace_write_qmd <- function(state) {
  str_replace(orig_qmd, "change_me", state) |>
    write_lines(paste0("state_report_", state, ".qmd"))
}

# iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)

# # render all QMDs
# quarto::quarto_render()
