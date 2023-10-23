############################################
# Project: AV Parole Project
# File: generate_state_reports.R
# Date last updated: October 20, 2023 (MAR)
# Author: Mari Roberts

# Generate html documents for each state
# based on _state_report_template.qmd
############################################

# Select year for analysis
select_year <- 2020

# There are two options for running this code. Scroll to your desired section.
# OPTION 1: To run all code and generate data and visualizations (takes time)
# OPTION 2: Render the site using saved data and visualizations (faster)



# OPTION 1: To run all code and generate data and visualizations (takes time)
#-------------------------------------------------------------------------------

# Add logs to see how the code is run
sink("logs.txt", append = TRUE, split = TRUE)

# Load packages and functions for generating HTMLs from QMDs
source("prep/library.R") # ACTION REQUIRED BY NEW USER
source("prep/functions.R")

# Load BJS, NCRP and APS data
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race_state_2020.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_state.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/aps_parole_2000_2018.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race.rds"))


# Run code to generate visualizations and tables for app
source("prep/page_national_trends.R")
source("prep/tab_parole_eligibility.R")
source("prep/tab_prison_population.R")
source("prep/tab_releases.R")
source("prep/tab_disparities.R")

# Save working directory
wd <- getwd()

# Get list of 50 states
states <- c("Georgia")
# states <- state.name

# Read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")

# Replace state name
states_qmd <- as.character(states)

# Function to replace place-holder text in orig qmd with our replacement values (state names)
#    and write out to qmd - name of qmd should include state name
replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_") # replace spaces with underscores
  str_replace(orig_qmd, "change_me", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# Iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)

# Render all qmds - takes ~5 minutes
quarto::quarto_render()

# Reset output to the console
sink()




# OPTION 2: Render the site using saved data and visualizations (faster)
#-------------------------------------------------------------------------------

# Add logs to see how the code is run
sink("logs.txt", append = TRUE, split = TRUE)

# Generate the site using saved data and visualizations:
source("prep/library.R") # ACTION REQUIRED BY NEW USER
source("prep/functions.R")
source("prep/dataframes.R")

# Render all qmds - takes ~5 minutes
quarto::quarto_render()

# Reset output to the console
sink()



