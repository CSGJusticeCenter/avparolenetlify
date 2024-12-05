# STEP 1) Load configuration settings and paths
# [ACTION REQUIRED] in this R file - check file
source("R/config.R")

# STEP 2) Import data and helper functions

# Import helper functions for data processing and formatting
source("R/prep/import/helper_functions_import.R")
source("R/prep/import/import_format.R")

# Import custom helper functions for analysis (functions start with 'fnc_')
source("R/prep/analysis/helper_functions.R")

# Step 3) Run R files in this order
source("R/prep/analysis/page_national_trends.R")
source("R/prep/analysis/tab_parole_eligibility.R")
source("R/prep/analysis/tab_population.R")
source("R/prep/analysis/tab_releases.R")
source("R/prep/analysis/tab_disparities.R")
source("R/prep/analysis/tab_disparities_rris.R")

# Step 4) Generate Quartos for state reports

# Function to replace placeholder text in the Quarto Markdown (QMD) template
# Generates state-specific Quarto files for each state
fnc_replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_") # Replace spaces with underscores
  str_replace(orig_qmd, "this_state", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# Save the current working directory for reference
wd <- getwd()

# Load pre-processed data for analysis
# States to exclude from th analysis
# States that should be in the National Snapshot page but not have a report
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_national_page_only.rds"))

# States that need reports
states <- parole_eligibility_table |>
  filter(!state %in% states_to_exclude$state) |>
  filter(!state %in% states_national_page_only$state) |>
  pull(state)

# Read the Quarto template for state reports
orig_qmd <- read_lines("_state_report_template.qmd")

# Generate state-specific QMD files using the template
walk(states, fnc_replace_write_qmd)

# Step 5) Render all pages and launch site

# Render all generated Quarto Markdown (QMD) files
# Note: Ensure Quarto is installed and properly configured before running this
# Takes about 15 minutes to run
quarto::quarto_render()

