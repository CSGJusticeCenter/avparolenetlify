library(logger)

# Configure logging
log_appender(appender_tee("script_log.log"))
log_threshold(INFO)

# STEP 1) Load configuration settings and paths
log_info("Loading configuration settings and paths...")
source("R/config.R")

# STEP 2) Import data and helper functions

log_info("Importing helper functions for data processing and formatting...")
source("R/prep/import/helper_functions_import.R")
log_info("Helper functions imported successfully.")

log_info("Importing and formatting data...")
source("R/prep/import/import_format.R")
log_info("Data imported and formatted successfully.")

log_info("Importing custom analysis helper functions...")
source("R/prep/analysis/helper_functions.R")
log_info("Helper functions imported successfully.")

# Step 3) Run R files in this order
log_info("Starting analysis scripts...")
scripts <- c(
  "R/prep/analysis/page_national_trends.R",
  "R/prep/analysis/tab_parole_eligibility.R",
  "R/prep/analysis/tab_population.R",
  "R/prep/analysis/tab_releases.R",
  "R/prep/analysis/tab_disparities.R",
  "R/prep/analysis/tab_disparities_rris.R"
)

for (script in scripts) {
  log_info("Running script: {script}")
  tryCatch(
    {
      source(script)
      log_success("Successfully completed: {script}")
    },
    error = function(e) {
      log_error("Error in {script}: {e$message}")
    }
  )
}

# Step 4) Generate Quartos for state reports
log_info("Generating state-specific Quarto reports...")

fnc_replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_")
  log_debug("Processing state: {state} -> {cleaned_state}")
  str_replace(orig_qmd, "this_state", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# Save the current working directory
wd <- getwd()
log_debug("Working directory: {wd}")

log_info("Loading data...")
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_national_page_only.rds"))

log_info("Filtering states for reports...")
states <- parole_eligibility_table |>
  filter(!state %in% states_to_exclude$state) |>
  filter(!state %in% states_national_page_only$state) |>
  pull(state)
# states <- c("Arkansas", "Georgia", "Colorado", "Hawaii", "Louisiana")

log_info("Reading Quarto template...")
orig_qmd <- read_lines("_state_report_template.qmd")

log_info("Generating QMD files for {length(states)} states...")
walk(states, fnc_replace_write_qmd)

# Step 5) Render all pages and launch site - takes ~15 min to run
log_info("Rendering all Quarto pages...")

quarto::quarto_render()
log_success("Quarto rendering completed successfully.")
log_info("Script completed.")
