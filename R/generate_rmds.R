
# [ACTION REQUIRED] - check file
source("R/config.R")

# Run import R files
source("R/prep/import/helper_functions_import.R")
source("R/prep/import/import_format.R")

# Run custom helper functions ('fnc_')
source("R/prep/analysis/helper_functions.R")
source("R/prep/analysis/helper_functions_disparities.R")

# Run R files
source("R/prep/analysis/page_national_trends.R")
source("R/prep/analysis/tab_parole_eligibility.R")
source("R/prep/analysis/tab_population.R")
source("R/prep/analysis/tab_releases.R")
source("R/prep/analysis/tab_disparities.R")
source("R/prep/analysis/tab_disparities_rris_past_pe.R")

# Function to replace place-holder text in orig qmd with our replacement values (state names)
#    and write out to qmd - name of qmd should include state name
replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_") # replace spaces with underscores
  str_replace(orig_qmd, "this_state", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# Save working directory
wd <- getwd()

load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))

# Get list of states for reports - only states with parole and complete PE data
states <- c("Georgia", "Hawaii", "Louisiana")
# states <- c("Georgia", "Louisiana", "Connecticut", "Colorado", "Michigan")
# states <- parole_eligibility_table |>
#   filter(!state %in% states_to_exclude$state) |>
#   filter(!state %in% states_national_page_only$state) |>
#   pull(state)

# Read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")
states_qmd <- as.character(states)

# Iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)

# Render all qmds
# quarto::quarto_render()








# Load Data
# load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_not_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing_race.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_undercounted.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_rptyear.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race_2019.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_sex_2019.rds"))
