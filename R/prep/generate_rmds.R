
source("R/prep/config.R")
# source("R/prep/page_national_trends.R")
# source("R/prep/tab_parole_eligibility.R")
# source("R/prep/tab_population.R")
# source("R/prep/tab_releases.R")
# source("R/prep/tab_disparities_rris.R")
# source("R/prep/tab_disparities_time_served.R")
# source("R/prep/tab_disparities_years_past_pe.R")

# Load notes
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))

# Save working directory
wd <- getwd()

# Get list of states where parol eligibility is the focus
states <- "Georgia"
# states <- state_notes |>
#   filter(abolished_parole == "N") |>
#   inner_join(parole_eligibility_table, by = "state") |>
#   filter(!is.na(current_perc)) |>
#   pull(state)

# Read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")

# Replace state name
states_qmd <- as.character(states)

# Function to replace place-holder text in orig qmd with our replacement values (state names)
#    and write out to qmd - name of qmd should include state name
replace_write_qmd <- function(state) {
  cleaned_state <- str_replace_all(state, "\\s+", "_") # replace spaces with underscores
  str_replace(orig_qmd, "this_state", state) |>
    write_lines(paste0("state_report_", cleaned_state, ".qmd"))
}

# Iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)

# # Render Georgia
# quarto::quarto_render()
