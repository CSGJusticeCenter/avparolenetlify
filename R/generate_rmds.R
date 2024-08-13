# Save working directory
wd <- getwd()

# Get list of 50 states
states <- c("Georgia", "Iowa", "Idaho", "West Virginia")

# Read in original qmd
orig_qmd <- read_lines("_new_state_report_template.qmd")

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
