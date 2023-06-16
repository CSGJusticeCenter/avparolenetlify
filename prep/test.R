library(stringr)
library(purrr)

# save working directory
wd <- getwd()

# states <- state.name
states <- c("Florida")

# read in original qmd
orig_qmd <- read_lines("_state_report_template.qmd")

# replacement values
states_qmd <- as.character(states)

# function to replace place-holder text in orig qmd with our replacement values
# and write out to qmd - name of qmd should include replacement value
replace_write_qmd <- function(state) {
  state_report_qmd <- str_replace(orig_qmd, "change_me", state)
  state_specific_qmd <- read_lines(paste0(state, "_notes.qmd"))

  combined_qmd <- c(state_report_qmd, state_specific_qmd)

  write_lines(combined_qmd, paste0("state_report_", state, ".qmd"))
}

# iterate over replacement values and write new qmds
walk(states_qmd, replace_write_qmd)
