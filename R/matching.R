library(dplyr)
library(fuzzyjoin)

# Rename columns for consistency
magistrate_ids <- raw_wv_magistrate_case_defendant_disposition |>
  rename(Name = Defendant, DOB_Magistrate = DOB) |>
  clean_names() |>
  select(name, dob_magistrate, defendant_id) |>
  distinct()

probation_ids <- raw_probation_ocms_offender_supervision |>
  rename(Name = NameComposite, DOB_Probation = DOB) |>
  clean_names() |>
  select(name, dob_probation, offender_id) |>
  distinct()

# record linkage
# probalistic matching techniques

# # Perform fuzzy matching on Name and exact matching on DOB
# # The Jaro-Winkler ("jw") method, is a popular choice for name matching because
# # it is sensitive to common spelling errors and variations.
# matched_data <- stringdist_inner_join(
#   magistrate_ids,
#   probation_ids,
#   by = "name",
#   method = "jw",
#   max_dist = 0.1,  # Adjust this value based on your tolerance for name differences
#   distance_col = "dist"
# ) |>
#   filter(dob_magistrate == dob_probation)  # Ensure DOBs match exactly

# https://github.com/kosukeimai/fastLink
# if(!require(devtools)) install.packages("devtools")
# library(devtools)
# install_github("kosukeimai/fastLink", dependencies = TRUE)

