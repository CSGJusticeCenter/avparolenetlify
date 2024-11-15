# Load necessary libraries
library(dplyr)

# Sample tibble data
which_overall_year <- tibble(
  state = c("Arkansas", "Colorado", "Connecticut", "Georgia", "Hawaii",
            "Idaho", "Kentucky", "Louisiana", "Maryland", "Massachusetts"),
  year_to_use = c(2019, 2019, 2019, 2019, 2018, 2019, 2019, 2019, 2019, 2019)
)

# Check if all `year_to_use` values are the same
unique_years <- unique(which_overall_year$year_to_use)

# Assign the select_year, pop_select_year, and bjs_data_year
if(length(unique_years) == 1) {
  select_year <- unique_years[1]
} else {
  # If there are different years, you may need to assign it conditionally
  select_year <- 2019
}

# Assign other variables based on select_year
pop_select_year <- select_year
bjs_data_year <- select_year

# Print the assigned years
select_year
pop_select_year
bjs_data_year









# # States with high missingness for race and ethnicity
# states_with_high_missing_race <- ncrp_yearendpop_consolidated |>
#   filter(rptyear == select_year) |>
#   group_by(state) |>
#   summarize(
#     perc_missing_race = round(mean(race == "Unknown" | is.na(race)) * 100, 1),
#     .groups = "drop") |>
#   filter(perc_missing_race > 50) |>
#   select(state) |>
#   distinct()
