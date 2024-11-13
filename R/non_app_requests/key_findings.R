
# Sample:
# All paroling states where we can impute PEY
# Use the imputed PEY to determine if someone is someone is past their parole eligibility
# Use 2019 data, or the year of data you are using in the tool if it is different

# Then just run the stats with the full sample - don't worry about weighting or anything else:


#
# XXX people held past their parole eligibility year were convicted of non-violent offenses.
# Thousands of people who have not committed violent offenses remain incarcerated after
# their parole eligibility date in XX states. This includes XX people convicted of
# property offenses, XX convicted of drug offenses, and YY convicted of public order offenses.


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility

# In 2019, XXX Black, non-Hispanic individuals
# collectively spent YYYY years in prison past their parole eligibility year across
# UU states, serving an average of X years beyond parole eligibility, compared to X years for White individuals.
# ---------------------------------------------------------------------------- #

# Filter data to people currently eligible for parole or past parole eligibility
ncrp_current_pe <- fnc_filter_pe_population_criteria(ncrp_yearendpop_consolidated) |>
  filter(parelig_status == "Current") |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  # Join with which_overall_year to get the specific year for each state
  left_join(which_overall_year, by = "state") |>
  # Use mutate to create a year filter column and then filter
  mutate(year_to_filter = rptyear == year_to_use) |>
  filter(year_to_filter)

# Get average time between PE and release by state and race
avg_current_pe_race <- ncrp_current_pe |>
  filter(race %in% c("White, non-Hispanic", "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic", "White, non-Hispanic")),
         # All are negative or zero since they are past parole eligibility
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # Change negative to positive, negative means past parole eligibility year
  group_by(race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Number of states included and excluded
included_states <- unique(ncrp_current_pe$state)
excluded_states <- states_with_high_missing_race$state

# Number of states included, excluded, and abolished parole
count_included_states <- length(included_states)
count_excluded_states <- length(excluded_states)
count_abolished_states <- length(abolished_states$state)

# List of states using 2018 data
states_using_2018_data <- which_overall_year$state[which_overall_year$year_to_use == 2018]

# Print the results with the additional note
cat("In 2019, ", comma(avg_current_pe_race$people[avg_current_pe_race$race == "Black, non-Hispanic"]),
    " Black, non-Hispanic individuals collectively spent ",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "Black, non-Hispanic"], 1)),
    " years in prison past their parole eligibility year across ",
    count_included_states, " states, serving an average of ",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "Black, non-Hispanic"], 1),
    " years beyond parole eligibility, compared to ",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    " years for White individuals.\n")

# Add the note about states using 2018 data
cat("Note: The following states used 2018 data due to unreliable 2019 data: ",
    paste(states_using_2018_data, collapse = ", "), ".\n")

# Print included and excluded states with counts
cat("Included states (", count_included_states, "): ", paste(included_states, collapse = ", "), "\n")
cat("Excluded states (", count_excluded_states, "): ", paste(excluded_states, collapse = ", "), "\n")
cat("States Abolished Parole (", count_abolished_states, "): ", paste(abolished_states$state, collapse = ", "), "\n")



