# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility

# In 2019, XXX Black, non-Hispanic individuals
# collectively spent YYYY years in prison past their parole eligibility year across
# UU states, serving an average of X years beyond parole eligibility, compared to X years for White individuals.
# ---------------------------------------------------------------------------- #

# Function that filters the population data to include only people in prison for new crimes
# with sentence lengths 1+ years except life
# Only includes states with parole systems and without high missingness
# Includes states don't need to be filtered by admission type or sentence length
ncrp_yearendpop_filtered <-
  fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                    exclude = states_to_exclude,
                                    dont_filter = states_nofilter)

# Get NCRP data for people in prison past parole eligibility
ncrp_past_pe <- ncrp_yearendpop_filtered |> filter(parelig_status == "Current")

# Get average time served past PE for people still in prison by race and ethnicity
avg_past_pe_race_by_state <- ncrp_past_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown") |>
  # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
  filter(
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))
  ) |>
  # change negative to positive, negative means past parole eligibility year
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  group_by(state, race, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Filter states that are using 2018 data
states_using_2018_data <- avg_past_pe_race |>
  filter(rptyear == 2018) |>
  pull(state) |>
  unique()

# Calculate across states
avg_past_pe_race <- avg_past_pe_race_by_state |>
  group_by(race) |>
  summarise(avg_years_to_estimated_pey = mean(avg_years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(total_years_past_pe, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Number of states included and excluded
included_states <- unique(avg_past_pe_race$state)

# Separate states with high missing data and high missing data for race and ethnicity
high_missing_general_states <- unique(states_with_high_missing$state)
high_missing_race_states <- unique(states_with_high_missing_race$state)

# Combine both lists for the excluded states, ensuring no duplicates
excluded_states <- unique(c(high_missing_general_states, high_missing_race_states))

# Number of states in each category
count_high_missing_general_states <- length(high_missing_general_states)
count_high_missing_race_states <- length(high_missing_race_states)
count_excluded_states <- length(excluded_states)
count_included_states <- length(included_states)
count_abolished_states <- length(states_abolished_parole$state)

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "White, non-Hispanic"]),
    "White, non-Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "White, non-Hispanic"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years beyond parole eligibility.\n")

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "Black, non-Hispanic"]),
    "Black, non-Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "Black, non-Hispanic"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "Black, non-Hispanic"], 1),
    "years beyond parole eligibility, compared to",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years for White individuals.\n")

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "Hispanic, any race"]),
    "Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "Hispanic, any race"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "Hispanic, any race"], 1),
    "years beyond parole eligibility, compared to",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years for White individuals.\n")

# Add the note about states using 2018 data
cat("Note: The following states used 2018 data due to unreliable 2019 data:",
    paste(states_using_2018_data, collapse = ", "), ".\n")

# Print included and excluded states with counts
cat("Included states (", count_included_states, "):", paste(included_states, collapse = ", "), "\n")
cat("Excluded states (", count_excluded_states, "):", paste(excluded_states, collapse = ", "), "\n")

# Print states with high missing data and high missing data for race separately
cat("States with Unreliable PE Data (", count_high_missing_general_states, "):", paste(high_missing_general_states, collapse = ", "), "\n")
cat("States with High Missingness for Race/Ethnicity (", count_high_missing_race_states, "):", paste(high_missing_race_states, collapse = ", "), "\n")

# Print states that have abolished parole
cat("States Abolished Parole (", count_abolished_states, "):", paste(states_abolished_parole$state, collapse = ", "), "\n")


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility

# XXX people held past their parole eligibility year were convicted of non-violent
# offenses. Thousands of people who have not committed violent offenses remain incarcerated
# after their parole eligibility date in XX states. This includes XX people convicted
# of property offenses, XX convicted of drug offenses, and YY convicted of public order offenses.
# ---------------------------------------------------------------------------- #

# Categorize offenses into violent vs. nonviolent, keeping "Unknown" offenses for initial analysis
ncrp_past_pe_offense_initial <- ncrp_past_pe |>
  mutate(offense_group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ fbi_index))

# Identify states with high missingness for offense type
states_with_high_missing_offense <- ncrp_past_pe_offense_initial |>
  group_by(state) |>
  summarise(unknown_offense_count = sum(offense_group == "Unknown"),
            total_count = n(),
            unknown_offense_percentage = (unknown_offense_count / total_count) * 100,
            .groups = "drop") |>
  filter(unknown_offense_percentage > 50) |>
  pull(state)  # Using a 50% threshold for high missingness

# Filter out "Unknown" offenses for further analysis
ncrp_past_pe_offense_filtered <- ncrp_past_pe_offense_initial |>
  filter(offense_group != "Unknown")

# Summarize counts for violent vs. nonviolent offenses
offense_summary <- ncrp_past_pe_offense_filtered |>
  group_by(offense_group) |>
  summarise(total_people = n(), .groups = "drop")

# Calculate percentages
total_people_overall <- sum(offense_summary$total_people)
violent_offense_count <- offense_summary$total_people[offense_summary$offense_group == "Violent"]
nonviolent_offense_count <- offense_summary$total_people[offense_summary$offense_group == "Nonviolent"]

percent_violent <- (violent_offense_count / total_people_overall) * 100
percent_nonviolent <- (nonviolent_offense_count / total_people_overall) * 100

# Check if each state has all nonviolent offense types (Property, Drug, Public Order)
states_with_all_offenses <- ncrp_past_pe_offense_filtered |>
  filter(offense_group == "Nonviolent") |>
  group_by(state) |>
  summarise(has_property = any(fbi_index == "Property"),
            has_drug = any(fbi_index == "Drug"),
            has_public_order = any(fbi_index == "Public Order"),
            .groups = "drop") |>
  filter(has_property & has_drug & has_public_order) |>
  pull(state)

# Count the number of states with all nonviolent offense types
count_states_with_all_offenses <- length(states_with_all_offenses)

# Combine high missingness states for different categories
all_high_missing_states <- unique(c(high_missing_general_states,
                                    high_missing_race_states,
                                    states_with_high_missing_offense))

# Separate states into included and excluded categories
excluded_states_with_offense <- unique(states_with_high_missing_offense)
excluded_states_overall <- unique(all_high_missing_states)
included_states_with_offense <- setdiff(included_states, excluded_states_overall)

# Update counts
count_excluded_states_overall <- length(excluded_states_overall)
count_included_states_with_offense <- length(included_states_with_offense)
count_high_missing_general_states <- length(high_missing_general_states)
count_high_missing_race_states <- length(high_missing_race_states)
count_high_missing_offense_states <- length(states_with_high_missing_offense)

# Print the updated results
cat(comma(nonviolent_offense_count), "people held past their parole eligibility year were convicted of non-violent offenses.\n")
cat("Thousands of people who have not committed violent offenses remain incarcerated after their parole eligibility date in",
    count_states_with_all_offenses, "states. This includes",
    comma(property_offense_count), "people convicted of property offenses,",
    comma(drug_offense_count), "convicted of drug offenses, and",
    comma(public_order_offense_count), "convicted of public order offenses.\n")

# Print percentages for violent and nonviolent offenses
cat("Overall, approximately", round(percent_violent, 1), "percent of individuals in prison past parole eligibility are serving time for violent offenses, while",
    round(percent_nonviolent, 1), "percent are serving time for non-violent offenses.\n")

# Print included and excluded states with details
cat("Included states with complete offense data (", count_included_states_with_offense, "):",
    paste(included_states_with_offense, collapse = ", "), "\n")
cat("Excluded states due to high missingness (", count_excluded_states_overall, "):",
    paste(excluded_states_overall, collapse = ", "), "\n")
cat("States with Unreliable PE Data (", count_high_missing_general_states, "):",
    paste(high_missing_general_states, collapse = ", "), "\n")
cat("States with High Missingness for Race/Ethnicity (", count_high_missing_race_states, "):",
    paste(high_missing_race_states, collapse = ", "), "\n")
cat("States with High Missingness for Offense Type (", count_high_missing_offense_states, "):",
    paste(states_with_high_missing_offense, collapse = ", "), "\n")

