# Import Seba Guzman's rri values
seba_rris_pop_pop_race_v1 <-
  read_csv(file.path(sp_data_path, "/data/analysis/rris/rris_pop_pop_race_v1.csv")) |>
  select(state, race, rri_seba = rri)

# # Import Mari Robert's rri values
# load(file = paste0(sp_data_path, "/data/analysis/app/all_pe_rri_data.rds"))
# Filter the consolidated year-end prison population data for specific criteria
ncrp_yearendpop_filtered <- ncrp_yearendpop_consolidated |>
  filter(state %in% seba_rris_pop_pop_race_v1$state) |>
  filter(!state %in% states_to_exclude$state) |>  # Exclude states with abolished parole or high missingness
  filter(
    !(admtype %in% c("Other", "Parole return/revocation") | is.na(admtype) | admtype == "Unknown") &
      !(sentlgth_raw %in% c("< 1 year", "Life, LWOP, Life plus additional years, Death") | is.na(sentlgth_raw) | sentlgth_raw == "Unknown")
  )

# Exclude states with high missingness for race and ethnicity and filter by state-specific conditions
ncrp_yearendpop_race <- ncrp_yearendpop_filtered |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  group_by(state) |>  # Group by state for state-specific filtering
  filter(
    race %in% ifelse(
      state %in% states_use_other_race_eth$state,  # For states requiring "Other race(s)"
      c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic"),
      c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")  # Default race categories
    )
  )

# Summarize the total prison population by state, year, and race
prison_pop_by_race <- ncrp_yearendpop_race |>
  group_by(state, rptyear, race) |>
  summarise(
    total_prison_pop = n(),  # Count the total population for each group
    .groups = "drop"         # Avoid grouped output in the result
  ) |>
  fnc_filter_by_year(which_overall_year) |>  # Filter for the most relevant year
  select(-c(rptyear, year_to_use))  # Remove unnecessary columns after filtering

# Calculate the population past parole eligibility by state, year, and race
prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_race |>
  filter(parelig_status == "Current") |>  # Include only individuals past their parole eligibility year
  group_by(state, rptyear, race) |>
  summarise(
    n = n(),  # Count the number of individuals past eligibility
    .groups = "drop"
  ) |>
  fnc_filter_by_year(which_overall_year) |>
  select(-year_to_use)  # Drop unused column

# Merge total prison population and past parole eligibility data to calculate rates
merged_prison_pop_data_race <- prison_pop_by_race |>
  left_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) |>  # Join by state and race
  mutate(past_pe_rate = n / total_prison_pop)  # Calculate the rate of individuals past parole eligibility

# ---------------------------------------------------------------------------- #
# Relative Rate Index (RRI) Calculation for Racial Groups
# ---------------------------------------------------------------------------- #

# Calculate RRI using the merged data, comparing each group to White individuals
all_pe_rri_data <- fnc_calculate_rri(
  merged_prison_pop_data_race,
  comparison_group = "White, non-Hispanic",  # Set "White, non-Hispanic" as the reference group
  category = "race") |>
  mutate(
    rri = case_when(
      TRUE ~ rri  # Retain calculated RRI otherwise
    )
  )

# Merge with Seba's findings to see if we get the same thing
all_pe_rri_data_with_seba_findings <- all_pe_rri_data |>
  left_join(seba_rris_pop_pop_race_v1, by = c("state", "race"))

# Summarize RRI counts for Black, non-Hispanic by number of states
rri_black_summary <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Black, non-Hispanic") %>%
  group_by(state) %>%
  summarise(
    rri_above_1 = any(rri > 1),
    rri_equal_1 = any(rri == 1),
    rri_below_1 = any(rri < 1)
  ) %>%
  summarise(
    above_1 = sum(rri_above_1),
    equal_1 = sum(rri_equal_1),
    below_1 = sum(rri_below_1)
  )

# Summarize RRI_SEBA counts for Black, non-Hispanic by number of states
rri_seba_black_summary <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Black, non-Hispanic") %>%
  group_by(state) %>%
  summarise(
    rri_seba_above_1 = any(rri_seba > 1),
    rri_seba_equal_1 = any(rri_seba == 1),
    rri_seba_below_1 = any(rri_seba < 1)
  ) %>%
  summarise(
    above_1 = sum(rri_seba_above_1),
    equal_1 = sum(rri_seba_equal_1),
    below_1 = sum(rri_seba_below_1)
  )

# Summarize RRI counts for Hispanic, any race by number of states
rri_hispanic_summary <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Hispanic, any race") %>%
  group_by(state) %>%
  summarise(
    rri_above_1 = any(rri > 1),
    rri_equal_1 = any(rri == 1),
    rri_below_1 = any(rri < 1)
  ) %>%
  summarise(
    above_1 = sum(rri_above_1),
    equal_1 = sum(rri_equal_1),
    below_1 = sum(rri_below_1)
  )

# Summarize RRI_SEBA counts for Hispanic, any race by number of states
rri_seba_hispanic_summary <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Hispanic, any race") %>%
  group_by(state) %>%
  summarise(
    rri_seba_above_1 = any(rri_seba > 1),
    rri_seba_equal_1 = any(rri_seba == 1),
    rri_seba_below_1 = any(rri_seba < 1)
  ) %>%
  summarise(
    above_1 = sum(rri_seba_above_1),
    equal_1 = sum(rri_seba_equal_1),
    below_1 = sum(rri_seba_below_1)
  )

# Generate sentences for Black, non-Hispanic
black_summary_sentence <- paste(
  "For Black, non-Hispanic individuals:\n",
  "Mari's Findings:\n",
  rri_black_summary$above_1, "states have RRI above 1,\n",
  rri_black_summary$equal_1, "states have RRI equal to 1,\n",
  rri_black_summary$below_1, "states have RRI below 1.\n\n",
  "Seba's Findings:\n",
  rri_seba_black_summary$above_1, "states have RRI above 1,\n",
  rri_seba_black_summary$equal_1, "states have RRI equal to 1,\n",
  rri_seba_black_summary$below_1, "states have RRI below 1."
)

# Generate sentences for Hispanic, any race
hispanic_summary_sentence <- paste(
  "For Hispanic individuals:\n",
  "Mari's Findings:\n",
  rri_hispanic_summary$above_1, "states have RRI above 1,\n",
  rri_hispanic_summary$equal_1, "states have RRI equal to 1,\n",
  rri_hispanic_summary$below_1, "states have RRI below 1.\n\n",
  "Seba's Findings:\n",
  rri_seba_hispanic_summary$above_1, "states have RRI above 1,\n",
  rri_seba_hispanic_summary$equal_1, "states have RRI equal to 1,\n",
  rri_seba_hispanic_summary$below_1, "states have RRI below 1."
)

# Print the results with breaks
cat(black_summary_sentence, "\n\n")
cat(hispanic_summary_sentence, "\n")


# Generate a list of states for each RRI condition for Black, non-Hispanic
black_states_mari <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Black, non-Hispanic") %>%
  group_by(state) %>%
  summarise(
    above_1 = any(rri > 1),
    equal_1 = any(rri == 1),
    below_1 = any(rri < 1)
  ) %>%
  filter(above_1 | equal_1 | below_1)

black_states_seba <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Black, non-Hispanic") %>%
  group_by(state) %>%
  summarise(
    above_1 = any(rri_seba > 1),
    equal_1 = any(rri_seba == 1),
    below_1 = any(rri_seba < 1)
  ) %>%
  filter(above_1 | equal_1 | below_1)

# Add list of states to the sentences for Black, non-Hispanic
black_summary_sentence <- paste(
  "For Black, non-Hispanic individuals:\n",
  "Mari's Findings:\n",
  rri_black_summary$above_1, "states have RRI above 1:\n", paste(black_states_mari$state[black_states_mari$above_1], collapse = "\n"), "\n",
  rri_black_summary$equal_1, "states have RRI equal to 1:\n", paste(black_states_mari$state[black_states_mari$equal_1], collapse = "\n"), "\n",
  rri_black_summary$below_1, "states have RRI below 1:\n", paste(black_states_mari$state[black_states_mari$below_1], collapse = "\n"), "\n\n",
  "Seba's Findings:\n",
  rri_seba_black_summary$above_1, "states have RRI above 1:\n", paste(black_states_seba$state[black_states_seba$above_1], collapse = "\n"), "\n",
  rri_seba_black_summary$equal_1, "states have RRI equal to 1:\n", paste(black_states_seba$state[black_states_seba$equal_1], collapse = "\n"), "\n",
  rri_seba_black_summary$below_1, "states have RRI below 1:\n", paste(black_states_seba$state[black_states_seba$below_1], collapse = "\n")
)

# Repeat the same logic for Hispanic, any race
hispanic_states_mari <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Hispanic, any race") %>%
  group_by(state) %>%
  summarise(
    above_1 = any(rri > 1),
    equal_1 = any(rri == 1),
    below_1 = any(rri < 1)
  ) %>%
  filter(above_1 | equal_1 | below_1)

hispanic_states_seba <- all_pe_rri_data_with_seba_findings %>%
  filter(race == "Hispanic, any race") %>%
  group_by(state) %>%
  summarise(
    above_1 = any(rri_seba > 1),
    equal_1 = any(rri_seba == 1),
    below_1 = any(rri_seba < 1)
  ) %>%
  filter(above_1 | equal_1 | below_1)

# Add list of states to the sentences for Hispanic, any race
hispanic_summary_sentence <- paste(
  "For Hispanic individuals:\n",
  "Mari's Findings:\n",
  rri_hispanic_summary$above_1, "states have RRI above 1:\n", paste(hispanic_states_mari$state[hispanic_states_mari$above_1], collapse = "\n"), "\n",
  rri_hispanic_summary$equal_1, "states have RRI equal to 1:\n", paste(hispanic_states_mari$state[hispanic_states_mari$equal_1], collapse = "\n"), "\n",
  rri_hispanic_summary$below_1, "states have RRI below 1:\n", paste(hispanic_states_mari$state[hispanic_states_mari$below_1], collapse = "\n"), "\n\n",
  "Seba's Findings:\n",
  rri_seba_hispanic_summary$above_1, "states have RRI above 1:\n", paste(hispanic_states_seba$state[hispanic_states_seba$above_1], collapse = "\n"), "\n",
  rri_seba_hispanic_summary$equal_1, "states have RRI equal to 1:\n", paste(hispanic_states_seba$state[hispanic_states_seba$equal_1], collapse = "\n"), "\n",
  rri_seba_hispanic_summary$below_1, "states have RRI below 1:\n", paste(hispanic_states_seba$state[hispanic_states_seba$below_1], collapse = "\n")
)

# Print the results with breaks
cat(black_summary_sentence, "\n\n")
cat(hispanic_summary_sentence, "\n")










