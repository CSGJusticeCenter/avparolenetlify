# Import Seba Guzman's rri values
seba_rris_pop_pop_race_v1 <-
  read_csv(file.path(sp_data_path, "/data/analysis/rris/rris_pop_pop_race_v1.csv")) |>
  select(state, race, rri_seba = rri)

# Import Mari Robert's rri values
load(file = paste0(sp_data_path, "/data/analysis/app/all_pe_rri_data.rds"))

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










