# ---------------------------------------------------------------------------- #
# Past Parole Eligibility -
# Denominator = Total prison population
# Numerator = Population past parole eligibility
# 12 RRIs above 1 for Black/5 below 1
# 6 RRIs above 1 for Hispanic/13 below 1
# ---------------------------------------------------------------------------- #

robina_table_seba <- read_excel("C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole/data/raw/Robina Institute/robina_table.xlsx",
                           sheet = "Sheet1", skip = 1) |>
  clean_names() |>
  select(state = variable_labels,
         low_min_less_serious = lower_boundary_of_min_sentence_for_less_serious_offenses_as_percent_of_max,
         upper_min_less_serious = upper_boundary_of_min_sentence_for_less_serious_offenses_as_percent_of_max,
         low_min_more_serious = lower_boundary_of_min_sentence_for_more_serious_offenses_as_percent_of_max,
         upper_min_more_serious = upper_boundary_of_min_sentence_for_more_serious_offenses_as_percent_of_max) |>
  mutate(less_serious_diff = upper_min_less_serious - low_min_less_serious,
         more_serious_diff = upper_min_more_serious - low_min_more_serious) |>
  mutate(potential_issue_flag_seba = case_when(
    less_serious_diff > 0 & more_serious_diff > 0 ~ 1,
    less_serious_diff == 0 & more_serious_diff == 0 ~ 0,
    TRUE ~ 0
  )) |>
  select(state, potential_issue_flag_seba)




# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# of 1+ years except life
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop_consolidated) |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ "Other or Unknown"))|>
  filter(race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic"))

ncrp_releases_filtered <- fnc_filter_pe_population_criteria(ncrp_releases_consolidated) |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ "Other or Unknown")) |>
  filter(race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic"))



fnc_calculate_rris <- function(numerator_data, denominator_data, reference_race = "White, non-Hispanic",
                               select_year = 2019, numerator_filter = "none", denominator_filter = "none", group_vars) {

  # Define a helper function to apply the appropriate filter based on input
  apply_filter <- function(data, filter_option) {
    if (filter_option == "none") {
      return(data)
    } else if (filter_option == "current") {
      return(data |> filter(estimated_pey_status == "current_year")) # Only current, not past PE
    } else if (filter_option == "past") {
      return(data |> filter(estimated_pey_status == "past")) # only past PE, not current
    } else if (filter_option == "both") {
      return(data |> filter(estimated_pey_status %in% c("past", "current_year"))) # both currently and past PE
    }
  }

  # Get total prison pop by state, race, and other group vars
  denominator_data1 <- denominator_data |>
    filter(rptyear == select_year) |>
    apply_filter(denominator_filter) |>
    group_by(across(all_of(group_vars))) |>
    summarise(denominator_pop = n(), .groups = "drop")

  # Get current PE pop by state, race, and other group vars
  numerator_data1 <- numerator_data |>
    filter(rptyear == select_year) |>
    apply_filter(numerator_filter) |>
    group_by(across(all_of(group_vars))) |>
    summarise(numerator_pop = n(), .groups = "drop")

  # Merge with parole eligibility data
  merged_data <- denominator_data1 |>
    left_join(numerator_data1, by = group_vars) |>
    mutate(rate = numerator_pop / denominator_pop)

  # Calculate the reference rate for the reference group (e.g., "White, non-Hispanic")
  reference_rate <- merged_data |>
    filter(race == reference_race) |>
    group_by(across(all_of(setdiff(group_vars, "race")))) |> # Group by everything except race
    summarise(reference_rate = mean(rate, na.rm = TRUE), .groups = "drop") # Aggregate to avoid duplicates

  # Calculate RRI for other racial groups
  rri_data <- merged_data |>
    inner_join(reference_rate, by = setdiff(group_vars, "race")) |> # Join by all group_vars except race
    mutate(rri = rate / reference_rate,
           rri = round(rri, 1)) |>
    select(all_of(group_vars), rri)

  return(rri_data)
}

fnc_count_rris <- function(rri_data) {
  counts <- rri_data |>
    filter(race != "White, non-Hispanic") |>
    group_by(race) |>
    summarise(
      above_1 = sum(rri > 1, na.rm = TRUE),
      below_1 = sum(rri < 1, na.rm = TRUE),
      is_1    = sum(rri == 1, na.rm = TRUE)
    )
  return(counts)
}



# *** This RRI the likelihood of being incarcerated past parole eligibility out of the
# entire prison population. The numerator and denominator both reflect individuals
# past their parole eligibility but still in prison, focusing on who remains
# incarcerated despite being eligible for release.

# Strength: Focuses specifically on individuals past parole eligibility,
# offering a clear view of those who remain incarcerated after becoming eligible.
# Limitation: Excludes individuals who have already been released,
# potentially missing important patterns in prompt releases.
# Research Discussion: Clearly isolates the effect of parole eligibility,
# avoiding distortion from individuals not yet eligible.

# Rate is the number of people in prison past PE out of the entire prison population
rris_pop_pop_race <- fnc_calculate_rris(
  numerator_data = ncrp_yearendpop_filtered,
  numerator_filter = "both",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
) |>
  left_join(robina_table_seba, by = "state")

cat("rris_pop_pop_race:\n")
print(fnc_count_rris(rris_pop_pop_race))

write.csv(rris_pop_pop_race, "rris_pop_pop_race.csv")

rris_pop_pop_race_pastonly <- fnc_calculate_rris(
  numerator_data = ncrp_yearendpop_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
)|>
  left_join(robina_table_seba, by = "state")

cat("rris_pop_pop_race_pastonly:\n")
print(fnc_count_rris(rris_pop_pop_race_pastonly))

write.csv(rris_pop_pop_race_pastonly, "rris_pop_pop_race_pastonly.csv")

# This dataset calculates the RRI for individuals released past parole eligibility out of everyone released.
# The numerator includes those released after their parole eligibility date, and
# the denominator is all individuals who were released during the year.

# Rate is the number of people released past PE out of everyone released,
rris_rel_rel_race <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_releases_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
)|>
  left_join(robina_table_seba, by = "state")

cat("rris_rel_rel_race:\n")
print(fnc_count_rris(rris_rel_rel_race))


write.csv(rris_rel_rel_race, "rris_rel_rel_race.csv")
