# ---------------------------------------------------------------------------- #
# Past Parole Eligibility -
# Denominator = Total prison population
# Numerator = Population past parole eligibility
# 12 RRIs above 1 for Black/5 below 1
# 6 RRIs above 1 for Hispanic/13 below 1
# ---------------------------------------------------------------------------- #

# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# of 1+ years except life
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
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

ncrp_releases_filtered <- fnc_filter_pe_population_criteria(ncrp_releases) |>
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
                               select_year = 2020, numerator_filter = "none", denominator_filter = "none", group_vars) {

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

# Function to count states with RRIs above and below 1 for each race
fnc_count_rris <- function(rri_data) {
  counts <- rri_data |>
    filter(race != "White, non-Hispanic") |>
    group_by(race) |>
    summarise(
      above_1 = sum(rri > 1),
      below_1 = sum(rri < 1),
      is_1    = sum(rri == 1)
    )
  return(counts)
}

# RRI - shows the likelihood of being released past parole eligibility (PE) out of
# the entire prison population. The numerator represents individuals released after
# they were past parole eligibility, and the denominator is the entire prison
# population at the end of the year.

# Strength: Provides a broad comparison, showing likelihood of release past PE for the whole prison population.
# It captures disparities across the full population, regardless of parole eligibility status.
# Limitation: The analysis might be diluted by including individuals not yet parole-eligible,
# which could skew interpretation by mixing in those who aren't eligible for parole.
# Research Discussion: Isolates parole disparities less clearly since many in the population aren't eligible.
# Release decisions may also depend on factors like behavior, not just parole eligibility.

# rris_rel_pop_race: Includes the entire prison population, diluting the analysis by
# mixing in individuals not yet parole-eligible, making it less focused on delays for those eligible.

# Rate is the number of people released past PE out of the entire prison population
rris_rel_pop_race <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
)
# now also by offense
rris_rel_pop_race_offense <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race", "group")
)

cat("rris_rel_pop_race:\n")
print(fnc_count_rris(rris_rel_pop_race))


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

# Rate is the number of people in prison past PE out of th entire prison population
rris_pop_pop_race <- fnc_calculate_rris(
  numerator_data = ncrp_yearendpop_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
)
# now also by offense
rris_pop_pop_race_offense <- fnc_calculate_rris(
  numerator_data = ncrp_yearendpop_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race", "group")
)

cat("rris_pop_pop_race:\n")
print(fnc_count_rris(rris_pop_pop_race))


# This dataset calculates the RRI for individuals released past parole eligibility out of everyone released.
# The numerator includes those released after their parole eligibility date, and
# the denominator is all individuals who were released during the year.

# Strength: Focuses on the likelihood of release past PE within the group of those actually released,
# providing a precise look at delayed releases among the released population.
# Limitation: Doesn’t account for those still incarcerated past PE,
# which could miss patterns where individuals are not released at all.
# Research Discussion: Highlights post-eligibility release delays,
# but misses information on those who remain in prison past eligibility.

# rris_rel_rel_race: Focuses on individuals already released, but misses those still
# incarcerated past PE, so it doesn’t capture the full picture of delayed releases.

# Rate is the number of people released past PE out of everyone released,
rris_rel_rel_race <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_releases_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race")
)
# now also by offense
rris_rel_rel_race_offense <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_releases_filtered,
  denominator_filter = "none",
  group_vars = c("state", "race", "group")
)

cat("rris_rel_rel_race:\n")
print(fnc_count_rris(rris_rel_rel_race))


# *** The RRI in this dataset compares the likelihood of being released past
# parole eligibility out of the population past parole eligibility. The numerator
# includes individuals released after their parole eligibility date, and
# the denominator is the total population of individuals who are past parole
# eligibility (whether they were released or not).

# Strength: Isolates those past parole eligibility, combining the entire past-PE population
# with release data to offer a focused view of delayed release.
# Limitation: Does not provide insights into those not yet parole-eligible,
# so may miss broader prison population trends.
# Research Discussion: Provides clear insights into post-PE release,
# but less useful for understanding the broader context of parole and prison population dynamics.

# rris_rel_pop_both_race: While it isolates individuals past PE, it combines those still
# incarcerated with those released, making it less precise in identifying specific release delays.

# Rate is the number of people released past PE out of the prison population past PE
rris_rel_pop_both_race <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "both",
  group_vars = c("state", "race")
)
# now also by offense
rris_rel_pop_both_race_offense <- fnc_calculate_rris(
  numerator_data = ncrp_releases_filtered,
  numerator_filter = "past",
  denominator_data = ncrp_yearendpop_filtered,
  denominator_filter = "both",
  group_vars = c("state", "race", "group")
)

cat("rris_rel_pop_both_race:\n")
print(fnc_count_rris(rris_rel_pop_both_race))


# Overall - rris_pop_pop_race is the best option if you're focused on understanding who remains
# incarcerated past parole eligibility, as it isolates those eligible for parole, providing a
# clear view of delays without being diluted by individuals not yet eligible.
