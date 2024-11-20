################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: November 15, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
################################################################################

# ---------------------------------------------------------------------------- #
# Prepare Data for Sentences and Visualizations
# Time Served Overall and by Offense
# Years Past Parole Eligibility and by Offense
# ---------------------------------------------------------------------------- #

desired_order <- c(
  "Drug",
  "Public Order",
  "Property",
  "Aggravated or Simple Assault",
  "Robbery",
  "Rape or Sexual Assault",
  "Negligent Manslaughter",
  "Murder or Nonnegligent Manslaughter",
  "Other Violent Offenses",
  "Other or Unspecified"
)

# Get NCRP data for people released from prison
ncrp_releases_filtered <- ncrp_releases_not_consolidated |>
  filter(!state %in% states_to_exclude$state) |>  ################ change to ncrp_releases_consolidated when complete
  mutate(fbi_index = factor(fbi_index,
                            levels = c(desired_order)))

# Calculate average time served by race, ethnicity, and state
# Remove states with high missingness for race and ethnicity
# (states_to_exclude created in prep/import_format.R)
los_race <- ncrp_releases_filtered |>
  # Exclude states with high missingness for race and ethnicity
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown") |>
  # Apply race filter conditionally
  filter(
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))
  ) |>
  group_by(state, race, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Calculate average time served by offense, race, ethnicity, and state
los_race_by_offense_type <- ncrp_releases_filtered |>
  # Exclude states with high missingness for race and ethnicity
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  # Apply race filter conditionally
  filter(
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))
  ) |>
  group_by(state, race, fbi_index, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Calculate average time served by sex and state
los_sex <- ncrp_releases_filtered |>
  filter(sex != "Unknown") |>
  group_by(state, sex, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Calculate average time served by offense, sex and state
los_sex_by_offense_type <- ncrp_releases_filtered |>
  filter(race != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  group_by(state, sex, fbi_index, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Function that filters the population data to include only people in prison for new crimes
# with sentence lengths 1+ years except life
# Only includes states with parole systems and without high missingness
# Includes states don't need to be filtered by admission type or sentence length
# These states are in states_nofilter
ncrp_yearendpop_filtered <-
  fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                    exclude = states_to_exclude,
                                    dont_filter = states_nofilter)

# Get NCRP data for people in prison past parole eligibility
ncrp_past_pe <- ncrp_yearendpop_filtered |> filter(parelig_status == "Current")

# Get average time served past PE for people still in prison by race and ethnicity
avg_past_pe_race <- ncrp_past_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown") |>
  # Apply race filter conditionally
  filter(
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))
  ) |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Get average time served past PE for people still in prison by sex
avg_past_pe_sex <- ncrp_past_pe |>
  filter(sex != "Unknown") |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")|>
  fnc_filter_by_year(which_overall_year)

# Get average time served past PE for people still in prison by race and ethnicity and offense
avg_past_pe_race_by_offense_type <- ncrp_past_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  # Apply race filter conditionally
  filter(
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))
  ) |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race, fbi_index, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Get average time served past PE for people still in prison by sex and offense
avg_past_pe_sex_by_offense_type <- ncrp_past_pe |>
  filter(race != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex, fbi_index, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")|>
  fnc_filter_by_year(which_overall_year)

# ---------------------------------------------------------------------------- #
# Time Served
# ---------------------------------------------------------------------------- #

# Generate sentence about average time served sentence by race and ethnicity
all_sentence_los_race <-
  fnc_generate_disparity_sentences(df = los_race,
                                   type = "in prison",
                                   compare_var = "race",
                                   los_col = "average_los")
all_sentence_los_race$Georgia

# Generate sentence about average time served sentence by sex
all_sentence_los_sex <-
  fnc_generate_disparity_sentences(df = los_sex,
                                   type = "in prison",
                                   compare_var = "sex",
                                   los_col = "average_los")
all_sentence_los_sex$Georgia

# Generate lollipop charts of time served by race and ethnicity
all_lollipop_los_race <- fnc_generate_lollipop_charts(
  df = los_race,
  compare_var = "race"
)
all_lollipop_los_race$Georgia

# Generate lollipop charts of time served by sex
all_lollipop_los_sex <- fnc_generate_lollipop_charts(
  df = los_sex,
  compare_var = "sex"
)
all_lollipop_los_sex$Georgia

# Time served by race and offense
# SENTENCE:  "This chart shows the average time served by offense type and race in 2020. The largest disparity
#             was observed among robbery offenses, where Hispanic people spent an
#             average of 3.6 more years in prison compared to White people."
all_sentence_los_race_offense <- fnc_generate_offense_disparity_sentence(los_race_by_offense_type,
                                                                         "race",
                                                                         "average_los")
all_sentence_los_race_offense$Georgia

# Time served by race and offense
# SENTENCE:  "This chart shows the average time spent in prison past parole
#             eligibility by offense type and sex in 2020. The largest disparity
#             was observed among murder or nonnegligent manslaughter offenses,
#             where males spent an average of 3.6 more years in prison
#             compared to females."
all_sentence_los_sex_offense <- fnc_generate_offense_disparity_sentence(los_sex_by_offense_type,
                                                                        "sex",
                                                                        "average_los")
all_sentence_los_sex_offense$Georgia

# Create scatter charts for average time served by race, ethnicity, and offense
all_scatter_los_race_offense <- fnc_create_scatter_charts_by_state(
  df = los_race_by_offense_type,
  group_var = "race",
  measure = "average_los",
  source = ncrp_source
)
all_scatter_los_race_offense$Georgia
all_scatter_los_race_offense$Hawaii

# Create scatter charts for average time served by sex and offense
all_scatter_los_sex_offense <- fnc_create_scatter_charts_by_state(
  df = los_sex_by_offense_type,
  group_var = "sex",
  measure = "average_los",
  source = ncrp_source
)
all_scatter_los_sex_offense$Georgia




# ---------------------------------------------------------------------------- #
# Years Past Parole Eligibility
# ---------------------------------------------------------------------------- #

# Generate sentence about average time served sentence by race and ethnicity
all_sentence_avg_past_pe_race <-
  fnc_generate_disparity_sentences(df = avg_past_pe_race,
                                   type = "past parole eligibility",
                                   compare_var = "race",
                                   los_col = "avg_years_to_estimated_pey")

# Generate sentence about average time served sentence by sex
all_sentence_avg_past_pe_sex <-
  fnc_generate_disparity_sentences(df = avg_past_pe_sex,
                                   type = "past parole eligibility",
                                   compare_var = "sex",
                                   los_col = "avg_years_to_estimated_pey")

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and race and ethnicity in 2020.
#            The largest disparity was observed among negligent manslaughter
#            offenses, where Hispanic people spent an average of 2.2 more years
#            in prison compared to White people."
all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence(avg_past_pe_race_by_offense_type,
                                                                                 "race",
                                                                                 "avg_years_to_estimated_pey")

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and sex in 2020. The largest disparity
#            was observed among rape or sexual assault offenses, where males
#            spent an average of 2.6 more years in prison compared to females."
all_sentence_avg_past_pe_sex_offense <- fnc_generate_offense_disparity_sentence(avg_past_pe_sex_by_offense_type,
                                                                                "sex",
                                                                                "avg_years_to_estimated_pey")

# Create scatter charts for average time served by race, ethnicity, and offense
all_scatter_avg_past_pe_race_offense <- fnc_create_scatter_charts_by_state(
  df = avg_past_pe_race_by_offense_type,
  group_var = "race",
  measure = "avg_years_to_estimated_pey",
  source = ncrp_csg_source
)
all_scatter_avg_past_pe_race_offense$Georgia

# Create scatter charts for average time served by sex and offense
all_scatter_avg_past_pe_sex_offense <- fnc_create_scatter_charts_by_state(
  df = avg_past_pe_sex_by_offense_type,
  group_var = "sex",
  measure = "avg_years_to_estimated_pey",
  source = ncrp_csg_source
)
all_scatter_avg_past_pe_sex_offense$Georgia

# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  avg_past_pe_race              = "avg_past_pe_race.rds",
  avg_past_pe_sex               = "avg_past_pe_sex.rds",

  all_sentence_los_race         = "all_sentence_los_race.rds",
  all_lollipop_los_race         = "all_lollipop_los_race.rds",
  all_sentence_los_sex          = "all_sentence_los_sex.rds",
  all_lollipop_los_sex          = "all_lollipop_los_sex.rds",

  all_sentence_avg_past_pe_race = "all_sentence_avg_past_pe_race.rds",
  all_sentence_avg_past_pe_sex  = "all_sentence_avg_past_pe_sex.rds",

  all_sentence_los_race_offense         = "all_sentence_los_race_offense.rds",
  all_sentence_los_sex_offense          = "all_sentence_los_sex_offense.rds",
  all_scatter_los_race_offense          = "all_scatter_los_race_offense.rds",
  all_scatter_los_sex_offense           = "all_scatter_los_sex_offense.rds",

  all_sentence_avg_past_pe_race_offense = "all_sentence_avg_past_pe_race_offense.rds",
  all_sentence_avg_past_pe_sex_offense  = "all_sentence_avg_past_pe_sex_offense.rds",
  all_scatter_avg_past_pe_race_offense  = "all_scatter_avg_past_pe_race_offense.rds",
  all_scatter_avg_past_pe_sex_offense   = "all_scatter_avg_past_pe_sex_offense.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

