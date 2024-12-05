################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Last Updated: December 5, 2024 (MAR)
# Description:
#   This script analyzes and visualizes disparities in race, ethnicity, and sex,
#   focusing on two key areas: time served and years past parole eligibility.
#   The analysis highlights disparities by race, ethnicity, sex, and offense type.
#
#   - Filtering and summarizing data to calculate average time served and
#     years past parole eligibility.
#   - Generating summary sentences for disparities across states.
#   - Creating lollipop charts, scatter charts, and other visualizations
#     for detailed insights.
#   - Saving processed data and visual outputs for use in reports.
################################################################################

# ---------------------------------------------------------------------------- #
# Prepare Data for Sentences and Visualizations
# Time Served Overall and by Offense
# Years Past Parole Eligibility Overall and by Offense
# ---------------------------------------------------------------------------- #

# Define the desired order for offense categories in charts
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

# Filter NCRP releases data and order offense categories
ncrp_releases_filtered <- ncrp_releases_not_consolidated |>  ################ Change to ncrp_releases_consolidated when complete
  filter(!state %in% states_to_exclude$state) |>  # Exclude states with high missingness or abolished parole
  mutate(fbi_index = factor(fbi_index, levels = desired_order))  # Set factor levels for offense categories



#------------------------------------------------------------------------------#
# Average Time Served Calculations
#------------------------------------------------------------------------------#

# Calculate average time served by race, ethnicity, and state
los_race <- ncrp_releases_filtered |>
  # Exclude states with high missingness for race and ethnicity
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown") |>  # Exclude Unknown
  filter(
    # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
    state %in% states_use_other_race_eth$state |
      (!state %in% states_use_other_race_eth$state &
         race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"))) |>
  group_by(state, race, rptyear) |>
  summarise(
    # Calculate average length of stay
    average_los = mean(time_between_admission_release, na.rm = TRUE),
    people = n(),
    .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

# Calculate average time served by offense type, race, and ethnicity
los_race_by_offense_type <- ncrp_releases_filtered |>
  # Exclude states with high missingness for race and ethnicity
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
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
  filter(sex != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
  group_by(state, sex, fbi_index, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            people = n(),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

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
avg_past_pe_race <- ncrp_past_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race != "Unknown") |>
  # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
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
  # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
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
  filter(sex != "Unknown" & fbi_index != "Unknown" & fbi_index != "Other or Unspecified") |>
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
# SENTENCE: Black people and Hispanic people spend more time behind bars than White people.
#           Black people spent on average 10 more months in prison, and Hispanic people spent on
#           average 9 more months in prison compared to White people."
all_sentence_los_race <-
  fnc_generate_disparity_sentences(df = los_race,
                                   type = "in prison",
                                   compare_var = "race",
                                   los_col = "average_los")
# Example state:
all_sentence_los_race$Georgia

# Generate sentence about average time served sentence by sex
# SENTENCE: Females released spent on average 1.2 less years in prison compared to males."
all_sentence_los_sex <-
  fnc_generate_disparity_sentences(df = los_sex,
                                   type = "in prison",
                                   compare_var = "sex",
                                   los_col = "average_los")
# Example state:
all_sentence_los_sex$Georgia

# Generate lollipop charts of time served by race and ethnicity
all_lollipop_los_race <- fnc_generate_lollipop_charts(
  df = los_race,
  compare_var = "race"
)

# Example states:
all_lollipop_los_race$Georgia
all_lollipop_los_race$Hawaii

# Generate lollipop charts of time served by sex
all_lollipop_los_sex <- fnc_generate_lollipop_charts(
  df = los_sex,
  compare_var = "sex"
)

# Example states:
all_lollipop_los_sex$Georgia
all_lollipop_los_sex$Louisiana

# Time served by race and offense
# SENTENCE:  "The chart below shows the average time served in prison by offense
#             type and race and ethnicity. The largest disparity was observed among
#             robbery offenses, where Hispanic people spent on average 1.7 more
#             years in prison compared to White people."
all_sentence_los_race_offense <- fnc_generate_offense_disparity_sentence(los_race_by_offense_type,
                                                                         "race",
                                                                         "average_los")
# Example state:
all_sentence_los_race_offense$Georgia

# Time served by race and offense
# SENTENCE: "The chart below shows the average time served in prison by offense
#            type and sex. The largest disparity was observed among murder or
#            nonnegligent manslaughter offenses, where males spent on average 2.5
#            more years in prison compared to females."
all_sentence_los_sex_offense <- fnc_generate_offense_disparity_sentence(los_sex_by_offense_type,
                                                                        "sex",
                                                                        "average_los")
# Example state:
all_sentence_los_sex_offense$Georgia
all_sentence_los_sex_offense$Louisiana

# Create scatter charts for average time served by race, ethnicity, and offense
all_scatter_los_race_offense <- fnc_create_scatter_charts_by_state(
  df = los_race_by_offense_type,
  group_var = "race",
  measure = "average_los",
  source1 = ncrp_source
)

# Example state:
all_scatter_los_race_offense$Georgia
all_scatter_los_race_offense$Hawaii

# Create scatter charts for average time served by sex and offense
all_scatter_los_sex_offense <- fnc_create_scatter_charts_by_state(
  df = los_sex_by_offense_type,
  group_var = "sex",
  measure = "average_los",
  source1 = ncrp_source
)

# Example state:
all_scatter_los_sex_offense$Georgia
all_scatter_los_sex_offense$Louisiana



# ---------------------------------------------------------------------------- #
# Years Past Parole Eligibility
# ---------------------------------------------------------------------------- #

# Generate sentence about average time served sentence by race and ethnicity
# SENTENCE: Black people and Hispanic people spend more time behind bars after
#           becoming eligible for parole than White people. Black people spent on
#           average 9 more months past parole eligibility, and Hispanic people spent
#           on average 9 more months past parole eligibility compared to White people.
all_sentence_avg_past_pe_race <-
  fnc_generate_disparity_sentences(df = avg_past_pe_race,
                                   type = "past parole eligibility",
                                   compare_var = "race",
                                   los_col = "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_race$Georgia
all_sentence_avg_past_pe_race$`South Carolina`
all_sentence_avg_past_pe_race$Hawaii

# Generate sentence about average time served sentence by sex
# SENTENCE: Females who were still incarcerated spent on average 1.7 less years
#           past parole eligibility compared to males.
all_sentence_avg_past_pe_sex <-
  fnc_generate_disparity_sentences(df = avg_past_pe_sex,
                                   type = "past parole eligibility",
                                   compare_var = "sex",
                                   los_col = "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_sex$Georgia

# SENTENCE: "The chart below shows the average time spent in prison past parole
#            eligibility by offense type and race and ethnicity. The largest
#            disparity was observed among negligent manslaughter offenses, where
#            Hispanic people spent on average 1.9 more years in prison compared to
#            White people."
all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence(avg_past_pe_race_by_offense_type,
                                                                                 "race",
                                                                                 "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_race_offense$Georgia

# SENTENCE: "The chart below shows the average time spent in prison past parole
#            eligibility by offense type and sex. The largest disparity was
#            observed among rape or sexual assault offenses, where males spent
#            on average 1.6 more years in prison compared to females"
all_sentence_avg_past_pe_sex_offense <- fnc_generate_offense_disparity_sentence(avg_past_pe_sex_by_offense_type,
                                                                                "sex",
                                                                                "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_sex_offense$Georgia

# Create scatter charts for average time served by race, ethnicity, and offense
all_scatter_avg_past_pe_race_offense <- fnc_create_scatter_charts_by_state(
  df = avg_past_pe_race_by_offense_type,
  group_var = "race",
  measure = "avg_years_to_estimated_pey",
  source1 = ncrp_source,
  source2 = csg_source
)

# Example state:
all_scatter_avg_past_pe_race_offense$Georgia

# Create scatter charts for average time served by sex and offense
all_scatter_avg_past_pe_sex_offense <- fnc_create_scatter_charts_by_state(
  df = avg_past_pe_sex_by_offense_type,
  group_var = "sex",
  measure = "avg_years_to_estimated_pey",
  source1 = ncrp_source,
  source2 = csg_source
)

# Example state:
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
