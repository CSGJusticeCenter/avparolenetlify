################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: November 15, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
################################################################################

# Remove states not in tool
ncrp_releases_filtered <- fnc_filter_states(ncrp_releases_not_consolidated, exclude = states_to_exclude)################ change to ncrp_releases_consolidated when complete

# ---------------------------------------------------------------------------- #
# Time Served
# ---------------------------------------------------------------------------- #

# Calculate average time served by sex and state
los_sex <- ncrp_releases_filtered |>
  filter(sex != "Unknown") |>
  group_by(state, sex, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)

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
            .groups = "drop") |>
  fnc_filter_by_year(which_overall_year)


all_sentence_los_race <-
  fnc_generate_disparity_sentences(df = los_race,
                                   type = "in prison",
                                   compare_var = "race",
                                   los_col = "average_los")

all_sentence_los_sex <-
  fnc_generate_disparity_sentences(df = los_sex,
                                   type = "in prison",
                                   compare_var = "sex",
                                   los_col = "average_los")

# ---------------------------------------------------------------------------- #
# Years Past Parole Eligibility
# ---------------------------------------------------------------------------- #

ncrp_past_pe <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                     exclude = states_to_exclude,
                                                     dont_filter = states_nofilter) |>
  filter(parelig_status == "Current")

# Get average time between PE and release by state and race
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

# Get average time between PE and release by state and sex
avg_past_pe_sex <- ncrp_past_pe |>
  filter(!is.na(sex)) |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")|>
  fnc_filter_by_year(which_overall_year)

all_sentence_avg_past_pe_race <-
  fnc_generate_disparity_sentences(df = avg_past_pe_race,
                                   type = "past parole eligibility",
                                   compare_var = "race",
                                   los_col = "avg_years_to_estimated_pey")

all_sentence_avg_past_pe_sex <-
  fnc_generate_disparity_sentences(df = avg_past_pe_sex,
                                   type = "past parole eligibility",
                                   compare_var = "sex",
                                   los_col = "avg_years_to_estimated_pey")




# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  avg_past_pe_race              = "avg_past_pe_race.rds",
  avg_past_pe_sex               = "avg_past_pe_sex.rds",

  all_sentence_los_race         = "all_sentence_los_race.rds",
  all_sentence_los_sex          = "all_sentence_los_sex.rds",
  all_sentence_avg_past_pe_race = "all_sentence_avg_past_pe_race.rds",
  all_sentence_avg_past_pe_sex  = "all_sentence_avg_past_pe_sex.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

