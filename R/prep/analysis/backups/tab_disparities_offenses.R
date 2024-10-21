
# ---------------------------------------------------------------------------- #
# Time Served by Offense and Race
# ---------------------------------------------------------------------------- #

# Filter releases to states we want to include
# and select year
ncrp_releases_disparities <-
  fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year)

# Average time served by race and offense
los_race_by_offense_type <- fnc_calc_los_by_var(
  df = ncrp_releases_disparities |> fnc_filter_exclude_high_missing_race(states_with_high_missing_race),
  var = "race",
  filter_values = c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"),
  time_var = "time_between_admission_release") |>
  mutate(avg_los = avg_time)

# Time served by race and offense
# SENTENCE:  "This chart shows the average time spent in prison past parole
#             eligibility by offense type and race in 2020. The largest disparity
#             was observed among robbery offenses, where Hispanic people spent an
#             average of 3.6 more years in prison compared to White people."
all_sentence_los_race_offense <- fnc_generate_offense_disparity_sentence(los_race_by_offense_type, "race", "avg_time")
all_sentence_los_race_offense$Georgia

# VISUALIZATION: Average Time Served by Race, Ethnicity and Offense
all_scatter_los_race_offense <- fnc_create_scatter_charts_by_state(
  df = los_race_by_offense_type,
  group_var = "race",
  measure = "avg_los",
  group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
  colors = c(color1, color4, color2)
)
all_scatter_los_race_offense$Georgia




# ---------------------------------------------------------------------------- #
# Time Served by Offense and Sex
# ---------------------------------------------------------------------------- #

# Average time served by sex and offense
los_sex_by_offense_type <- fnc_calc_los_by_var(
    df = ncrp_releases_disparities, var = "sex",
    filter_values = c("Male", "Female"),
    time_var = "time_between_admission_release") |>
  mutate(avg_los = avg_time)

# Time served by race and offense
# SENTENCE:  "This chart shows the average time spent in prison past parole
#             eligibility by offense type and sex in 2020. The largest disparity
#             was observed among murder or nonnegligent manslaughter offenses,
#             where males spent an average of 3.6 more years in prison
#             compared to females."
all_sentence_los_sex_offense <- fnc_generate_offense_disparity_sentence(los_sex_by_offense_type, "sex", "avg_time")
all_sentence_los_sex_offense$Georgia

# VISUALIZATION: Average Time Served by Sex and Offense
all_scatter_los_sex_offense <- fnc_create_scatter_charts_by_state(
  df = los_sex_by_offense_type,
  group_var = "sex",
  measure = "avg_los",
  group_labels = c("Male", "Female"),
  colors = c(color4, color2)
)
all_scatter_los_sex_offense$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
ncrp_current_pe <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  filter(rptyear == select_year &
         parelig_status == "Current" &
         !is.na(fbi_index) & fbi_index != "Unknown" & !is.na(years_to_estimated_pey))

# Average time past parole eligibility as of select_year by race and offense
avg_current_pe_offense_race <- fnc_calc_los_by_var(
  df = ncrp_current_pe |> fnc_filter_exclude_high_missing_race(states_with_high_missing_race),
  var = "race",
  filter_values = c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"),
  time_var = "years_to_estimated_pey") |>
  mutate(avg_time_past_pe = avg_time)

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and race and ethnicity in 2020.
#            The largest disparity was observed among negligent manslaughter
#            offenses, where Hispanic people spent an average of 2.2 more years
#            in prison compared to White people."
all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence(avg_current_pe_offense_race, "race", "avg_time_past_pe")
all_sentence_avg_past_pe_race_offense$Georgia

# VISUALIZATION: Average Time Past Parole Eligibility by Race, Ethnicity, and Offense
all_scatter_avg_past_pe_race_offense <- fnc_create_scatter_charts_by_state(
  df = avg_current_pe_offense_race,
  group_var = "race",
  measure = "avg_time_past_pe",
  group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
  colors = c(color1, color4, color2)
)
all_scatter_los_race_offense$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex
# ---------------------------------------------------------------------------- #

# Average time past parole eligibility as of select_year by sex and offense
avg_current_pe_offense_sex <- fnc_calc_los_by_var(
  df = ncrp_current_pe,
  var = "sex",
  filter_values = c("Male", "Female"),
  time_var = "years_to_estimated_pey") |>
  mutate(avg_time_past_pe = avg_time)

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and sex in 2020. The largest disparity
#            was observed among rape or sexual assault offenses, where males
#            spent an average of 2.6 more years in prison compared to females."
all_sentence_avg_past_pe_sex_offense <- fnc_generate_offense_disparity_sentence(avg_current_pe_offense_sex, "sex", "avg_time_past_pe")
all_sentence_avg_past_pe_sex_offense$Georgia

# VISUALIZATION: Avergae Time Past Parole Eligibility by Sex and Offense
all_scatter_avg_past_pe_sex_offense <- fnc_create_scatter_charts_by_state(
    df = avg_current_pe_offense_sex,
    group_var = "sex",
    measure = "avg_time_past_pe",
    group_labels = c("Male", "Female"),
    colors = c(color4, color2)
  )
all_scatter_avg_past_pe_sex_offense$Georgia




# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
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












