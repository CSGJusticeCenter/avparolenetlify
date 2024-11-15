#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: October 14, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#    Focusing on RRIs
#######################################

# ---------------------------------------------------------------------------- #
# Time Served - Sentences
# ---------------------------------------------------------------------------- #

# Calculate time served
ncrp_releases_timeserved <- fnc_filter_population(ncrp_releases_not_consolidated,
                                                  exclude = states_to_exclude) |> #########################################
  mutate(time_between_admission_release =  as.numeric(relyr) - as.numeric(admityr))

# Calculate average time served by race, ethnicity, and state
# Remove states without parole systems and with high missingness
# (states_to_exclude created in prep/import_format.R)
los_race <- ncrp_releases_timeserved |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  # Only include these racial and ethnic groups
  filter(race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic")) |>
  group_by(state, race, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            .groups = "drop")

# SENTENCE: "In 2020, Black people spent an average of 0.7 more years in prison,
#            and Hispanic people spent an average of 1.5 more years in
#            prison compared to White people."
all_sentence_los_race <-
  fnc_generate_los_disparity_sentences(df = los_race,
                                       type = "in prison",
                                       compare_var = "race",
                                       los_col = "average_los",
                                       which_year = which_overall_year)
all_sentence_los_race$Georgia
all_sentence_los_race$Louisiana

# Calculate average time served by sex and state
los_sex <- ncrp_releases_timeserved |>
  filter(sex != "Unknown") |>
  group_by(state, sex, rptyear) |>
  summarise(average_los = mean(time_between_admission_release, na.rm = TRUE),
            .groups = "drop")

# SENTENCE: "In 2020, females spent an average of 1 year fewer in prison compared
#            to males in Georgia."
all_sentence_los_sex <-
  fnc_generate_los_disparity_sentences(df = los_sex,
                                       type = "in prison",
                                       compare_var = "sex",
                                       los_col = "average_los",
                                       which_year = which_overall_year)
all_sentence_los_sex$Georgia





# ---------------------------------------------------------------------------- #
# Time Served - Lollipop Charts
# ---------------------------------------------------------------------------- #

# Generate charts by race
states_race <- unique(los_race$state)
all_lollipop_los_race <- map(.x = states_race, .f = function(x) {
  select_year <- fnc_determine_select_year(x, which_overall_year)
  fnc_create_lollipop_chart(
    df = los_race,
    group_var = "race",
    group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
    colors = c(color1, color4, color2),
    state_name = x,
    source = ncrp_source,
    year = select_year
  )
})
all_lollipop_los_race <- setNames(all_lollipop_los_race, states_race)
all_lollipop_los_race$Georgia
all_lollipop_los_race$Louisiana

# Generate charts by sex
states_sex <- unique(los_sex$state)
all_lollipop_los_sex <- map(.x = states_sex, .f = function(x) {
  select_year <- fnc_determine_select_year(x, which_overall_year)
  fnc_create_lollipop_chart(
    df = los_sex,
    group_var = "sex",
    group_labels = c("Male", "Female"),
    colors = c(color4, color2),
    state_name = x,
    source = ncrp_source,
    year = select_year
  )
})
all_lollipop_los_sex <- setNames(all_lollipop_los_sex, states_sex)
all_lollipop_los_sex$Georgia




# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility - Sentences
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Select racial and ethnic groups of interest
ncrp_current_pe <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                     exclude = states_to_exclude,
                                                     dont_filter = states_nofilter) |> #########################################
  filter(parelig_status == "Current")

# Get average time between PE and release by state and sex
avg_current_pe_sex <- ncrp_current_pe |>
  filter(!is.na(sex)) |>
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# SENTENCE: "In 2020, females spent an average of 1.9 less years past parole
#            eligibility compared to males in Georgia."
all_sentence_avg_past_pe_sex <- fnc_generate_los_disparity_sentences(df = avg_current_pe_sex,
                                                                     type = "past parole eligibility",
                                                                     compare_var = "sex",
                                                                     los_col = "avg_years_to_estimated_pey",
                                                                     which_year = which_overall_year)
all_sentence_avg_past_pe_sex$Georgia


# Get average time between PE and release by state and race
avg_current_pe_race <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race %in% c("White, non-Hispanic",
                     "Hispanic, any race",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "Hispanic, any race",
                                  "White, non-Hispanic")),
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race, rptyear) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# SENTENCE: "In 2020, Black people spent an average of 0.7 more years past parole
#            eligibility, and Hispanic people spent an average of 0.7 more years
#            past parole eligibility compared to White people."
all_sentence_avg_past_pe_race <- fnc_generate_los_disparity_sentences(df = avg_current_pe_race,
                                                                      type = "past parole eligibility",
                                                                      compare_var = "race",
                                                                      los_col = "avg_years_to_estimated_pey",
                                                                      which_year = which_overall_year)
all_sentence_avg_past_pe_race$Georgia






# ---------------------------------------------------------------------------- #
# Time Served by Offense and Race
# ---------------------------------------------------------------------------- #

# Filter releases to states we want to include
# and select year
ncrp_releases_disparities <-
  fnc_filter_population(ncrp_releases_not_consolidated, exclude = states_to_exclude) ###############################

# Average time served by race and offense
los_race_by_offense_type <- fnc_calc_los_by_var(
  df = ncrp_releases_disparities |> fnc_filter_exclude_high_missing_race(states_with_high_missing_race),
  var = "race",
  filter_values = c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"),
  time_var = "time_between_admission_release",
  exclude = states_to_exclude) |> mutate(avg_los = avg_time)

# Time served by race and offense
# SENTENCE:  "This chart shows the average time served by offense type and race in 2020. The largest disparity
#             was observed among robbery offenses, where Hispanic people spent an
#             average of 3.6 more years in prison compared to White people."
all_sentence_los_race_offense <- fnc_generate_offense_disparity_sentence(los_race_by_offense_type,
                                                                         "race",
                                                                         "avg_los",
                                                                         which_year = which_overall_year)
all_sentence_los_race_offense$Georgia

# VISUALIZATION: Average Time Served by Race, Ethnicity and Offense
all_scatter_los_race_offense <- fnc_create_scatter_charts_by_state(
  df = los_race_by_offense_type,
  group_var = "race",
  measure = "avg_los",
  group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
  colors = c(color1, color4, color2),
  source = ncrp_source,
  which_years = which_overall_year
)
all_scatter_los_race_offense$Georgia




# ---------------------------------------------------------------------------- #
# Time Served by Offense and Sex
# ---------------------------------------------------------------------------- #

# Average time served by sex and offense
los_sex_by_offense_type <- fnc_calc_los_by_var(
  df = ncrp_releases_disparities,
  var = "sex",
  filter_values = c("Male", "Female"),
  time_var = "time_between_admission_release",
  exclude = states_to_exclude) |>
  mutate(avg_los = avg_time)

# Time served by race and offense
# SENTENCE:  "This chart shows the average time spent in prison past parole
#             eligibility by offense type and sex in 2020. The largest disparity
#             was observed among murder or nonnegligent manslaughter offenses,
#             where males spent an average of 3.6 more years in prison
#             compared to females."
all_sentence_los_sex_offense <- fnc_generate_offense_disparity_sentence(los_sex_by_offense_type,
                                                                        "sex",
                                                                        "avg_los",
                                                                        which_year = which_overall_year)
all_sentence_los_sex_offense$Georgia

# VISUALIZATION: Average Time Served by Sex and Offense
all_scatter_los_sex_offense <- fnc_create_scatter_charts_by_state(
  df = los_sex_by_offense_type,
  group_var = "sex",
  measure = "avg_los",
  group_labels = c("Male", "Female"),
  colors = c(color4, color2),
  source = ncrp_source,
  which_years = which_overall_year
)
all_scatter_los_sex_offense$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
ncrp_current_pe <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                     exclude = states_to_exclude,
                                                     dont_filter = states_nofilter) |> #########################################
  mutate(years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  filter(parelig_status == "Current" &
           !is.na(fbi_index) & fbi_index != "Unknown" & !is.na(years_to_estimated_pey))

# Average time past parole eligibility as of select_year by race and offense
avg_current_pe_offense_race <- fnc_calc_los_by_var(
  df = ncrp_current_pe |> fnc_filter_exclude_high_missing_race(states_with_high_missing_race),
  var = "race",
  filter_values = c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic"),
  time_var = "years_to_estimated_pey",
  exclude = states_to_exclude) |>
  mutate(avg_time_past_pe = avg_time)

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and race and ethnicity in 2020.
#            The largest disparity was observed among negligent manslaughter
#            offenses, where Hispanic people spent an average of 2.2 more years
#            in prison compared to White people."
all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence(avg_current_pe_offense_race,
                                                                                 "race",
                                                                                 "avg_time_past_pe",
                                                                                 which_year = which_overall_year)
all_sentence_avg_past_pe_race_offense$Georgia

# VISUALIZATION: Average Time Past Parole Eligibility by Race, Ethnicity, and Offense
all_scatter_avg_past_pe_race_offense <- fnc_create_scatter_charts_by_state(
  df = avg_current_pe_offense_race,
  group_var = "race",
  measure = "avg_time_past_pe",
  group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
  colors = c(color1, color4, color2),
  source = ncrp_csg_source,
  which_years = which_overall_year
)
all_scatter_avg_past_pe_race_offense$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex
# ---------------------------------------------------------------------------- #

# Average time past parole eligibility as of select_year by sex and offense
avg_current_pe_offense_sex <- fnc_calc_los_by_var(
  df = ncrp_current_pe,
  var = "sex",
  filter_values = c("Male", "Female"),
  time_var = "years_to_estimated_pey",
  exclude = states_to_exclude) |>
  mutate(avg_time_past_pe = avg_time)

# SENTENCE: "This chart shows the average time spent in prison past parole
#            eligibility by offense type and sex in 2020. The largest disparity
#            was observed among rape or sexual assault offenses, where males
#            spent an average of 2.6 more years in prison compared to females."
all_sentence_avg_past_pe_sex_offense <- fnc_generate_offense_disparity_sentence(avg_current_pe_offense_sex,
                                                                                "sex",
                                                                                "avg_time_past_pe",
                                                                                which_year = which_overall_year)
all_sentence_avg_past_pe_sex_offense$Georgia

# VISUALIZATION: Avergae Time Past Parole Eligibility by Sex and Offense
all_scatter_avg_past_pe_sex_offense <- fnc_create_scatter_charts_by_state(
  df = avg_current_pe_offense_sex,
  group_var = "sex",
  measure = "avg_time_past_pe",
  group_labels = c("Male", "Female"),
  colors = c(color4, color2),
  source = ncrp_csg_source,
  which_years = which_overall_year
)
all_scatter_avg_past_pe_sex_offense$Georgia




# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  avg_current_pe_race           = "avg_current_pe_race.rds",
  avg_current_pe_sex            = "avg_current_pe_sex.rds",

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

