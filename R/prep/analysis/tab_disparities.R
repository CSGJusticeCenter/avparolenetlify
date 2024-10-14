
# ---------------------------------------------------------------------------- #
# Time Served - Sentences
# ---------------------------------------------------------------------------- #

# Calculate average time served by race, ethnicity, and state
los_race <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic")) |>
  group_by(state, race) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(los_race$state)

# Generate sentence for each state
all_sentence_los_race <- map(.x = states, .f = function(x) {
  sentences <- fnc_disparities_sentences(state_var = x,
                                         df = los_race,
                                         type = "in prison",
                                         compare_var = "race",
                                         los_col = "average_los")
  return(sentences)
})
# Assign state names to list
all_sentence_los_race <- setNames(all_sentence_los_race, states)
all_sentence_los_race$Georgia
rm(states)

# Calculate average time served by sex and state
los_sex <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(sex != "Unknown") |>
  group_by(state, sex) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(los_sex$state)

# Generate sentence for each state
all_sentence_los_sex <- map(.x = states, .f = function(x) {
  sentences <- fnc_disparities_sentences(state_var = x,
                                         df = los_sex,
                                         type = "in prison",
                                         compare_var = "sex",
                                         los_col = "average_los")

  return(sentences)
})
# Assign state names to list
all_sentence_los_sex <- setNames(all_sentence_los_sex, states)
all_sentence_los_sex$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Time Served - Lollipop Charts
# ---------------------------------------------------------------------------- #

# Generate charts by race
states_race <- unique(los_race$state)
all_lollipop_los_race <- map(.x = states_race, .f = function(x) {
  create_lollipop_chart(
    df = los_race,
    group_var = "race",
    group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
    colors = c(color1, color4, color2),
    state_name = x
  )
})
all_lollipop_los_race <- setNames(all_lollipop_los_race, states_race)
all_lollipop_los_race$Georgia

# Generate charts by sex
states_sex <- unique(los_sex$state)
all_lollipop_los_sex <- map(.x = states_sex, .f = function(x) {
  create_lollipop_chart(
    df = los_sex,
    group_var = "sex",
    group_labels = c("Male", "Female", NA),
    colors = c(color4, color2, NA), # Only 2 colors needed
    state_name = x,
    height = 100 # Different height for sex charts
  )
})
all_lollipop_los_sex <- setNames(all_lollipop_los_sex, states_sex)
all_lollipop_los_sex$Georgia


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility - Sentences
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
# Factor sex
ncrp_current_pe <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  filter(rptyear == select_year &
         parelig_status == "Current" &
           race %in% c("White, non-Hispanic",
                     "Hispanic, any race",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "Hispanic, any race",
                                  "White, non-Hispanic")),
         years_to_estimated_pey = abs(years_to_estimated_pey))

# Get average time between PE and release by state and sex
avg_current_pe_sex <- ncrp_current_pe |>
  filter(!is.na(sex)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(avg_current_pe_sex$state)

# Generate sentence for each state
all_sentence_avg_past_pe_sex <- map(.x = states, .f = function(x) {
  sentences <- fnc_disparities_sentences(state_var = x,
                                         df = avg_current_pe_sex,
                                         type = "past parole eligibility",
                                         compare_var = "sex",
                                         los_col = "avg_years_to_estimated_pey")
  return(sentences)
})

# Assign state names to list
all_sentence_avg_past_pe_sex <- setNames(all_sentence_avg_past_pe_sex, states)
all_sentence_avg_past_pe_sex$Georgia
rm(states)


# Get average time between PE and release by state and race
avg_current_pe_race <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(!is.na(race)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(avg_current_pe_race$state)

# Generate sentence for each state
all_sentence_avg_past_pe_race <- map(.x = states, .f = function(x) {
  sentences <- fnc_disparities_sentences(state_var = x,
                                         df = avg_current_pe_race,
                                         type = "past parole eligibility",
                                         compare_var = "race",
                                         los_col = "avg_years_to_estimated_pey")
  return(sentences)
})

# Assign state names to list
all_sentence_avg_past_pe_race <- setNames(all_sentence_avg_past_pe_race, states)
all_sentence_avg_past_pe_race$Georgia
rm(states)



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
  all_sentence_avg_past_pe_sex  = "all_sentence_avg_past_pe_sex.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))


