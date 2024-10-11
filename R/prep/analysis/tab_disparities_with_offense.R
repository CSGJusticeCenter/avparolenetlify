
# ---------------------------------------------------------------------------- #
# Time Served by Offense and Race
# ---------------------------------------------------------------------------- #

# Calculate the average time served by race, state, and by offense type
los_race_by_offense_type <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(race %in% c("White, non-Hispanic",
                     "Hispanic, any race",
                     "Black, non-Hispanic")) |>
  group_by(state, race, fbi_index) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE),
    people_released = n(),
    .groups = "drop") |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race")))

# Get unique states to iterate over
states <- unique(los_race_by_offense_type$state)

# SENTENCE: "The largest disparity was observed among X offenses, where
#            GROUP people spent X more years in prison on average compared
#            White people."
# Generate sentence for each state
all_sentence_los_race_offense <- map(.x = states, .f = function(x) {

  df1 <- los_race_by_offense_type |>
    filter(state == x & fbi_index != "Unknown")

  # Handling missing data
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Calculate the difference in average LOS between the races for each offense type
  df_disparity <- df1 %>%
    group_by(fbi_index) %>%
    reframe(
      max_los = max(average_los),
      min_los = min(average_los),
      diff_los = max_los - min_los,
      race_longest = race[which.max(average_los)],
      race_shortest = race[which.min(average_los)]
    ) %>%
    arrange(desc(diff_los))

  # Use case_when() to replace race descriptions directly
  df_disparity <- df_disparity %>%
    mutate(
      race_longest = case_when(
        race_longest == "Black, non-Hispanic" ~ "Black",
        race_longest == "White, non-Hispanic" ~ "White",
        race_longest == "Hispanic, any race" ~ "Hispanic",
        TRUE ~ race_longest
      ),
      race_shortest = case_when(
        race_shortest == "Black, non-Hispanic" ~ "Black",
        race_shortest == "White, non-Hispanic" ~ "White",
        race_shortest == "Hispanic, any race" ~ "Hispanic",
        TRUE ~ race_shortest
      )
    )

  # Filter for Black and Hispanic disparities where White people had shorter LOS
  df_disparity_filtered <- df_disparity %>%
    filter(race_shortest == "White" & race_longest %in% c("Black", "Hispanic"))

  # If no disparities exist, return the default message
  if (nrow(df_disparity_filtered) == 0) {
    return("This chart shows the average time served in prison by offense type and by race and ethnicity in 2020.")
  }

  # Remove "Other Violent Offenses" if it has the largest disparity
  if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
    df_disparity_filtered <- df_disparity_filtered %>% slice(2)
  }

  # Get the largest remaining disparity
  largest_disparity <- df_disparity_filtered %>% slice(1)

  # Extract values for the sentence
  offense_type <- largest_disparity$fbi_index
  race_longest <- largest_disparity$race_longest
  los_longest <- round(largest_disparity$max_los, 1)
  race_shortest <- largest_disparity$race_shortest
  los_shortest <- round(largest_disparity$min_los, 1)
  disparity_diff <- round(largest_disparity$diff_los, 1)

  # Construct the sentence without the "shortest time served" part
  sentence <- paste0(
    "This chart shows the average time served in prison by offense type and by race and ethnicity in 2020. ",
    "The largest disparity was observed among ", tolower(offense_type), " offenses, where ", race_longest,
    " people spent an average of ", disparity_diff, " more years in prison compared to White people."
  )

  return(sentence)
})

# Assign state names to list
all_sentence_los_race_offense <- setNames(all_sentence_los_race_offense, states)

# Example access:
all_sentence_los_race_offense$Georgia





# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_los_race_offense = "all_sentence_los_race_offense.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
