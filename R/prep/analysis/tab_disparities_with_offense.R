
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
all_sentence_los_race_offense$Georgia

# VISUALIZATION:
# Generate chart for each state
all_scatter_los_race_offense <- map(.x = states, .f = function(x) {

  df1 <- los_race_by_offense_type |>
    ungroup() |>
    filter(state == x & fbi_index != "Unknown")|>
    mutate(fbi_index = fct_rev(as.factor(fbi_index)),
           fbi_index_num = as.numeric(fbi_index),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2
           ))

  # Create a named vector for y-axis labels
  y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(fbi_index_num, start_x, end_x, race, fbi_index)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_add_series(
      df1 %>% filter(race == "Black, non-Hispanic"),
      type = 'scatter',
      color = color4,
      hcaes(x = average_los, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "circle"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Hispanic, any race"),
      type = 'scatter',
      color = color2,
      hcaes(x = average_los, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "diamond"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "White, non-Hispanic"),
      type = 'scatter',
      color = color1,
      hcaes(x = average_los, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "square"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Other race(s), non-Hispanic"),
      type = 'scatter',
      color = color5,
      hcaes(x = average_los, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "triangle"
      )
    ) |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      title = list(text = ""),
      labels = fnc_xaxis_labels_right,
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "white",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "white",
      title = list(text = "Average Time Served (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = paste0("Average Time Served by Offense and Race and Ethnicity, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(
      headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span> {series.name}:<br/>',
        'Offense: {point.name}<br/>',
        'Average LOS: {point.x: .1f} years<br/>',
        'People Released: {point.people_released}<br/>'
      )
    ) |>
    hc_legend(verticalAlign = "top",
              layout = "horizontal") |>
    hc_colors(c(color1, color4, color2, color5)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
# Assign state names to list
all_scatter_los_race_offense <- setNames(all_scatter_los_race_offense, states)
all_scatter_los_race_offense$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Time Served by Offense and Sex
# ---------------------------------------------------------------------------- #

# Calculate the average time served by sex, state, and offense type
los_sex_by_offense_type <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(sex != "Unknown") |>
  group_by(state, sex, fbi_index) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE),
    people_released = n(),
    .groups = "drop")

# Get unique states to iterate over
states <- unique(los_sex_by_offense_type$state)

# SENTENCE:
# Generate sentence for each state
all_sentence_los_sex_offense <- map(.x = states, .f = function(x) {

  df1 <- los_sex_by_offense_type |>
    filter(state == x & fbi_index != "Unknown")

  # Handling missing data
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Calculate the difference in average LOS between the sexes for each offense type
  df_disparity <- df1 %>%
    group_by(fbi_index) %>%
    reframe(
      max_los = max(average_los),
      min_los = min(average_los),
      diff_los = max_los - min_los,
      sex_longest = sex[which.max(average_los)],
      sex_shortest = sex[which.min(average_los)]
    ) %>%
    arrange(desc(diff_los))

  # If no disparities exist, return a message
  if (nrow(df_disparity) == 0) {
    return(paste0(""))
  }

  # Filter out "Other Violent Offenses" before determining the largest disparity
  df_disparity_filtered <- df_disparity %>%
    filter(fbi_index != "Other Violent Offenses" | nrow(df_disparity) == 1)

  # Get the largest remaining disparity
  largest_disparity <- df_disparity_filtered %>% slice(1)

  # Extract values for the sentence
  offense_type <- largest_disparity$fbi_index
  sex_longest <- largest_disparity$sex_longest
  los_longest <- round(largest_disparity$max_los, 1)
  sex_shortest <- largest_disparity$sex_shortest
  los_shortest <- round(largest_disparity$min_los, 1)
  disparity_diff <- round(largest_disparity$diff_los, 1)

  # Construct the sentence
  sentence <- paste0(
    "This chart shows the average time served in prison by offense type and sex in 2020. ",
    "For ", tolower(offense_type), " offenses, ", tolower(sex_longest),
    "s spent on average ", disparity_diff, " more years in prison compared to ",
    tolower(sex_shortest), "s."
  )

  return(sentence)
})

# Assign state names to list
all_sentence_los_sex_offense <- setNames(all_sentence_los_sex_offense, states)
all_sentence_los_sex_offense$Georgia
rm(states)





# Get unique states to iterate over
states <- unique(los_sex_by_offense_type$state)

# VISUALIZATION:
# Generate chart for each state
all_scatter_los_sex_offense <- map(.x = states, .f = function(x) {

  df1 <- los_sex_by_offense_type |>
    ungroup() |>
    filter(state == x & fbi_index != "Unknown")|>
    mutate(fbi_index = fct_rev(as.factor(fbi_index)),
           fbi_index_num = as.numeric(fbi_index),
           color = case_when(
             sex == "Female" ~ color2,
             sex == "Male" ~ color4
           ))

  # Create a named vector for y-axis labels
  y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(fbi_index_num, start_x, end_x, sex, fbi_index)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_add_theme(base_hc_theme) |>
    hc_add_series(
      df1 %>% filter(sex == "Male"),
      type = 'scatter',
      color = color4,
      hcaes(x = average_los, y = fbi_index_num, group = sex, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "square"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(sex == "Female"),
      type = 'scatter',
      color = color2,
      hcaes(x = average_los, y = fbi_index_num, group = sex, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "circle"
      )
    ) |>
    hc_yAxis(
      title = list(text = ""),
      labels = fnc_xaxis_labels_right,
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "white",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "white",
      title = list(text = "Average Time Served (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = paste0("Average Time Served by Offense and Sex, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(
      headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span> {series.name}:<br/>',
        'Offense: {point.name}<br/>',
        'Average LOS: {point.x: .1f} years<br/>',
        'People Released: {point.people_released}<br/>'
      )
    ) |>
    hc_legend(verticalAlign = "top",
              layout = "horizontal") |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
# Assign state names to list
all_scatter_los_sex_offense <- setNames(all_scatter_los_sex_offense, states)
all_scatter_los_sex_offense$Georgia
rm(states)









































































# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- unique(avg_past_pe_race$state)

# SENTENCE:
# Generate sentence for each state
all_sentence_avg_past_pe_race <- map(.x = states, .f = function(x) {

  # Data preparation: filter the dataset for the current state and rename race categories for clarity.
  df1 <- avg_past_pe_race |>
    ungroup() |>
    mutate(race = case_when(
      race == "White, non-Hispanic" ~ "White",
      race == "Black, non-Hispanic" ~ "Black",
      race == "Hispanic, any race" ~ "Hispanic"
    )) |>
    filter(state == x)  # Filter dataset for the current state.

  # Handle cases where no data is available for the selected state.
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Create a subset of the data for White individuals to use as the comparison group.
  df_white <- df1 |> filter(race == "White")

  # Initialize variables to hold parts of the sentence
  black_sentence <- ""
  hispanic_sentence <- ""

  # Generate sentence for Black comparison
  df_black <- df1 |> filter(race == "Black")
  if (nrow(df_black) > 0 && nrow(df_white) > 0) {
    los_diff_black <- df_black$average_avg_past_pe - df_white$average_avg_past_pe
    if (!is.na(los_diff_black) && los_diff_black > 0) {
      black_sentence <- paste0("In ", select_year, ", Black people spent an average of ", round(los_diff_black, 1),
                               " more years in prison past parole eligibility")
    }
  }

  # Generate sentence for Hispanic comparison
  df_hispanic <- df1 |> filter(race == "Hispanic")
  if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
    los_diff_hispanic <- df_hispanic$average_avg_past_pe - df_white$average_avg_past_pe
    if (!is.na(los_diff_hispanic) && los_diff_hispanic > 0) {
      hispanic_sentence <- paste0("Hispanic people spent an average of ", round(los_diff_hispanic, 1),
                                  " more years in prison past parole eligibility")
    }
  }

  # Combine the sentences properly, with "and" when both exist
  if (black_sentence != "" && hispanic_sentence != "") {
    sentence <- paste0(black_sentence, " and ", hispanic_sentence, " compared to White people.")
  } else if (black_sentence != "") {
    sentence <- paste0(black_sentence, " compared to White people.")
  } else if (hispanic_sentence != "") {
    sentence <- paste0(hispanic_sentence, " compared to White people.")
  } else {
    sentence <- paste0("No disparities compared to White people found for ", x, ".")
  }

  return(sentence)  # Return the generated sentence for the state.
})
# Assign state names to list
all_sentence_avg_past_pe_race <- setNames(all_sentence_avg_past_pe_race, states)
all_sentence_avg_past_pe_race$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity by Offense Type
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- unique(avg_past_pe_race_offense$state)

# SENTENCE:
# Generate sentence for each state
all_sentence_avg_past_pe_race_offense <- map(.x = states, .f = function(x) {

  # Filter the dataset for the current state.
  df1 <- avg_past_pe_race_offense |>
    filter(state == x & fbi_index != "Unknown")

  # Handle missing data for the state.
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Calculate the disparity in average years spent past parole eligibility across races for each offense,
  # but always compare to White, non-Hispanic individuals.
  df_disparity <- df1 %>%
    group_by(fbi_index) %>%
    reframe(
      avg_white_past_pe = avg_years[race == "White, non-Hispanic"],  # Average years for White people.
      avg_non_white_past_pe = avg_years[race != "White, non-Hispanic"],  # Average years for non-White groups.
      race_non_white = race[race != "White, non-Hispanic"],  # Race group for non-White individuals.
      disparity_diff = avg_non_white_past_pe - avg_white_past_pe  # Difference in years.
    ) %>%
    arrange(desc(disparity_diff))  # Sort by the magnitude of disparity.

  # Filter out cases where White individuals do not have shorter time than non-White groups.
  df_disparity_filtered <- df_disparity %>% filter(disparity_diff > 0)

  # If no disparities involving non-White individuals are found, return a message.
  if (nrow(df_disparity_filtered) == 0) {
    return(paste0("No disparities involving non-White people found for ", x))
  }

  # Extract the largest disparity for non-White groups.
  largest_disparity <- df_disparity_filtered %>% slice(1)

  # Extract details for the generated sentence.
  offense_type <- tolower(largest_disparity$fbi_index)  # Convert the offense type to lowercase.
  race_non_white <- largest_disparity$race_non_white
  los_white <- round(largest_disparity$avg_white_past_pe, 1)
  los_non_white <- round(largest_disparity$avg_non_white_past_pe, 1)
  disparity_diff <- round(largest_disparity$disparity_diff, 1)

  # Adjust wording for race categories for the sentence construction.
  race_non_white_adjusted <- case_when(
    race_non_white == "Hispanic, any race" ~ "Hispanic people",
    TRUE ~ paste0(race_non_white, " people")
  )

  # Construct the sentence summarizing the disparity.
  sentence <- paste0(
    "This chart shows the average years in prison past parole eligibility by offense type and race and ethnicity in 2020. ",
    "The largest disparity observed was for ", offense_type, " offenses, where ", race_non_white_adjusted,
    " had on average ", disparity_diff, " more years in prison past parole eligibility compared to White, non-Hispanic people."
  )

  return(sentence)  # Return the constructed sentence for the state.
})

# Assign state names to list
all_sentence_avg_past_pe_race_offense <- setNames(all_sentence_avg_past_pe_race_offense, states)
all_sentence_avg_past_pe_race_offense$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- unique(avg_past_pe_sex$state)

# SENTENCE:
# Generate sentence for each state
all_sentence_avg_past_pe_sex <- map(.x = states, .f = function(x) {

  # Data preparation: filter the dataset for the current state and rename sex categories for clarity.
  df1 <- avg_past_pe_sex |>
    ungroup() |>
    filter(state == x)  # Filter dataset for the current state.

  # Handle cases where no data is available for the selected state.
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Create a subset of the data for Male individuals to use as the comparison group.
  df_male <- df1 |> filter(sex == "Male")

  # Initialize variables to hold parts of the sentence
  female_sentence <- ""

  # Generate sentence for Female comparison
  df_female <- df1 |> filter(sex == "Female")
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    los_diff_female <- df_female$average_avg_past_pe - df_male$average_avg_past_pe
    if (!is.na(los_diff_female) && los_diff_female > 0) {
      female_sentence <- paste0("Females spent an average of ", round(los_diff_female, 1),
                                " more years in prison past parole eligibility compared to males.")
    } else if (!is.na(los_diff_female) && los_diff_female < 0) {
      female_sentence <- paste0("Females spent an average of ", round(abs(los_diff_female), 1),
                                " fewer years in prison past parole eligibility compared to males.")
    }
  }

  # Return the generated sentence for the state.
  if (female_sentence != "") {
    return(female_sentence)
  } else {
    return(paste0("No significant disparities found between males and females for ", x, "."))
  }
})
# Assign state names to list
all_sentence_avg_past_pe_sex <- setNames(all_sentence_avg_past_pe_sex, states)
all_sentence_avg_past_pe_sex$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex and Offense Type
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- unique(avg_past_pe_sex_offense$state)

# SENTENCE:
# Generate sentence for each state
all_sentence_avg_past_pe_sex_offense <- map(.x = states, .f = function(x) {

  # Filter the dataset for the current state.
  df1 <- avg_past_pe_sex_offense |>
    filter(state == x & fbi_index != "Unknown")

  # Handle missing data for the state.
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x, "."))
  }

  # Calculate the disparity in average years spent past parole eligibility between sexes for each offense.
  df_disparity <- df1 %>%
    group_by(fbi_index) %>%
    reframe(
      avg_male = avg_years[sex == "Male"],     # Average years for males
      avg_female = avg_years[sex == "Female"], # Average years for females
      diff_avg_past_pe = avg_male - avg_female # Difference in average years
    ) %>%
    arrange(desc(abs(diff_avg_past_pe)))  # Sort by the absolute magnitude of disparity

  # If no comparison between males and females is possible, return a message.
  if (nrow(df_disparity) == 0) {
    return(paste0("No disparities found between males and females for ", x, "."))
  }

  # Extract the largest disparity.
  largest_disparity <- df_disparity %>% slice(1)

  # Extract details for the generated sentence.
  offense_type <- tolower(largest_disparity$fbi_index)  # Convert the offense type to lowercase.
  avg_male <- round(largest_disparity$avg_male, 1)
  avg_female <- round(largest_disparity$avg_female, 1)
  disparity_diff <- round(abs(largest_disparity$diff_avg_past_pe), 1)

  # Construct the sentence summarizing the disparity.
  if (largest_disparity$diff_avg_past_pe > 0) {
    # Males spent longer
    sentence <- paste0(
      "This chart shows the average years in prison past parole eligibility by offense type and sex in 2020. ",
      "For ", offense_type, " offenses, males spent on average ", disparity_diff,
      " more years in prison past parole eligibility compared to females."
    )
  } else {
    # Females spent longer
    sentence <- paste0(
      "This chart shows the average years in prison past parole eligibility by offense type and sex in 2020. ",
      "For ", offense_type, " offenses, females spent on average ", disparity_diff,
      " more years in prison past parole eligibility compared to males."
    )
  }

  return(sentence)  # Return the constructed sentence for the state.
})
# Assign state names to list
all_sentence_avg_past_pe_sex_offense <- setNames(all_sentence_avg_past_pe_sex_offense, states)
all_sentence_avg_past_pe_sex_offense$Georgia












# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_los_race_offense = "all_sentence_los_race_offense.rds",
  all_scatter_los_race_offense  = "all_scatter_los_race_offense.rds",
  all_sentence_los_sex_offense  = "all_sentence_los_sex_offense.rds",
  all_scatter_los_sex_offense   = "all_scatter_los_sex_offense.rds",

  all_sentence_avg_past_pe_race_offense = "all_sentence_avg_past_pe_race_offense.rds",
  # all_scatter_avg_past_pe_race_offense  = "all_scatter_avg_past_pe_race_offense.rds",
  all_sentence_avg_past_pe_sex_offense  = "all_sentence_avg_past_pe_sex_offense.rds"
  # all_scatter_avg_past_pe_sex_offense   = "all_scatter_avg_past_pe_sex_offense.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))





