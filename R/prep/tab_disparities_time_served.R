#######################################
# Project: AV Parole
# File: tab_disparities_time_served.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#######################################

# ---------------------------------------------------------------------------- #
# Time Served by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Calculate average time served by race, ethnicity, and state
los_race <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(race != "Unknown") |>
  group_by(state, race) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# Get states with data
states <- unique(los_race$state)

# SENTENCE: "In YEAR, Black people spent X more years on average in prison, and Hispanic people
# spent X more years on average in prison compared to White people."
# Generate sentence for each state
all_sentence_los_race <- map(.x = states, .f = function(x) {

  df1 <- los_race |>
    ungroup() |>
    mutate(race = case_when(
      race == "White, non-Hispanic" ~ "White",
      race == "Black, non-Hispanic" ~ "Black",
      race == "Hispanic, any race" ~ "Hispanic",
      race == "Other race(s), non-Hispanic" ~ "Other races"
    )) |>
    filter(state == x)

  # Handling missing data
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Focus on comparisons with White people
  df_white <- df1 |> filter(race == "White")

  # Initialize variables to hold parts of the sentence
  black_sentence <- ""
  hispanic_sentence <- ""

  # Generate sentence for Black comparison
  df_black <- df1 |> filter(race == "Black")
  if (nrow(df_black) > 0 && nrow(df_white) > 0) {
    los_diff_black <- df_black$average_los - df_white$average_los
    if (!is.na(los_diff_black) && los_diff_black > 0) {
      black_sentence <- paste0("In ", select_year, ", Black people spent ", round(los_diff_black, 1), " more years on average in prison")
    }
  }

  # Generate sentence for Hispanic comparison
  df_hispanic <- df1 |> filter(race == "Hispanic")
  if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
    los_diff_hispanic <- df_hispanic$average_los - df_white$average_los
    if (!is.na(los_diff_hispanic) && los_diff_hispanic > 0) {
      hispanic_sentence <- paste0("Hispanic people spent ", round(los_diff_hispanic, 1), " more years on average in prison")
    }
  }

  # Combine the sentences properly, with commas and "and" when both exist
  if (black_sentence != "" && hispanic_sentence != "") {
    sentence <- paste0(black_sentence, ", and ", hispanic_sentence, " compared to White people.")
  } else if (black_sentence != "") {
    sentence <- paste0(black_sentence, " compared to White people.")
  } else if (hispanic_sentence != "") {
    sentence <- paste0(hispanic_sentence, " compared to White people.")
  } else {
    sentence <- ""
  }

  return(sentence)
})

# Set names for the list elements
all_sentence_los_race <- setNames(all_sentence_los_race, states)
all_sentence_los_race$Georgia
rm(states) # remove states



# Get unique states with data
states <- unique(los_race$state)

# VISUALIZATION: Lollipop showing avg time served by race and ethnicity
# Generate chart for each state
all_lollipop_los_race <- map(.x = states, .f = function(x) {

  df1 <- los_race |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(average_los)) |>
    mutate(race_num = row_number(),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2,
             race == "Other race(s), non-Hispanic" ~ color5
           ))

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$race), df1$race_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(race_num, start_x, end_x, race)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = race_num, group = race),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Other race(s), non-Hispanic - triangle
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(race == "Other race(s), non-Hispanic"),
      type = 'scatter',
      color = color5,
      hcaes(x = average_los, y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "triangle"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Add scatter series for each race with the appropriate marker symbol
  # White, non-Hispanic - square
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(race == "White, non-Hispanic"),
      type = 'scatter',
      color = color1,
      hcaes(x = average_los, y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "square"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Black, non-Hispanic - circle
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(race == "Black, non-Hispanic"),
      type = 'scatter',
      color = color4,
      hcaes(x = average_los, y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "circle"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Hispanic, any race - diamond
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(race == "Hispanic, any race"),
      type = 'scatter',
      color = color2,
      hcaes(x = average_los, y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "diamond"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Add y-axis and x-axis customization
  highcharts <- highcharts |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      minorGridLineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = 150)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_los_race <- setNames(all_lollipop_los_race, states)
all_lollipop_los_race$Georgia




# ---------------------------------------------------------------------------- #
# Time Served by Sex
# ---------------------------------------------------------------------------- #

# Calculate average time served by sex and state
los_sex <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(sex != "Unknown") |>
  group_by(state, sex) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(los_sex$state)

# SENTENCE: "In YEAR, females spent X year(s) fewer./more on average in prison compared to males in STATE."
# Generate sentence for each state
all_sentence_los_sex <- map(.x = states, .f = function(x) {

  df1 <- los_sex |>
    ungroup() |>
    filter(state == x)

  # Handling missing data
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Focus on comparisons with males
  df_male <- df1 |> filter(sex == "Male")

  # Initialize variable to hold the sentence
  female_sentence <- ""

  # Generate sentence for female comparison
  df_female <- df1 |> filter(sex == "Female")
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    los_diff_female <- df_female$average_los - df_male$average_los
    if (!is.na(los_diff_female)) {
      abs_los_diff_female <- round(abs(los_diff_female), 1)
      if (los_diff_female > 0) {
        female_sentence <- paste0("In ", select_year, ", females spent ",
                                  abs_los_diff_female,
                                  if (abs_los_diff_female == 1) " year more" else " years more",
                                  " on average in prison compared to males in ", x, ".")
      } else if (los_diff_female < 0) {
        female_sentence <- paste0("In ", select_year, ", females spent ",
                                  abs_los_diff_female,
                                  if (abs_los_diff_female == 1) " year fewer" else " years fewer",
                                  " on average in prison compared to males in ", x, ".")
      }
    }
  }

  # Return the sentence or a no disparity message
  if (female_sentence != "") {
    return(female_sentence)
  } else {
    return(paste0("In ", select_year, ", females and males spent the same number of years on average in prison in ", x, "."))
  }
})

# Set names for the list elements
all_sentence_los_sex <- setNames(all_sentence_los_sex, states)
all_sentence_los_sex$Georgia
rm(states)

# Get unique states to iterate over
states <- unique(los_sex$state)

# VISUALIZATION: Lollipop showing avg time served by sex
# Generate chart for each state
all_lollipop_los_sex <- map(.x = states, .f = function(x) {

  df1 <- los_sex |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(average_los)) |>
    mutate(sex_num = row_number(),
           color = case_when(
             sex == "Female" ~ color2,
             sex == "Male" ~ color4
           ))

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$sex), df1$sex_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(sex_num, start_x, end_x, sex)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = sex_num, group = sex),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Add scatter series for each sex with the appropriate marker symbol
  # Male - square
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(sex == "Male"),
      type = 'scatter',
      color = color4,
      hcaes(x = average_los, y = sex_num, group = sex, name = sex),
      marker = list(
        radius = 5,
        symbol = "square"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Female - circle
  highcharts <- highcharts |>
    hc_add_series(
      df1 %>% filter(sex == "Female"),
      type = 'scatter',
      color = color2,
      hcaes(x = average_los, y = sex_num, group = sex, name = sex),
      marker = list(
        radius = 5,
        symbol = "circle"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    )

  # Add y-axis and x-axis customization
  highcharts <- highcharts |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      minorGridLineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = 100) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_los_sex <- setNames(all_lollipop_los_sex, states)
all_lollipop_los_sex$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Time Served by Offense and Race
# ---------------------------------------------------------------------------- #

# Calculate the average time served by race, state, and by offense type
los_race_by_offense_type <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(race != "Unknown") |>
  group_by(state, race, fbi_index) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE),
    people_released = n(),
    .groups = "drop") |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))

# Get unique states
states <- unique(los_race_by_offense_type$state)

# SENTENCE: "The largest disparity was observed among X offenses, where
#            GROUP people spent X more years in prison on average compared
#            to GROUP people, who had the shortest time served for these offenses."
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
        race_longest == "Other race(s), non-Hispanic" ~ "people of Other race(s)",
        race_longest == "Hispanic, any race" ~ "Hispanic",
        TRUE ~ race_longest
      ),
      race_shortest = case_when(
        race_shortest == "Black, non-Hispanic" ~ "Black",
        race_shortest == "White, non-Hispanic" ~ "White",
        race_shortest == "Other race(s), non-Hispanic" ~ "people of Other race(s)",
        race_shortest == "Hispanic, any race" ~ "Hispanic",
        TRUE ~ race_shortest
      )
    )

  # Filter out disparities where White people have the longest LOS
  df_disparity_filtered <- df_disparity %>% filter(race_longest != "White")

  # If no non-White disparities exist, return a message
  if (nrow(df_disparity_filtered) == 0) {
    # return(paste0("No disparities involving non-White people found for ", x))
    return(paste0(""))
  }

  # Get the largest non-White disparity
  largest_disparity <- df_disparity_filtered %>% slice(1)

  # Extract values for the sentence
  offense_type <- largest_disparity$fbi_index
  race_longest <- largest_disparity$race_longest
  los_longest <- round(largest_disparity$max_los, 1)
  race_shortest <- largest_disparity$race_shortest
  los_shortest <- round(largest_disparity$min_los, 1)
  disparity_diff <- round(largest_disparity$diff_los, 1)

  # Construct the sentence with special handling for "people of Other race(s)"
  sentence <- paste0(
    "This chart shows the average time served in prison for the most serious sentenced offense by race and ethnicity in 2020. ",
    "The largest disparity was observed among ", tolower(offense_type), " offenses, where ", race_longest,
    " people spent ", disparity_diff, " more years in prison on average compared to ",
    if (race_shortest == "people of Other race(s)") {
      race_shortest
    } else {
      paste0(race_shortest, " people")
    },
    ", who had the shortest time served for these offenses."
  )

  return(sentence)
})

# Set names for the list elements
all_sentence_los_race_offense <- setNames(all_sentence_los_race_offense, states)
all_sentence_los_race_offense$Georgia
rm(states)



# Get unique states
states <- unique(los_race_by_offense_type$state)

# Generate chart for each state
all_scatter_los_race_offense <- map(.x = states, .f = function(x) {

  df1 <- los_race_by_offense_type |>
    ungroup() |>
    filter(state == x & fbi_index != "Unknown")|>
    mutate(fbi_index_num = as.numeric(as.factor(fbi_index)),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2,
             race == "Other race(s), non-Hispanic" ~ color5
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
      labels = fnc_xaxis_labels,
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "black",
      title = list(text = "Average Time Served (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = "Average Time Served by Offense and Race and Ethnicity") |>
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

# Name the list of charts by state
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
    # return(paste0("No disparities found for ", x))
    return(paste0(""))
  }

  # Get the largest disparity
  largest_disparity <- df_disparity %>% slice(1)

  # Extract values for the sentence
  offense_type <- largest_disparity$fbi_index
  sex_longest <- largest_disparity$sex_longest
  los_longest <- round(largest_disparity$max_los, 1)
  sex_shortest <- largest_disparity$sex_shortest
  los_shortest <- round(largest_disparity$min_los, 1)
  disparity_diff <- round(largest_disparity$diff_los, 1)

  # Construct the sentence
  sentence <- paste0(
    "This chart shows the average time served in prison for the most serious sentenced offense by gender in 2020. ",
    "For ", tolower(offense_type), " offenses, ", tolower(sex_longest),
    "s spent ", disparity_diff, " more years in prison on average compared to ",
    tolower(sex_shortest), "s."
  )

  return(sentence)
})

# Set names for the list elements
all_sentence_los_sex_offense <- setNames(all_sentence_los_sex_offense, states)
all_sentence_los_sex_offense$Georgia
rm(states)



# Get unique states
states <- unique(los_sex_by_offense_type$state)

# Generate chart for each state
all_scatter_los_sex_offense <- map(.x = states, .f = function(x) {

  df1 <- los_sex_by_offense_type |>
    ungroup() |>
    filter(state == x & fbi_index != "Unknown")|>
    mutate(fbi_index_num = as.numeric(as.factor(fbi_index)),
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
      labels = fnc_xaxis_labels,
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "black",
      title = list(text = "Average Time Served (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = "Average Time Served by Offense and Sex") |>
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

# Name the list of charts by state
all_scatter_los_sex_offense <- setNames(all_scatter_los_sex_offense, states)
all_scatter_los_sex_offense$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

save(all_sentence_los_race,                    file = file.path(app_folder, "all_sentence_los_race.rds"))
save(all_lollipop_los_race,                    file = file.path(app_folder, "all_lollipop_los_race.rds"))

save(all_sentence_los_sex,                     file = file.path(app_folder, "all_sentence_los_sex.rds"))
save(all_lollipop_los_sex,                     file = file.path(app_folder, "all_lollipop_los_sex.rds"))

save(all_sentence_los_race_offense,            file = file.path(app_folder, "all_sentence_los_race_offense.rds"))
save(all_scatter_los_race_offense,             file = file.path(app_folder, "all_scatter_los_race_offense.rds"))

save(all_sentence_los_sex_offense,             file = file.path(app_folder, "all_sentence_los_sex_offense.rds"))
save(all_scatter_los_sex_offense,              file = file.path(app_folder, "all_scatter_los_sex_offense.rds"))





