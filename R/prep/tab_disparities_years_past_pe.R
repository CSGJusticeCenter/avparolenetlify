#######################################
# Project: AV Parole
# File: tab_disparities_years_past_pe.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#######################################

# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# Visualizations
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
# Factor race
ncrp_pe_release <- fnc_filter_pe_population_criteria(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(!is.na(time_between_ped_rptyear) &
           !is.na(estimated_pey) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0
  ) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))

# Get average time between PE and release by state and race
avg_pe_release_race <- ncrp_pe_release |>
  filter(!is.na(race)) |>
  group_by(state, race) |>
  summarise(average_avg_pe_release = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(avg_pe_release_race$state)

# Generate chart for each state
all_lollipop_avg_pe_release_race <- map(.x = states, .f = function(x) {
  df1 <- avg_pe_release_race |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(average_avg_pe_release)) |>
    mutate(race_num = row_number(),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2,
             race == "Other race(s), non-Hispanic" ~ color5
           ))

  max_los <- max(df1$average_avg_pe_release, na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$race), df1$race_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_avg_pe_release) |>
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
      hcaes(x = average_avg_pe_release, y = race_num, group = race, name = race),
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
      hcaes(x = average_avg_pe_release, y = race_num, group = race, name = race),
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
      hcaes(x = average_avg_pe_release, y = race_num, group = race, name = race),
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
      hcaes(x = average_avg_pe_release, y = race_num, group = race, name = race),
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
all_lollipop_avg_pe_release_race <- setNames(all_lollipop_avg_pe_release_race, states)
all_lollipop_avg_pe_release_race$Georgia
rm(states)

# Get average time between PE and release by state and race and offense
avg_pe_release_race_offense <- ncrp_pe_release |>
  filter(!is.na(race) & !is.na(fbi_index)) |>
  group_by(state, race, fbi_index) |>
  summarise(total_years = sum(time_between_ped_release, na.rm = TRUE),
            avg_years = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(avg_pe_release_race_offense$state)

# Generate chart for each state
all_scatter_avg_pe_release_race_offense <- map(.x = states, .f = function(x) {
  # df1 <- avg_pe_release_race_offense |>
  #   ungroup() |>
  #   filter(state == x & fbi_index != "Unknown")|>
  #   mutate(fbi_index_num = as.numeric(as.factor(fbi_index)),
  #          color = case_when(
  #            race == "White, non-Hispanic" ~ color1,
  #            race == "Black, non-Hispanic" ~ color4,
  #            race == "Hispanic, any race" ~ color2,
  #            race == "Other race(s), non-Hispanic" ~ color5
  #          ))
  df1 <- avg_pe_release_race_offense |>
    ungroup() |>
    filter(state == x & fbi_index != "Unknown") |>
    mutate(fbi_index = factor(fbi_index),  # Ensure fbi_index is a factor
           fbi_index_num = as.numeric(fbi_index),  # Convert factor to numeric
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
    mutate(start_x = 0, end_x = avg_years) |>
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
      hcaes(x = avg_years, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "circle"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Hispanic, any race"),
      type = 'scatter',
      color = color2,
      hcaes(x = avg_years, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "diamond"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "White, non-Hispanic"),
      type = 'scatter',
      color = color1,
      hcaes(x = avg_years, y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "square"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Other race(s), non-Hispanic"),
      type = 'scatter',
      color = color5,
      hcaes(x = avg_years, y = fbi_index_num, group = race, name = fbi_index),
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
      title = list(text = "Average Years Past Parole Eligibility",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = paste0("Average Years Past Parole Eligibility by Offense and Race and Ethnicity, ", select_year)) |>
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
all_scatter_avg_pe_release_race_offense <- setNames(all_scatter_avg_pe_release_race_offense, states)
all_scatter_avg_pe_release_race_offense$Georgia
all_scatter_avg_pe_release_race_offense$Arkansas
rm(states)




# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex
# Visualizations
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
# Factor sex
ncrp_pe_release <- fnc_filter_pe_population_criteria(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(!is.na(time_between_ped_rptyear) &
           !is.na(estimated_pey) &
           !is.na(relyr) &
           !is.na(sex) &
           time_between_ped_release >= 0
  )

# Get average time between PE and release by state and sex
avg_pe_release_sex <- ncrp_pe_release |>
  filter(!is.na(sex)) |>
  group_by(state, sex) |>
  summarise(average_avg_pe_release = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Get states with data
states <- unique(avg_pe_release_sex$state)

# Generate chart for each state
all_lollipop_avg_pe_release_sex <- map(.x = states, .f = function(x) {
  df1 <- avg_pe_release_sex |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(average_avg_pe_release)) |> ####### might need to adjust like with race graph
    mutate(sex_num = row_number(),
           color = case_when(
             sex == "Female" ~ color2,
             sex == "Male" ~ color4
           ))

  max_los <- max(df1$average_avg_pe_release, na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$sex), df1$sex_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_avg_pe_release) |>
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
      hcaes(x = average_avg_pe_release, y = sex_num, group = sex, name = sex),
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
      hcaes(x = average_avg_pe_release, y = sex_num, group = sex, name = sex),
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
    hc_size(height = 100)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_avg_pe_release_sex <- setNames(all_lollipop_avg_pe_release_sex, states)
all_lollipop_avg_pe_release_sex$Georgia
rm(states)

# Gstates# Get average time between PE and release by state, sex, and offense
avg_pe_release_sex_offense <- ncrp_pe_release |>
  filter(!is.na(sex) & !is.na(fbi_index)) |>
  group_by(state, sex, fbi_index) |>
  summarise(avg_years = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Get unique states to iterate over
states <- unique(avg_pe_release_sex_offense$state)

# Generate chart for each state
all_scatter_avg_pe_release_sex_offense <- map(.x = states, .f = function(x) {
  df1 <- avg_pe_release_sex_offense |>
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
    mutate(start_x = 0, end_x = avg_years) |>
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
      hcaes(x = avg_years, y = fbi_index_num, group = sex, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "square"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(sex == "Female"),
      type = 'scatter',
      color = color2,
      hcaes(x = avg_years, y = fbi_index_num, group = sex, name = fbi_index),
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
      title = list(text = "Average Years Past Parole Eligibility",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = paste0("Average Years Past Parole Eligibility by Offense and Sex, ", select_year)) |>
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
all_scatter_avg_pe_release_sex_offense <- setNames(all_scatter_avg_pe_release_sex_offense, states)
all_scatter_avg_pe_release_sex_offense$Georgia
rm(states)







# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- unique(avg_pe_release_race$state)

# Function to generate sentences summarizing racial disparities in time spent past parole eligibility.
all_sentence_avg_pe_release_race <- map(.x = states, .f = function(x) {

  # Data preparation: filter the dataset for the current state and rename race categories for clarity.
  df1 <- avg_pe_release_race |>
    ungroup() |>
    mutate(race = case_when(
      race == "White, non-Hispanic" ~ "White",
      race == "Black, non-Hispanic" ~ "Black",
      race == "Hispanic, any race" ~ "Hispanic",
      race == "Other race(s), non-Hispanic" ~ "Other races"
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
    los_diff_black <- df_black$average_avg_pe_release - df_white$average_avg_pe_release
    if (!is.na(los_diff_black) && los_diff_black > 0) {
      black_sentence <- paste0("In ", select_year, ", Black people spent an average of ", round(los_diff_black, 1),
                               " more years in prison past parole eligibility")
    }
  }

  # Generate sentence for Hispanic comparison
  df_hispanic <- df1 |> filter(race == "Hispanic")
  if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
    los_diff_hispanic <- df_hispanic$average_avg_pe_release - df_white$average_avg_pe_release
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

# Set the names of the list elements to match the state names.
all_sentence_avg_pe_release_race <- setNames(all_sentence_avg_pe_release_race, states)
all_sentence_avg_pe_release_race$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity by Offense Type
# ---------------------------------------------------------------------------- #

states <- unique(avg_pe_release_race_offense$state)
# Iterate over states to generate offense-specific summaries.
all_sentence_avg_pe_release_race_offense <- map(.x = states, .f = function(x) {

  # Filter the dataset for the current state.
  df1 <- avg_pe_release_race_offense |>
    filter(state == x & fbi_index != "Unknown")

  # Handle missing data for the state.
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Calculate the disparity in average years spent past parole eligibility across races for each offense.
  df_disparity <- df1 %>%
    group_by(fbi_index) %>%
    reframe(
      max_avg_pe_release = max(avg_years),  # Maximum years spent past parole eligibility.
      min_avg_pe_release = min(avg_years),  # Minimum years spent past parole eligibility.
      diff_avg_pe_release = max_avg_pe_release - min_avg_pe_release,  # Difference in years.
      race_longest = race[which.max(avg_years)],  # Race group with longest average years.
      race_shortest = race[which.min(avg_years)]  # Race group with shortest average years.
    ) %>%
    arrange(desc(diff_avg_pe_release))  # Sort by the magnitude of disparity.

  # Filter out cases where White individuals had the longest time spent.
  df_disparity_filtered <- df_disparity %>% filter(race_longest != "White, non-Hispanic")

  # If no disparities involving non-White individuals are found, return a message.
  if (nrow(df_disparity_filtered) == 0) {
    return(paste0("No disparities involving non-White people found for ", x))
  }

  # Extract the largest disparity for non-White groups.
  largest_disparity <- df_disparity_filtered %>% slice(1)

  # Extract details for the generated sentence.
  offense_type <- tolower(largest_disparity$fbi_index)  # Convert the offense type to lowercase.
  race_longest <- largest_disparity$race_longest
  los_longest <- round(largest_disparity$max_avg_pe_release, 1)
  race_shortest <- largest_disparity$race_shortest
  los_shortest <- round(largest_disparity$min_avg_pe_release, 1)
  disparity_diff <- round(largest_disparity$diff_avg_pe_release, 1)

  # Adjust wording for race categories for the sentence construction.
  race_longest_adjusted <- case_when(
    race_longest == "Hispanic, any race" ~ "Hispanic people",
    race_longest == "Other race(s), non-Hispanic" ~ "people of Other race(s)",
    TRUE ~ paste0("people of ", race_longest)
  )
  race_shortest_adjusted <- case_when(
    race_shortest == "Hispanic, any race" ~ "Hispanic people",
    race_shortest == "Other race(s), non-Hispanic" ~ "people of Other race(s)",
    TRUE ~ paste0("people of ", race_shortest)
  )

  # Construct the sentence summarizing the disparity.
  sentence <- paste0(
    "This chart shows the average years in prison past parole eligibility for the most serious sentenced offense by race and ethnicity in 2020. ",
    "The largest disparity observed was for ", offense_type, " offenses, where ", race_longest_adjusted,
    " had ", disparity_diff, " more years in prison past parole eligibility on average compared to ",
    race_shortest_adjusted, " people, who had the shortest time in prison past parole eligibility for these offenses."
  )

  return(sentence)  # Return the constructed sentence for the state.
})

# Set the names of the list elements to match the state names.
all_sentence_avg_pe_release_race_offense <- setNames(all_sentence_avg_pe_release_race_offense, states)
all_sentence_avg_pe_release_race_offense$Georgia


# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex
# ---------------------------------------------------------------------------- #

states <- unique(avg_pe_release_sex$state)

# Function to generate sentences summarizing sex disparities in time spent past parole eligibility.
all_sentence_avg_pe_release_sex <- map(.x = states, .f = function(x) {

  # Data preparation: filter the dataset for the current state and rename sex categories for clarity.
  df1 <- avg_pe_release_sex |>
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
    los_diff_female <- df_female$average_avg_pe_release - df_male$average_avg_pe_release
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

# Set the names of the list elements to match the state names.
all_sentence_avg_pe_release_sex <- setNames(all_sentence_avg_pe_release_sex, states)
all_sentence_avg_pe_release_sex$Georgia



# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Sex and Offense Type
# ---------------------------------------------------------------------------- #

# Iterate over states to generate offense-specific summaries.
states <- unique(avg_pe_release_sex_offense$state)
all_sentence_avg_pe_release_sex_offense <- map(.x = states, .f = function(x) {

  # Filter the dataset for the current state.
  df1 <- avg_pe_release_sex_offense |>
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
      diff_avg_pe_release = avg_male - avg_female # Difference in average years
    ) %>%
    arrange(desc(abs(diff_avg_pe_release)))  # Sort by the absolute magnitude of disparity

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
  disparity_diff <- round(abs(largest_disparity$diff_avg_pe_release), 1)

  # Construct the sentence summarizing the disparity.
  if (largest_disparity$diff_avg_pe_release > 0) {
    # Males spent longer
    sentence <- paste0(
      "This chart shows the average years in prison past parole eligibility for the most serious sentenced offense by sex in 2020. ",
      "For ", offense_type, " offenses, males spent ", disparity_diff,
      " more years in prison past parole eligibility on average compared to females."
    )
  } else {
    # Females spent longer
    sentence <- paste0(
      "This chart shows the average years in prison past parole eligibility for the most serious sentenced offense by sex in 2020. ",
      "For ", offense_type, " offenses, females spent ", disparity_diff,
      " more years in prison past parole eligibility on average compared to males."
    )
  }

  return(sentence)  # Return the constructed sentence for the state.
})

# Set the names of the list elements to match the state names.
all_sentence_avg_pe_release_sex_offense <- setNames(all_sentence_avg_pe_release_sex_offense, states)
all_sentence_avg_pe_release_sex_offense$Georgia



# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #


save(all_sentence_avg_pe_release_race,         file = file.path(app_folder, "all_sentence_avg_pe_release_race.rds"))
save(all_lollipop_avg_pe_release_race,         file = file.path(app_folder, "all_lollipop_avg_pe_release_race.rds"))

save(all_sentence_avg_pe_release_race_offense, file = file.path(app_folder, "all_sentence_avg_pe_release_race_offense.rds"))
save(all_scatter_avg_pe_release_race_offense,  file = file.path(app_folder, "all_scatter_avg_pe_release_race_offense.rds"))

save(avg_pe_release_race,                      file = file.path(app_folder, "avg_pe_release_race.rds"))

save(all_sentence_avg_pe_release_sex,          file = file.path(app_folder, "all_sentence_avg_pe_release_sex.rds"))
save(all_lollipop_avg_pe_release_sex,          file = file.path(app_folder, "all_lollipop_avg_pe_release_sex.rds"))

save(all_sentence_avg_pe_release_sex_offense,  file = file.path(app_folder, "all_sentence_avg_pe_release_sex_offense.rds"))
save(all_scatter_avg_pe_release_sex_offense,   file = file.path(app_folder, "all_scatter_avg_pe_release_sex_offense.rds"))

save(avg_pe_release_sex,                       file = file.path(app_folder, "avg_pe_release_sex.rds"))

