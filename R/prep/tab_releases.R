#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#######################################


#------ Prison Releases by Year ------#

# Prison releases by year
ncrp_releases_by_year <- ncrp_releases |>
  filter(rptyear >= 2010) |>
  group_by(state, rptyear) |>
  summarise(total = n())

states <- unique(ncrp_releases_by_year$state)

# Generate sentence for each state
all_sentence_releases <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- ncrp_releases_by_year %>% filter(state == x)

  # Find the earliest and latest year prison releases
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)
  earliest_year_release <- df1$total[df1$rptyear == earliest_year]
  latest_year_release <- df1$total[df1$rptyear == latest_year]

  # Calculate the percent change
  percent_change <- (latest_year_release - earliest_year_release) / earliest_year_release * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", prison releases ",
                      change_type, " ", percent_change_abs, "%, dropping from ",
                      format(earliest_year_release, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_release, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Set names for the list elements
all_sentence_releases <- setNames(all_sentence_releases, states)

# Check the sentence for Georgia
all_sentence_releases$Georgia



# Highchart by state since 2010
states <- unique(ncrp_releases_by_year$state)
all_line_releases_by_year <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_releases_by_year |>
    ungroup() |>
    filter(state == x) |>
    distinct()

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$total)*1.1
  min_value <- min(df1$total)/1.5

  hc_accessibility_text <- paste0("This graph shows the number of releases in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Releases by Year") |>
    hc_yAxis(title = list(text = ""),
             min = min_value,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "Releases",
        data = df1$total,
        tooltip = list(
          pointFormat = "Year: {point.category}<br>Prison Releases: {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color1))

  return(highcharts)
})
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
all_line_releases_by_year$Georgia





#------ Proportion of Parole-Eligible Population Released ------#


# We have the year-end population of those who were parole-eligible but were not released,
#   and we have the number of parole-eligible individuals who were released but
#   we don't have the total initial population of parole-eligible individuals for each year,
#   so, determine this below.

# Calculate the number of parole eligible people released by state and year
ncrp_pe_releases_by_year <- ncrp_releases |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_releases = n())

# Calculate the number of parole eligible people in prison by state and year
ncrp_pe_population_not_released_by_year <- ncrp_yearendpop |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_population_not_released = n())

# Merge data together
ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year |>
  left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) |>
  mutate(total_parole_eligible_population =
           total_parole_eligible_releases + total_parole_eligible_population_not_released,
         prop_parole_elgible_released =
           total_parole_eligible_releases/total_parole_eligible_population) |>
  select(state, rptyear,
         total_parole_eligible_population_not_released,
         total_parole_eligible_releases) |>
  pivot_longer(
    cols = c(total_parole_eligible_population_not_released, total_parole_eligible_releases),
    names_to = "status",
    values_to = "n"
  ) |>
  mutate(status = case_when(
    status == "total_parole_eligible_population_not_released" ~ "Not Released",
    status == "total_parole_eligible_releases" ~ "Released"
  ))



# Highchart stacked bar chart
states <- unique(ncrp_pe_proportion_released$state)
all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_proportion_released |>
    filter(state == x)
  jsFormatter <- JS("function() {
                   var total = this.point.stackTotal;
                   var percentage = Math.round((this.y / total) * 100);
                   return percentage + '%';
                  }")
  highcharts <- df1 |>
    hchart(
      type = "column",
      hcaes(x = rptyear, y = n, group = status)
    ) |>
    hc_yAxis(title = list(text = "")) |>
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 # dataLabels = list(enabled = TRUE,
                                 #                   style = list(textOutline = "none",
                                 #                                color = "white"),
                                 #                   formatter = jsFormatter),
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD accessibility text",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD accessibility text"))) |>
    hc_title(text = "Proportion of Parole-Eligible People Released from Prison by Year") |>
    hc_colors(c(color2, color1))
  return(highcharts)
})
all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
all_stackedbar_parole_eligibility_release$Georgia



# In 2020, X% of people eligible for parole were released during their eligibility
# year. This represents a X% decrease/increase compared YEAR.
states <- unique(ncrp_pe_proportion_released$state)

all_sentence_pe_proportion_released <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_proportion_released %>%
    filter(state == x) %>%
    group_by(rptyear, status) %>%
    summarize(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = status, values_from = total, values_fill = list(total = 0)) %>%
    mutate(proportion_released = Released / (Released + `Not Released`) * 100) %>%
    arrange(rptyear)

  # Handling case with only one year of data
  if (nrow(df1) == 1) {
    latest_year <- df1$rptyear[1]
    latest_value <- df1$proportion_released[1]
    sentence <- paste0(
      "In ", latest_year, ", ", round(latest_value, 1),
      "% of people eligible for parole were released during their eligibility year."
    )
    return(sentence)
  }

  # Handling missing data
  if (nrow(df1) < 2) {
    return(paste0("Insufficient data for ", x))
  }

  # Get the latest and earliest years
  latest_year <- tail(df1$rptyear, 1)
  earliest_year <- head(df1$rptyear, 1)

  # Get the values for the latest and earliest years
  latest_value <- tail(df1$proportion_released, 1)
  earliest_value <- head(df1$proportion_released, 1)

  # Calculate the percentage change
  percentage_change <- latest_value - earliest_value

  # Determine if the change is an increase or decrease
  change_type <- ifelse(percentage_change >= 0, "increase", "decrease")

  # Construct the sentence
  sentence <- paste0(
    "In ", latest_year, ", ", round(latest_value, 0),
    "% of people eligible for parole were released during their eligibility year. ",
    "This represents a ", round(abs(percentage_change), 0), "% ", change_type,
    " compared to ", earliest_year, "."
  )

  return(sentence)
})

all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
all_sentence_pe_proportion_released$Arizona



#------ Releases by Release Type ------#

# Filter to people with release type information
# Remove "Other releases" - although Alabama has 30% other releases
ncrp_release_type <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(reltype == "Conditional release" | reltype == "Unconditional release") |>
  mutate(reltype = case_when(reltype == "Conditional release" ~ "Conditional Release",
                             reltype == "Unconditional release" ~ "Unconditional Release",
                             TRUE ~ reltype)) |>
  group_by(state) |>
  count(reltype)

# Highchart pie chart showing releases by release type
states <- unique(ncrp_release_type$state)
all_pie_release_type <- map(.x = states, .f = function(x) {
  df1 <- ncrp_release_type |>
    ungroup() |>
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by release type (unconditional release vs. conditional
                                  release) in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create a pie chart
    highchart() |>
    hc_chart(type = "pie") |>
    hc_title(text = "Proportion of Conditional vs. Unconditional Releases") |>
    hc_plotOptions(pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:1em">{point.name}: </span><br><span style="font-size:2em"><b>{point.percentage:.0f}%</b></span>'
      )
    )) |>
    hc_series(list(
      name = "Release Type",
      colorByPoint = TRUE,
      data = list_parse2(df1 |>
                           mutate(y = n) |>
                           select(name = reltype, y))
    )) |>
    hc_add_theme(base_hc_theme) |>
    hc_colors(c(color2, color3)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(pointFormat = '{point.name}: <b>{point.percentage:.0f}%</b> ({point.y})')
  return(highcharts)
})
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia





#------ Releases by Race, Ethnicity, Age, and Gender ------#

# Prepare the data for race
current_releases_race <- fnc_prepare_releases_data(ncrp_releases, race)

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_releases_race <- fnc_hc_waffle(current_releases_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)

# Prepare the data for sex
current_releases_sex <- fnc_prepare_releases_data(ncrp_releases, sex)

# Colors for sex
colors_sex <- c(color1, color3)

# Accessibility text for sex
accessibility_text_sex <- "TBD"

# Create the charts for sex
all_waffle_releases_sex <- fnc_hc_waffle(current_releases_sex, "sex", colors_sex, "Gender", accessibility_text_sex)

# Prepare the data for age
current_releases_agerlse <- fnc_prepare_releases_data(ncrp_releases, agerlse) |>
  arrange(state, desc(agerlse))
current_releases_agerlse$agerlse <- factor(current_releases_agerlse$agerlse,
                                      levels = c("18-24 years",
                                                 "25-34 years",
                                                 "35-44 years",
                                                 "45-54 years",
                                                 "55+ years"))

# Colors for age
colors_age <- c(color1, color2, color3, color5, color4)

# Accessibility text for age
accessibility_text_age <- "TBD"

# Create the charts for age
all_waffle_releases_agerlse <- fnc_hc_waffle(current_releases_agerlse, "agerlse", colors_age, "Current Age", accessibility_text_age)

# Display the chart for Georgia as an example
all_waffle_releases_race$Georgia
all_waffle_releases_sex$Georgia
all_waffle_releases_agerlse$Georgia


states <- unique(current_releases_race$state)
all_sentence_releases_demographics <- map(.x = states,  .f = function(x) {

  # Race demographics
  df_race <- current_releases_race  |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check for missing race data
  if (nrow(df_race) < 2 || any(is.na(df_race$prop[1:2]))) {
    race_sentence <- "Data on race and ethnicity is incomplete or missing."
  } else {
    # race_sentence <- paste0("notable proportions among ",
    #                         df_race$race[1], " (", round(df_race$prop[1] * 100, 0), "%) and ",
    #                         tolower(df_race$race[2]), " (", round(df_race$prop[2] * 100, 0), "%) people.")
    race_sentence <- paste0("notable proportions among ",
                            df_race$race[1], " and ",
                            tolower(df_race$race[2]), " people.")
  }

  # Gender distribution
  df_sex <- current_releases_sex  |>
    filter(state == x)

  # Check for missing sex data
  if (nrow(df_sex) < 2 || any(is.na(df_sex$prop))) {
    sex_sentence <- "Gender distribution data is incomplete or missing."
  } else {
    if (df_sex$prop[df_sex$sex == "Male"] > df_sex$prop[df_sex$sex == "Female"]) {
      sex_sentence <- "Gender distribution indicates a predominance of males over females."
    } else if (df_sex$prop[df_sex$sex == "Female"] > df_sex$prop[df_sex$sex == "Male"]) {
      sex_sentence <- "Gender distribution indicates a predominance of females over males."
    } else {
      sex_sentence <- "Gender distribution indicates an equal number of males and females."
    }
  }

  # Age distribution
  df_agerlse <- current_releases_agerlse  |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check for missing agerlse data
  if (nrow(df_agerlse) < 2 || any(is.na(df_agerlse$prop[1:2]))) {
    age_sentence <- "Age distribution data is incomplete or missing."
  } else {
    # age_sentence <- paste0("Age-wise, the majority of people were ",
    #                        df_agerlse$agerlse[1], " (", round(df_agerlse$prop[1] * 100, 0), "%) and ",
    #                        df_agerlse$agerlse[2], " (", round(df_agerlse$prop[2] * 100, 0), "%) old.")
    age_sentence <- paste0("Age-wise, the majority of people were ",
                           df_agerlse$agerlse[1], " and ",
                           df_agerlse$agerlse[2],
                           " old. These findings provide insights into the populations transitioning back into the community.")
  }

  # Combine the sentences
  sentences <- paste0("The demographics of people released from prison reveal ",
                      race_sentence, " ", sex_sentence, " ", age_sentence)

  return(sentences)
})

all_sentence_releases_demographics <- setNames(all_sentence_releases_demographics, states)
all_sentence_releases_demographics$Georgia




#------ Change in Length of Stay by Offense Type ------#

# Calculate the average length of stay by state and by offense type
ncrp_los_by_offense_type <- ncrp_releases |>
  filter(rptyear >= 2010) |>
  group_by(state, fbi_index, rptyear) |>
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE)) |>
  pivot_longer(cols = Average, names_to = "type", values_to = "value") |>
  group_by(state) |>
  mutate(max_rptyear = max(rptyear),
         min_rptyear = min(rptyear),
         years_ago = max_rptyear - min_rptyear) |>
  filter(rptyear == min_rptyear | rptyear == max_rptyear) |>
  group_by(state, fbi_index) |>
  mutate(change_value_over_years = last(value) - first(value),
         prop = (last(value) - first(value)) / first(value) * 100,
         change_sentence = ifelse(prop >= 0,
                                  paste0(round(value, 1), "<br><b>", "\u2191", "</b> ", round(prop, 0), "% from ", years_ago, " years ago"),
                                  paste0(round(value, 1), "<br><b>", "\u2193", "</b> ", round(abs(prop), 0), "% from ", years_ago, " years ago"))) |>
  ungroup()

# Get unique states
states <- unique(ncrp_los_by_offense_type$state)

# Create Highcharts visualizations for each state
all_lollipop_offense_los <- map(.x = states, .f = function(x) {

  # Get the max and min reporting years for the current state
  state_data <- ncrp_los_by_offense_type |> filter(state == x)
  max_rptyear <- max(state_data$rptyear)
  min_rptyear <- min(state_data$rptyear)

  # Ensure that the necessary years exist for calculations
  if (!all(c(min_rptyear, max_rptyear) %in% state_data$rptyear)) {
    return(NULL)  # Skip the state if required years are missing
  }

  # Create the df1 data frame
  df1 <- state_data |>
    ungroup() |>
    select(fbi_index, rptyear, value) |>
    mutate(fbi_index_num = as.numeric(as.factor(fbi_index)))

  # Pivot the data wider for calculations
  df_wide <- df1 |>
    pivot_wider(names_from = rptyear, values_from = value, names_prefix = "year_")

  # Calculate percentage change and prepare tooltip text
  df_calculations <- df_wide |>
    mutate(
      pct_change = (get(paste0("year_", max_rptyear)) - get(paste0("year_", min_rptyear))) / get(paste0("year_", min_rptyear)) * 100,
      tooltip_text = paste0(
        "Offense: ", fbi_index, "<br>",
        min_rptyear, ": ", round(get(paste0("year_", min_rptyear)), 2), "<br>",
        max_rptyear, ": ", round(get(paste0("year_", max_rptyear)), 2), "<br>",
        "Change: ", round(pct_change, 2), "%")
    ) |>
    select(fbi_index, tooltip_text)

  # Merge tooltip text back into df1
  df1 <- df1 |>
    left_join(df_calculations, by = "fbi_index")

  # Create a named vector for y-axis labels
  y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))

  # Create a data frame for line series
  df_lines <- df_wide |>
    pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "value") |>
    mutate(year = as.numeric(gsub("year_", "", year))) |>
    arrange(fbi_index, year)

  highcharts <- # Plotting
    highchart() |>
    hc_add_series(
      df1,
      type = 'scatter',
      hcaes(x = value, y = fbi_index_num, group = rptyear, name = fbi_index),
      tooltip = list(pointFormat = "{point.tooltip_text}")
    ) |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = fbi_index_num, group = fbi_index),
      lineWidth = 1,
      color = darkgray,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    ) |>
    hc_yAxis(
      title = list(text = ""),
      categories = y_labels
    ) |>
    hc_xAxis(title = list(text = "Length of Stay (Years)")) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_title(text = "Change in Length of Stay by Offense Type") |>
    hc_colors(c(color2, color4)) |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_offense_los <- setNames(all_lollipop_offense_los, states)

# Display the chart for Georgia as an example
all_lollipop_offense_los$Kansas


states <- unique(ncrp_los_by_offense_type$state)

# Generate sentence for each state
all_sentence_los_offense <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df_state <- ncrp_los_by_offense_type %>% filter(state == x)

  # Handling missing data
  if (nrow(df_state) == 0) {
    return(paste0("No data available for ", x))
  }

  # Handling case with only one year of data
  if (length(unique(df_state$rptyear)) == 1) {
    single_year <- unique(df_state$rptyear)
    sentence <- paste0(
      "In ", single_year, ", the average time served for different offense types was observed in ", x, "."
    )
    return(sentence)
  }

  # Calculate the largest change in percentage
  df_change <- df_state %>%
    arrange(desc(abs(prop))) %>%
    slice(1)

  largest_change_offense <- df_change$fbi_index
  largest_change_value <- df_change$prop
  change_type <- ifelse(largest_change_value >= 0, "increased", "decreased")

  # Get the earliest and latest year
  earliest_year <- min(df_state$rptyear)
  latest_year <- max(df_state$rptyear)

  # Construct the sentence
  sentence <- paste0(
    "Between ", earliest_year, " and ", latest_year,
    ", there were shifts in the average time served by individuals for different offense types in ", x, ". ",
    "The largest change was for ", tolower(as.character(largest_change_offense)), " offenses, which ", change_type, " by ",
    round(abs(largest_change_value), 0), "%."
  )

  return(sentence)
})

# Set names for the list elements
all_sentence_los_offense <- setNames(all_sentence_los_offense, states)

# Check the sentence for Georgia
all_sentence_los_offense$Georgia











#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_sentence_releases, file = file.path(folder, "all_sentence_releases.rds"))
  save(all_line_releases_by_year, file = file.path(folder, "all_line_releases_by_year.rds"))

  save(all_sentence_pe_proportion_released, file = file.path(folder, "all_sentence_pe_proportion_released.rds"))
  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))
  save(all_pie_release_type,  file = file.path(folder, "all_pie_release_type.rds"))

  save(all_sentence_releases_demographics, file = file.path(folder, "all_sentence_releases_demographics.rds"))
  save(all_waffle_releases_race,  file = file.path(folder, "all_waffle_releases_race.rds"))
  save(all_waffle_releases_sex,  file = file.path(folder, "all_waffle_releases_sex.rds"))
  save(all_waffle_releases_agerlse,  file = file.path(folder, "all_waffle_releases_agerlse.rds"))

  save(all_sentence_los_offense,  file = file.path(folder, "all_sentence_los_offense.rds"))
  save(all_lollipop_offense_los,  file = file.path(folder, "all_lollipop_offense_los.rds"))
}

