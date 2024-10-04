#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    This script is responsible for generating the visualizations and
#    findings for the 'Releases' tab in the AV Parole web tool.
#    It covers:
#    - Trends in prison releases by year and state
#    - Proportion of parole-eligible individuals released by year
#    - Breakdown by release type, race, sex, age, and offense type
#######################################


# ---------------------------------------------------------------------------- #
# Prison Release Trends
# ---------------------------------------------------------------------------- #

# Filter the NCRP releases data to include only states with parole systems
ncrp_releases_filtered <- fnc_filter_population(ncrp_releases)

# Summarize total prison releases by state and year for data from 2010 onwards
ncrp_releases_by_year <- ncrp_releases_filtered |>
  filter(rptyear >= 2010) |>
  group_by(state, rptyear) |>
  summarise(total = n(), .groups = "drop")

# Create a list of unique states for which we have release data
states <- unique(ncrp_releases_by_year$state)

# Generate sentences summarizing the change in prison releases over time for each state
all_sentence_releases <- map(.x = states, .f = function(x) {
  # Filter release data for the specific state
  df1 <- ncrp_releases_by_year %>% filter(state == x)

  # Determine the earliest and latest years of available data
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)

  # Get release totals for the earliest and latest years
  earliest_year_release <- df1$total[df1$rptyear == earliest_year]
  latest_year_release <- df1$total[df1$rptyear == latest_year]

  # Calculate the percentage change between the two years
  percent_change <- (latest_year_release - earliest_year_release) / earliest_year_release * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  # Construct a sentence describing the trend in releases
  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", prison releases ",
                      change_type, " ", percent_change_abs, " percent, dropping from ",
                      format(earliest_year_release, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_release, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Assign state names as the names of the sentences list
all_sentence_releases <- setNames(all_sentence_releases, states)
all_sentence_releases$Georgia
rm(states)

# Generate line charts for each state showing the trend in prison releases since 2010
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
        # tooltip = list(
        #   pointFormat = "Year: {point.category}<br>Prison Releases: {point.y}"
        # )
        tooltip = list(
          pointFormat = "<b>Prison Releases:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5))

  return(highcharts)
})

# Assign state names as the names of the charts list
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
all_line_releases_by_year$Georgia

# We have the year-end population of those who were parole-eligible but were not released,
#   and we have the number of parole-eligible individuals who were released but
#   we don't have the total initial population of parole-eligible individuals for each year,
#   so, determine this below.

# Calculate the number of parole eligible people released by state and year
ncrp_pe_releases_by_year <- ncrp_releases_filtered |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_releases = n(), .groups = "drop")

# Calculate the number of parole eligible people in prison by state and year
ncrp_pe_population_not_released_by_year <- fnc_filter_population(ncrp_yearendpop) |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_population_not_released = n(), .groups = "drop")

# Calculate the number of parole-eligible people released and not released by state and year
ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year |>
  left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) |>
  mutate(total_parole_eligible_population =
           total_parole_eligible_releases + total_parole_eligible_population_not_released,
         prop_parole_eligible_released =
           total_parole_eligible_releases / total_parole_eligible_population,
         prop_parole_eligible_not_released =
           total_parole_eligible_population_not_released / total_parole_eligible_population) |>
  select(state, rptyear,
         prop_parole_eligible_not_released,
         prop_parole_eligible_released) |>
  pivot_longer(
    cols = c(prop_parole_eligible_not_released, prop_parole_eligible_released),
    names_to = "status",
    values_to = "proportion"
  ) |>
  mutate(status = case_when(
    status == "prop_parole_eligible_not_released" ~ "Not Released",
    status == "prop_parole_eligible_released" ~ "Released")
  )


# Generate stacked bar charts for each state showing the proportion of parole-eligible people released and not released
states <- unique(ncrp_pe_proportion_released$state)
all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_proportion_released |>
    filter(state == x)
  highcharts <- df1 |>
    hchart(
      type = "column",
      hcaes(x = rptyear, y = proportion, group = status)
    ) |>
    hc_yAxis(title = list(text = ""),
             max = 1,
             labels = list(formatter = JS("function() { return (this.value * 100) + '%'; }"))) |>
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD accessibility text",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD accessibility text"))) |>
    hc_title(text = "Proportion of Parole-Eligible People Released from Prison by Year") |>
    hc_tooltip(formatter = JS("
      function() {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' +
          (this.y * 100).toFixed(0) + '%</b><br/>';
      }
    ")) |>
    hc_colors(c(color3, color5))
  return(highcharts)
})
all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
all_stackedbar_parole_eligibility_release$Georgia
rm(states)

# SENTENCE: In 2020, X% of people eligible for parole were released during their eligibility
#           year. This represents a X% decrease/increase compared YEAR.

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
      " percent of people eligible for parole were released during their eligibility year."
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
    " percent of people eligible for parole were released during their eligibility year. ",
    "This represents a ", round(abs(percentage_change), 0), " percent ", change_type,
    " compared to ", earliest_year, "."
  )

  return(sentence)
})

all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
all_sentence_pe_proportion_released$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# RELEASE TYPE
# ---------------------------------------------------------------------------- #

# Filter to include only conditional and unconditional releases, removing other release types
# Remove "Other releases" - although Alabama has 30% other releases
release_types <- ncrp_releases_filtered |>
  filter(rptyear == select_year) |>
  filter(reltype == "Conditional release" | reltype == "Unconditional release") |>
  mutate(reltype = case_when(reltype == "Conditional release" ~ "Conditional Release",
                             reltype == "Unconditional release" ~ "Unconditional Release",
                             TRUE ~ reltype)) |>
  group_by(state) |>
  count(reltype) |>
  mutate(prop = n/sum(n))

# Generate pie charts for each state showing the proportion of conditional vs. unconditional releases
states <- unique(release_types$state)
all_pie_release_type <- map(.x = states, .f = function(x) {
  df1 <- release_types |>
    ungroup() |>
    filter(state == x)

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by release type (unconditional release vs. conditional
                                  release) in ",
                                  select_year, " in the state of ", x, ".")

  # Check if 100% of the releases are "Conditional Release"
  is_100_conditional <- all(df1$reltype == "Conditional Release")

  highcharts <- # Create a pie chart
    highchart() |>
    hc_chart(type = "pie") |>
    hc_title(text = "Proportion of Conditional vs. Unconditional Releases") |>
    hc_plotOptions(pie = list(
      startAngle = if (is_100_conditional) 90 else 0,  # Rotate by 90 degrees if 100% conditional
      endAngle = if (is_100_conditional) 450 else 360, # Keep chart full if rotated
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
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
    hc_tooltip(pointFormat = 'Number of Releases: {point.y}<br>Percentage of Releases: {point.percentage:.0f}%')

  return(highcharts)
})

# Assign state names as the names of the charts list
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia
all_pie_release_type$`South Dakota`
rm(states)



# Generate sentence for each state
states <- unique(release_types$state)
all_sentence_release_type <- map(.x = states,  .f = function(x) {
  df1 <- release_types |>
    filter(state == x & reltype == "Conditional Release")
  sentences <- paste0(
    "Conditional release involves an individual’s release under specific conditions and supervision, whereas unconditional release means the individual is released without further obligations or restrictions. ",
    "In ", select_year, ", ", round(df1$prop*100, 0), " percent of prison releases were ", tolower(df1$reltype), "s.")
  return(sentences)
})
# Assign state names as the names of the charts list
all_sentence_release_type <- setNames(all_sentence_release_type, states)
all_sentence_release_type$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# DEMOGRAPHICS
# ---------------------------------------------------------------------------- #

# Generate bar charts and sentences describing prison releases by demographic categories (race, sex, age)

# Race and Ethnicity
# Filter releases for valid race data and generate visualizations and summary sentences
prison_releases_race <- ncrp_releases_filtered |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(race)& race != "Unknown") |>
  count(race) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Sex
# Filter releases for valid sex data and generate visualizations and summary sentences
prison_releases_sex <- ncrp_releases_filtered |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(sex)& sex != "Unknown") |>
  count(sex) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Age
# Filter releases for valid age data and generate visualizations and summary sentences
prison_releases_agerlse <- ncrp_releases_filtered |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(agerlse) & agerlse != "Unknown") |>
  count(agerlse) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate graph for each state
states <- unique(prison_releases_race$state)
all_bar_releases_race <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_race |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Race and Ethnicity:</b> ", race, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by race and ethnicity in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "race", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5))
  return(highcharts)
})

# Assign state names as the names of the charts list
all_bar_releases_race <- setNames(all_bar_releases_race, states)
all_bar_releases_race$Georgia
rm(states)

# Generate sentence for each state
states <- unique(prison_releases_race$state)
all_sentence_releases_race <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_race |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of prison releases were ", df1$race, " people.")
  return(sentences)
})

# Assign state names as the names of the charts list
all_sentence_releases_race <- setNames(all_sentence_releases_race, states)
all_sentence_releases_race$Georgia
rm(states)

# Generate graph for each state
states <- unique(prison_releases_sex$state)
all_bar_releases_sex <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_sex |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Sex:</b> ", sex, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by sex in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sex", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Sex") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE)|>
    hc_colors(c(color5))
  return(highcharts)
})

# Assign state names as the names of the charts list
all_bar_releases_sex <- setNames(all_bar_releases_sex, states)
all_bar_releases_sex$Georgia
rm(states)


# Generate sentence for each state
states <- unique(prison_releases_sex$state)
all_sentence_releases_sex <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_sex |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of prison releases were ", tolower(df1$sex), "s.")
  return(sentences)
})

# Assign state names as the names of the charts list
all_sentence_releases_sex <- setNames(all_sentence_releases_sex, states)
all_sentence_releases_sex$Georgia
rm(states)

# Generate graph for each state
states <- unique(prison_releases_agerlse$state)
all_bar_releases_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_agerlse |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Age:</b> ", agerlse, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(agerlse))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "agerlse", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Age") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE)|>
    hc_colors(c(color5))
  return(highcharts)
})

# Assign state names as the names of the charts list
all_bar_releases_agerlse <- setNames(all_bar_releases_agerlse, states)
all_bar_releases_agerlse$Georgia
rm(states)


# Generate sentence for each state
states <- unique(prison_releases_agerlse$state)
all_sentence_releases_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_agerlse |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$agerlse <- gsub("-", " to ", df1$agerlse)
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of prison releases were between the ages of ", df1$agerlse, " old.")
  return(sentences)
})

# Assign state names as the names of the charts list
all_sentence_releases_agerlse <- setNames(all_sentence_releases_agerlse, states)
all_sentence_releases_agerlse$Georgia
rm(states)







# ---------------------------------------------------------------------------- #
# OFFENSE TYPE
# ---------------------------------------------------------------------------- #

# Filter release data for the selected year and group by state and offense type (FBI index)
# Remove cases where offense type is missing or unknown
releases_fbi_index <- ncrp_releases_filtered |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(fbi_index) & fbi_index != "Unknown") |>
  count(fbi_index) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate a bar chart for each state to visualize the proportion of releases by offense type
states <- unique(releases_fbi_index$state)
all_bar_releases_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- releases_fbi_index |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Offense Type:</b> ", fbi_index, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by offense type in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = paste0("Prison Releases by Offense Type, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5))
  return(highcharts)
})

# Assign state names as the names of the charts list
all_bar_releases_fbi_index <- setNames(all_bar_releases_fbi_index, states)
all_bar_releases_fbi_index$Georgia
rm(states)

# Generate sentence for each state
states <- unique(releases_fbi_index$state)
all_sentence_releases_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- releases_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of prison releases were for people incarcerated for ", tolower(df1$fbi_index), " offenses.")
  return(sentences)
})

# Assign state names as the names of the charts list
all_sentence_releases_fbi_index <- setNames(all_sentence_releases_fbi_index, states)
all_sentence_releases_fbi_index$Georgia
rm(states)





# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

save(all_sentence_releases,                     file = file.path(app_folder, "all_sentence_releases.rds"))
save(all_line_releases_by_year,                 file = file.path(app_folder, "all_line_releases_by_year.rds"))
save(all_sentence_pe_proportion_released,       file = file.path(app_folder, "all_sentence_pe_proportion_released.rds"))
save(all_stackedbar_parole_eligibility_release, file = file.path(app_folder, "all_stackedbar_parole_eligibility_release.rds"))

save(all_sentence_release_type,                 file = file.path(app_folder, "all_sentence_release_type.rds"))
save(all_pie_release_type,                      file = file.path(app_folder, "all_pie_release_type.rds"))

save(all_sentence_releases_race,                file = file.path(app_folder, "all_sentence_releases_race.rds"))
save(all_bar_releases_race,                     file = file.path(app_folder, "all_bar_releases_race.rds"))

save(all_sentence_releases_sex,                 file = file.path(app_folder, "all_sentence_releases_sex.rds"))
save(all_bar_releases_sex,                      file = file.path(app_folder, "all_bar_releases_sex.rds"))

save(all_sentence_releases_agerlse,             file = file.path(app_folder, "all_sentence_releases_agerlse.rds"))
save(all_bar_releases_agerlse,                  file = file.path(app_folder, "all_bar_releases_agerlse.rds"))

save(all_sentence_releases_fbi_index,           file = file.path(app_folder, "all_sentence_releases_fbi_index.rds"))
save(all_bar_releases_fbi_index,                file = file.path(app_folder, "all_bar_releases_fbi_index.rds"))


