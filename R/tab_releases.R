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

  hc_accessibility_text <- paste0("This graph shows the number of releases in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Releases by Year") |>
    hc_yAxis(title = list(text = "")) |>
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
    hc_colors(c(color2))

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
                                 dataLabels = list(enabled = TRUE,
                                                   style = list(textOutline = "none",
                                                                color = "white"),
                                                   formatter = jsFormatter),
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
    hc_tooltip(pointFormat = '{point.name}: <b>{point.percentage:.0f}%</b> ({point.y})') |>
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
    hc_colors(c(color2, color3))
  return(highcharts)
})
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia





#------ Releases by Race, Ethnicity, Age, and Gender ------#

# Prepare the data for race
current_ped_race <- fnc_prepare_releases_data(ncrp_releases, race)

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_releases_race <- fnc_hc_waffle(current_ped_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)

# Prepare the data for sex
current_ped_sex <- fnc_prepare_releases_data(ncrp_releases, sex)

# Colors for sex
colors_sex <- c(color1, color3)

# Accessibility text for sex
accessibility_text_sex <- "TBD"

# Create the charts for sex
all_waffle_releases_sex <- fnc_hc_waffle(current_ped_sex, "sex", colors_sex, "Gender", accessibility_text_sex)

# Prepare the data for age
current_ped_agerlse <- fnc_prepare_releases_data(ncrp_releases, agerlse) |>
  arrange(state, desc(agerlse))
current_ped_agerlse$agerlse <- factor(current_ped_agerlse$agerlse,
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
all_waffle_releases_agerlse <- fnc_hc_waffle(current_ped_agerlse, "agerlse", colors_age, "Current Age", accessibility_text_age)

# Display the chart for Georgia as an example
all_waffle_releases_race$Georgia
all_waffle_releases_sex$Georgia
all_waffle_releases_agerlse$Georgia







#------ Change in Length of Stay by Offense Type ------#

# Calculate the average length of stay by state and by offense type
ncrp_los_by_offense_type <- ncrp_releases |>
  group_by(state, fbi_index, rptyear) |>
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE)) |>
  pivot_longer(cols = Average, names_to = "type", values_to = "value") |>
  group_by(state) |>
  mutate(max_rptyear = max(rptyear),
         min_rptyear = max_rptyear - 10) |>
  filter(rptyear == min_rptyear | rptyear == max_rptyear) |>
  group_by(state, fbi_index) |>
  mutate(change_value_10_years = last(value) - first(value),
         prop = (last(value) - first(value)) / first(value) * 100,
         change_sentence = ifelse(prop >= 0,
                                  paste0(round(value, 1), "<br><b>", "\u2191", "</b> ", round(prop, 0), "% from 10 years ago"),
                                  paste0(round(value, 1), "<br><b>", "\u2193", "</b> ", round(abs(prop), 0), "% from 10 years ago"))) |>
  ungroup()

# # Create Highcharts visualizations for each state
# all_lollipop_offense_los <- map(.x = states, .f = function(x) {
#
#   # Get the max and min reporting years for the current state
#   state_data <- ncrp_los_by_offense_type |> filter(state == x)
#   max_rptyear <- max(state_data$rptyear)
#   min_rptyear <- max_rptyear - 10
#
#   # Ensure that the necessary years exist for calculations
#   if (!all(c(min_rptyear, max_rptyear) %in% state_data$rptyear)) {
#     return(NULL)  # Skip the state if required years are missing
#   }
#
#   # Create the df1 data frame
#   df1 <- state_data |>
#     ungroup() |>
#     select(fbi_index, rptyear, value) |>
#     mutate(fbi_index_num = as.numeric(as.factor(fbi_index)))
#
#   # Pivot the data wider for calculations
#   df_wide <- df1 |>
#     pivot_wider(names_from = rptyear, values_from = value, names_prefix = "year_")
#
#   # Calculate percentage change and prepare tooltip text
#   df_calculations <- df_wide |>
#     mutate(
#       pct_change = (get(paste0("year_", max_rptyear)) - get(paste0("year_", min_rptyear))) / get(paste0("year_", min_rptyear)) * 100,
#       tooltip_text = paste0(
#         "Offense: ", fbi_index, "<br>",
#         min_rptyear, ": ", round(get(paste0("year_", min_rptyear)), 2), "<br>",
#         max_rptyear, ": ", round(get(paste0("year_", max_rptyear)), 2), "<br>",
#         "Change: ", round(pct_change, 2), "%")
#     ) |>
#     select(fbi_index, tooltip_text)
#
#   # Merge tooltip text back into df1
#   df1 <- df1 |>
#     left_join(df_calculations, by = "fbi_index")
#
#   # Create a named vector for y-axis labels
#   y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))
#
#   highcharts <- # Plotting
#     df1 |>
#     hchart('scatter', hcaes(x = value, y = fbi_index_num, group = rptyear, name = fbi_index)) |>
#     hc_yAxis(
#       title = list(text = ""),
#       categories = y_labels
#     ) |>
#     hc_xAxis(title = list(text = "Length of Stay (Years)")) |>
#     hc_add_theme(hc_theme_with_line) |>
#     hc_title(text = "Change in Length of Stay by Offense Type") |>
#     hc_colors(c(color2, color4)) |>
#     hc_exporting(enabled = TRUE) |>
#     hc_tooltip(pointFormat = "{point.tooltip_text}")
#
#   return(highcharts)
# })

# Get unique states
states <- unique(ncrp_los_by_offense_type$state)

# Create Highcharts visualizations for each state
all_lollipop_offense_los <- map(.x = states, .f = function(x) {

  # Get the max and min reporting years for the current state
  state_data <- ncrp_los_by_offense_type |> filter(state == x)
  max_rptyear <- max(state_data$rptyear)
  min_rptyear <- max_rptyear - 10

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
all_lollipop_offense_los$Georgia







#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_line_releases_by_year, file = file.path(folder, "all_line_releases_by_year.rds"))
  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))
  save(all_pie_release_type,  file = file.path(folder, "all_pie_release_type.rds"))
  save(all_waffle_releases_race,  file = file.path(folder, "all_waffle_releases_race.rds"))
  save(all_waffle_releases_sex,  file = file.path(folder, "all_waffle_releases_sex.rds"))
  save(all_waffle_releases_agerlse,  file = file.path(folder, "all_waffle_releases_agerlse.rds"))
  save(all_lollipop_offense_los,  file = file.path(folder, "all_lollipop_offense_los.rds"))
}
























# # Get unique states
# states <- unique(ncrp_los_by_offense_type$state)
#
# # Create Highcharts visualizations for each state
# all_lollipop_offense_los <- map(.x = states, .f = function(x) {
#
#   # Create the df1 data frame
#   df1 <- ncrp_los_by_offense_type |>
#     ungroup() |>
#     filter(state == x) |>
#     select(fbi_index, rptyear, value) |>
#     mutate(fbi_index_num = as.numeric(as.factor(fbi_index)))
#
#   # Calculate percentage change and prepare tooltip text
#   df_calculations <- df1 |>
#     pivot_wider(names_from = rptyear, values_from = value, names_prefix = "year_") |>
#     mutate(pct_change = (year_2020 - year_2010) / year_2010 * 100,
#            tooltip_text = paste0(
#              "Offense: ", fbi_index, "<br>",
#              "2010: ", round(year_2010, 2), "<br>2020: ", round(year_2020, 2), "<br>Change: ", round(pct_change, 2), "%")) |>
#     select(fbi_index, tooltip_text)
#
#   # Merge tooltip text back into df1
#   df1 <- df1 |>
#     left_join(df_calculations, by = "fbi_index")
#
#   # Create a named vector for y-axis labels
#   y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))
#
#   hc_accessibility_text <- paste0("Text TBD")
#
#   highcharts <- # Plotting
#     df1 |>
#     hchart('scatter', hcaes(x = value, y = fbi_index_num, group = rptyear, name = fbi_index)) |>
#     hc_yAxis(
#       title = list(text = ""),
#       categories = y_labels
#     ) |>
#     hc_xAxis(title = list(text = "Length of Stay (Years)")) |>
#     hc_add_theme(hc_theme_with_line) |>
#     hc_title(text = "Change in Length of Stay by Offense Type") |>
#     hc_colors(c(color2, color4)) |>
#     hc_exporting(enabled = TRUE) |>
#     hc_tooltip(pointFormat = "{point.tooltip_text}")
#
#   return(highcharts)
# })
#
# # Name the list of charts by state
# all_lollipop_offense_los <- setNames(all_lollipop_offense_los, states)
#
# # Display the chart for Georgia as an example
# all_lollipop_offense_los$Georgia




# # Create the chart# Creatas.numeric()e the chart
# hchart(df_combined |> filter(year == "2010"), type = "scatter",
#        hcaes(x = fbi_index, y = value), name = "2010",
#        marker = list(symbol = "circle", fillColor = color2, radius = 5)) |>
#   hc_add_series(df_combined |> filter(year == "2020"), type = "scatter",
#                 hcaes(x = fbi_index, y = value), name = "2020",
#                 marker = list(symbol = "circle", fillColor = color4, radius = 5)) |>
#   hc_xAxis(type = "category") |>
#   hc_yAxis(labels = list(format = "{value} years")) |>
#   hc_tooltip(shared = TRUE, crosshairs = TRUE)

