#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#######################################


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
  save(all_lollipop_offense_los, file = file.path(folder, "all_lollipop_offense_los.rds"))
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
# hchart(df_combined %>% filter(year == "2010"), type = "scatter",
#        hcaes(x = fbi_index, y = value), name = "2010",
#        marker = list(symbol = "circle", fillColor = color2, radius = 5)) %>%
#   hc_add_series(df_combined %>% filter(year == "2020"), type = "scatter",
#                 hcaes(x = fbi_index, y = value), name = "2020",
#                 marker = list(symbol = "circle", fillColor = color4, radius = 5)) %>%
#   hc_xAxis(type = "category") %>%
#   hc_yAxis(labels = list(format = "{value} years")) %>%
#   hc_tooltip(shared = TRUE, crosshairs = TRUE)

