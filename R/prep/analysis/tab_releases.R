################################################################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#    Uses BJS Prisoners Data
################################################################################

# ---------------------------------------------------------------------------- #
# Prison Release Trends
# ---------------------------------------------------------------------------- #

# Function that filters the releases data to include only includes states with
# parole systems and without high missingness
ncrp_releases_filtered <- fnc_filter_states(ncrp_releases_not_consolidated, exclude = states_to_exclude)################ change to ncrp_releases_consolidated when complete

# Summarize total people released from prison by state and year for data from 2010 onwards
ncrp_releases_by_year <- ncrp_releases_filtered |>
  filter(rptyear >= 2010) |>
  group_by(state, rptyear) |>
  summarise(total = n(), .groups = "drop") |>
  left_join(which_overall_year, by = "state")

# Get unique states to iterate over
states <- unique(ncrp_releases_by_year$state)

# Generate sentence for each state
all_sentence_releases_by_year <- map(.x = states, .f = function(x) {
  # Filter release data for the specific state
  df1 <- ncrp_releases_by_year %>% filter(state == x)

  # Determine the earliest year and use year_to_use for the latest year
  earliest_year <- min(df1$rptyear)
  latest_year <- df1$year_to_use[1]  # Use the year_to_use from which_overall_year

  # Get release totals for the earliest and latest years
  earliest_year_release <- df1$total[df1$rptyear == earliest_year]
  latest_year_release <- df1$total[df1$rptyear == latest_year]

  # Calculate the percentage change between the two years
  percent_change <- (latest_year_release - earliest_year_release) / earliest_year_release * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  # Construct a sentence describing the trend in releases
  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the number of people released from prison ",
                      change_type, " ", percent_change_abs, " percent, dropping from ",
                      format(earliest_year_release, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_release, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Assign state names to list
all_sentence_releases_by_year <- setNames(all_sentence_releases_by_year, states)
all_sentence_releases_by_year$Georgia
all_sentence_releases_by_year$Connecticut
all_sentence_releases_by_year$Hawaii

# VISUALIZATION: Prison Releases by Year
# Generate chart for each state
all_line_releases_by_year <- map(.x = states,  .f = function(x) {

  max_year <- unique(df1$year_to_use)

  df1 <- ncrp_releases_by_year |>
    ungroup() |>
    filter(state == x & rptyear <= max_year) |>
    distinct()

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$total)*1.1

  hc_accessibility_text <- paste0("This graph shows the number of releases in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = paste0("People Released From Prison by Year, ", min(df1$rptyear), "-", max_year)) |>
    hc_yAxis(title = list(text = ""),
             min = 0,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "Releases",
        data = df1$total,
        tooltip = list(
          pointFormat = "<b>People Released From Prison:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5)) |>
    hc_caption(text = ncrp_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})
# Assign state names to list
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
all_line_releases_by_year$Georgia
all_line_releases_by_year$Connecticut
rm(states)


# ---------------------------------------------------------------------------- #
# Proportion of Parole-Eligible People Released on Their Parole Eligibility Year

# We have the year-end population of those who were parole-eligible but were not released,
#   and we have the number of parole-eligible individuals who were released but
#   we don't have the total initial population of parole-eligible individuals for each year,
#   so, determine this below.
# ---------------------------------------------------------------------------- #

# Filter NCRP releases to people in prison for new crimes and sentence lengths
# not less than one year or life
ncrp_releases_filtered_pop <- fnc_filter_pe_population_criteria(data = ncrp_releases_not_consolidated,######################################### change to ncrp_releases_consolidated when ready
                                                                exclude = states_to_exclude,
                                                                dont_filter = states_nofilter) |>
  left_join(which_overall_year, by = "state")

# Get number of people past PE and released and get people released on PE
ncrp_pe_releases_by_year <- ncrp_releases_filtered_pop |>
  filter(estimated_pey_status %in% c("past", "current_year")) |>
  group_by(state, rptyear, year_to_use, estimated_pey_status) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(state, rptyear, year_to_use) |>
  mutate(prop = n/sum(n),
         estimated_pey_status = case_when(
           estimated_pey_status == "past" ~ "Past Parole Eligibility",
           estimated_pey_status == "current_year" ~ "On Parole Eligibility"
         )) |>
  filter(rptyear >= 2010 & rptyear <= year_to_use)

# Get unique states to iterate over
states <- unique(ncrp_pe_releases_by_year$state)

# VISUALIZATION: Percentage of Parole-Eligible People Released Past Their Parole Eligibility Year
# Generate chart for each state
all_stackedbar_pe_release <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_releases_by_year |>
    filter(state == x)

  title <- "People Released on Parole Eligibility Year vs. Past Parole Eligibility Year"
  hc_accessibility_text <- ""

  highcharts <- df1 |>
    hchart(
      type = "column",
      hcaes(x = rptyear, y = prop, group = estimated_pey_status)
    ) |>
    hc_yAxis(title = list(text = ""),
             max = 1,
             labels = list(formatter = JS("function() { return (this.value * 100) + '%'; }"))) |>
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") |>
    hc_add_theme(base_hc_theme) |>
    hc_colors(c(color3, color5)) |>
    hc_legend(enabled = TRUE) |>
    hc_tooltip(formatter = JS("
      function() {
        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' +
          (this.y * 100).toFixed(0) + '%</b><br/>';
      }
    ")) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_",
                                   min(df1$rptyear), "_", max(df1$rptyear))) |>
    hc_title(text = paste0(title, ", ", min(df1$rptyear), "-", max(df1$rptyear))) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4)) |>
    fnc_add_hc_accessibility(hc_accessibility_text) |>
    hc_caption(text = ncrp_csg_source)

  return(highcharts)
})
# Assign state names to list
all_stackedbar_pe_release <- setNames(all_stackedbar_pe_release, states)
all_stackedbar_pe_release$Georgia

# Generate sentence for each state
all_sentence_pe_proportion_released <- map(.x = states, .f = function(x) {
  df1 <- ncrp_pe_releases_by_year %>%
    filter(state == x)

  # Find the earliest and latest years
  earliest_year <- min(df1$rptyear)
  latest_year <- unique(df1$year_to_use)

  # Filter data for the earliest and latest years
  df_earliest <- df1 %>% filter(rptyear == earliest_year)
  df_latest <- df1 %>% filter(rptyear == latest_year)

  # Check if both years have data
  if (nrow(df_earliest) == 0 || nrow(df_latest) == 0) {
    # Return a message if data is missing for the earliest or latest year
    sentence <- paste0("Data for ", earliest_year, " or ", latest_year, " is missing for ", x, ".")
  } else {
    # Get the proportion for 'Past Parole Eligibility' for both years
    prop_past_parole_earliest <- df_earliest %>%
      filter(estimated_pey_status == "Past Parole Eligibility") %>%
      pull(prop)

    prop_past_parole_latest <- df_latest %>%
      filter(estimated_pey_status == "Past Parole Eligibility") %>%
      pull(prop)

    # Check if the proportions exist
    if (length(prop_past_parole_earliest) == 0 || length(prop_past_parole_latest) == 0) {
      sentence <- paste0("Proportion data for Past Parole Eligibility is missing for ", earliest_year, " or ", latest_year, " in ", x, ".")
    } else {
      # Calculate percentage change between the earliest and latest years
      prop_change <- (prop_past_parole_latest - prop_past_parole_earliest) / prop_past_parole_earliest * 100

      # Generate sentence with appropriate increase/decrease
      if (prop_change > 0) {
        sentence <- paste0(
          "In ", latest_year, ", ", round(prop_past_parole_latest * 100, 0),
          " percent of parole-eligible people released in ", x, " were released past their parole eligibility year, ",
          "which is an increase of ", round(prop_change, 0), " percent from ", earliest_year, "."
        )
      } else {
        sentence <- paste0(
          "In ", latest_year, ", ", round(prop_past_parole_latest * 100, 0),
          " percent of parole-eligible people released in ", x, " were released past their parole eligibility year, ",
          "which is a decrease of ", round(abs(prop_change), 0), " percent from ", earliest_year, "."
        )
      }
    }
  }

  return(sentence)
})
all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
all_sentence_pe_proportion_released$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Releases by Release Type
# ---------------------------------------------------------------------------- #

# Filter to include only conditional and unconditional releases, removing other release types
# Remove "Other releases" - although Alabama has 30% other releases
release_types <- ncrp_releases_filtered_pop |>
  filter(reltype == "Conditional release" | reltype == "Unconditional release") |>
  mutate(reltype = case_when(reltype == "Conditional release" ~ "Conditional Release",
                             reltype == "Unconditional release" ~ "Unconditional Release",
                             TRUE ~ reltype)) |>
  filter(rptyear == year_to_use) |>
  group_by(state, rptyear) |>
  count(reltype) |>
  mutate(prop = n/sum(n))

# Get unique states to iterate over
states <- unique(release_types$state)

# VISUALIZATION: Proportion of Conditional vs Unconditional Releases
# Generate chart for each state
all_pie_release_type <- map(.x = states, .f = function(x) {

  df1 <- release_types |>
    ungroup() |>
    filter(state == x)
  year <- unique(df1$rptyear)

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by release type (unconditional release vs. conditional
                                  release) in ",
                                  year, " in the state of ", x, ".")

  # Check if 100% of the releases are "Conditional Release"
  is_100_conditional <- all(df1$reltype == "Conditional Release")

  highcharts <- # Create a pie chart
    highchart() |>
    hc_chart(type = "pie") |>
    hc_title(text = paste0("Percentage of Conditional vs. Unconditional Releases, ", year)) |>
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
    hc_colors(c(color4, color2)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(pointFormat = 'Number of People Released: {point.y}<br>Percentage of People Released: {point.percentage:.0f}%') |>
    hc_caption(text = ncrp_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})
# Assign state names to list
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia
all_pie_release_type$`South Dakota`

# Get unique states to iterate over
states <- unique(release_types$state)

# SENTENCE: "Conditional release involves an individual’s release under specific
#            conditions and supervision, whereas unconditional release means
#            the individual is released without further obligations or restrictions.
#            In YEAR, 45 percent of people released from prison were conditional releases."
# Generate sentence for each state
all_sentence_release_type <- map(.x = states,  .f = function(x) {
  select_year <- fnc_determine_select_year(x, which_overall_year)
  df1 <- release_types |>
    filter(state == x & reltype == "Conditional Release")
  sentences <- paste0(
    "Conditional release involves an individual’s release under specific conditions and supervision, whereas unconditional release means the individual is released without further obligations or restrictions. ",
    "In ", select_year, ", ", round(df1$prop*100, 0), " percent of people released from prison were ", tolower(df1$reltype), "s.")
  return(sentences)
})
# Assign state names as the names of the charts list
all_sentence_release_type <- setNames(all_sentence_release_type, states)
all_sentence_release_type$Georgia
rm(states)




# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Filter data to year depending on state
current_releases <- ncrp_releases_not_consolidated |>  # ncrp_releases_consolidated |>#####################################################might need to temp change this until data is ready
  fnc_filter_by_year(which_overall_year)

# Filter data to year depending on state
current_releases_not_consolidated <- ncrp_releases_not_consolidated |>
  fnc_filter_by_year(which_overall_year)

# Summarize number of people in prison by race, sex, ageyrend, offense, and sentence length
ncrp_releases_race       <- fnc_summarize_data(current_releases, "race") |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
ncrp_releases_sex        <- fnc_summarize_data(current_releases, "sex")
ncrp_releases_agerlse    <- fnc_summarize_data(current_releases, "agerlse") ################# might need to change to agerelease
ncrp_releases_fbi_index  <- fnc_summarize_data(current_releases, "fbi_index") |> fnc_group_offense_type()
ncrp_releases_sentlgth   <- fnc_summarize_data(current_releases, "sentlgth")

# List of parameters for each category
categories <- list(
  list(data = ncrp_releases_race, x_var = "race", metric = "Race and Ethnicity"),
  list(data = ncrp_releases_sex, x_var = "sex", metric = "Sex"),
  list(data = ncrp_releases_agerlse, x_var = "agerlse", metric = "Age"),
  list(data = ncrp_releases_sentlgth, x_var = "sentlgth", metric = "Sentence Length"),
  list(data = ncrp_releases_fbi_index, x_var = "fbi_index", metric = "Offense Type")
)

# ---------------------------------------------------------------------------- #
# Generate Sentences and Column Charts (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Initialize empty lists to store bar charts and sentences
all_bar_releases <- list()
all_sentence_releases <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  all_bar_releases[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,
    x_var      = category$x_var,
    metric     = category$metric,
    type_desc  = "released from prison",
    title_type = "People Released from Prison",
    y_var      = "prop"
  )

  all_sentence_releases[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type_desc = "released from prison"
  )
}

# Access specific bar charts and sentences
all_bar_releases_race <- all_bar_releases[["race"]]
all_sentence_releases_race <- all_sentence_releases[["race"]]
all_bar_releases_sex <- all_bar_releases[["sex"]]
all_sentence_releases_sex <- all_sentence_releases[["sex"]]
all_bar_releases_agerlse <- all_bar_releases[["agerlse"]]
all_sentence_releases_agerlse <- all_sentence_releases[["agerlse"]]
all_bar_releases_sentlgth <- all_bar_releases[["sentlgth"]]
all_sentence_releases_sentlgth <- all_sentence_releases[["sentlgth"]]
all_bar_releases_fbi_index <- all_bar_releases[["fbi_index"]]
all_sentence_releases_fbi_index <- all_sentence_releases[["fbi_index"]]


# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_releases_by_year             = "all_sentence_releases_by_year.rds",
  all_line_releases_by_year                 = "all_line_releases_by_year.rds",
  all_sentence_pe_proportion_released       = "all_sentence_pe_proportion_released.rds",
  all_stackedbar_pe_release                 = "all_stackedbar_pe_release.rds",
  all_sentence_release_type                 = "all_sentence_release_type.rds",
  all_pie_release_type                      = "all_pie_release_type.rds",
  all_sentence_releases_race                = "all_sentence_releases_race.rds",
  all_bar_releases_race                     = "all_bar_releases_race.rds",
  all_sentence_releases_sex                 = "all_sentence_releases_sex.rds",
  all_bar_releases_sex                      = "all_bar_releases_sex.rds",
  all_sentence_releases_agerlse             = "all_sentence_releases_agerlse.rds",
  all_bar_releases_agerlse                  = "all_bar_releases_agerlse.rds",
  all_sentence_releases_fbi_index           = "all_sentence_releases_fbi_index.rds",
  all_bar_releases_fbi_index                = "all_bar_releases_fbi_index.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
