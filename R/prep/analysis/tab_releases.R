################################################################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Last Updated: December 5, 2024 (MAR)
# Description:
#   This script analyzes and visualizes trends in prison releases across states
#   and generates summary sentences and charts for key demographic and
#   offense-related categories. It includes functionality to filter and process
#   data from NCRP to prepare state-specific insights for release trends,
#   parole eligibility proportions, and release types.
#
#   - Filtering and summarizing release data by race, sex, age, sentence length,
#     and offense type.
#   - Generating summary sentences for prison release trends and parole-eligible
#     release proportions for individual states.
#   - Creating pie charts for conditional vs. unconditional release types.
#   - Creating bar charts for demographic breakdowns and offense types.
#   - Generating stacked bar charts visualizing parole-eligible release trends
#     over time.
#   - Saving all outputs (sentences and visualizations) to `.rds` files.
################################################################################

# ---------------------------------------------------------------------------- #
# Prison Release Trends
# ---------------------------------------------------------------------------- #

# Filter NCRP releases data to include only states with parole systems
# Exclude states with high missingness or abolished parole (in `states_to_exclude`)
ncrp_releases_filtered <- ncrp_releases_consolidated |> ############################# Amund, Will change to ncrp_releases_consolidated when complete
  filter(!state %in% states_to_exclude$state)

# Summarize total number of people released from prison by state and year
# Include data from 2010 onwards
ncrp_releases_by_year <- ncrp_releases_filtered |>
  group_by(state, rptyear) |>
  summarise(total = n(), .groups = "drop") |>  # Calculate total releases for each state and year
  left_join(which_overall_year, by = "state") |>  # Add the best year information for filtering
  filter(rptyear >= 2010)

# Get unique states to iterate over
states <- unique(ncrp_releases_by_year$state)

# Generate summary sentences describing prison release trends for each state
# "From 2010 to 2020, the number of people released from prison decreased 16 percent,
# dropping from 21,717 in 2010 to 18,298 in 2020."
all_sentence_releases_by_year <- map(.x = states, .f = function(x) {
  # Filter release data for the specific state
  df1 <- ncrp_releases_by_year %>% filter(state == x)

  # Identify the earliest and latest years with valid data
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)  # Use the selected `year_to_use` as the latest year

  # Retrieve the total number of releases for the earliest and latest years
  earliest_year_release <- df1$total[df1$rptyear == earliest_year]
  latest_year_release <- df1$total[df1$rptyear == latest_year]

  # Calculate the percentage change between the earliest and latest years
  percent_change <- (latest_year_release - earliest_year_release) / earliest_year_release * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")  # Determine if the trend is positive or negative
  percent_change_abs <- abs(round(percent_change, 0))  # Use absolute value for reporting

  # Construct a sentence summarizing the release trend for the state
  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the number of people released from prison ",
                      change_type, " ", percent_change_abs, " percent, from ",
                      format(earliest_year_release, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_release, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Assign state names to the list of sentences
all_sentence_releases_by_year <- setNames(all_sentence_releases_by_year, states)

# Example states:
all_sentence_releases_by_year$Georgia
all_sentence_releases_by_year$Connecticut
all_sentence_releases_by_year$Hawaii
all_sentence_releases_by_year$`West Virginia`
all_sentence_releases_by_year$Alaska

# Generate line charts for prison releases trends for each state
all_line_releases_by_year <- map(.x = states,  .f = function(x) {

  # Filter data for the specific state
  df1 <- ncrp_releases_by_year |>
    ungroup() |>
    filter(state == x) |>
    distinct()  # Ensure unique rows

  # Determine the maximum value for the y-axis, adding a small margin for better visualization
  max_value <- max(df1$total) * 1.1

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the number of releases in ",
                                  "the state of ", x, " by year.")

  # Create the Highchart
  highcharts <-
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = paste0("People Released From Prison by Year, ", min(df1$rptyear), "-", max(df1$rptyear))) |>
    hc_yAxis(title = list(text = ""),
             min = 0,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "Releases",  # Name of the series
        data = df1$total,  # Data to visualize
        tooltip = list(
          pointFormat = "<b>People Released From Prison:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5)) |>
    hc_caption(text = paste0(ncrp_source, ", ", min(df1$rptyear), "-", max(df1$rptyear))) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})

# Assign state names to the list of charts for easy access
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
rm(states)  # Cleanup: Remove the temporary `states` variable

# Example states:
all_line_releases_by_year$Georgia
all_line_releases_by_year$Connecticut
all_line_releases_by_year$Hawaii
all_line_releases_by_year$Alaska


# ---------------------------------------------------------------------------- #
# Proportion of Parole-Eligible People Released on Their Parole Eligibility Year
# ---------------------------------------------------------------------------- #

# Filter NCRP release data for individuals in prison for new crimes and sentence
# lengths not less than one year or life. Exclude states with high missingness
# or abolished parole, and avoid filtering certain states based on other criteria.
ncrp_releases_filtered_pop <- fnc_filter_pe_population_criteria(
  data = ncrp_releases_consolidated,
  exclude = states_to_exclude,
  dont_filter = states_nofilter) |>
  left_join(which_overall_year, by = "state")

# Were people released on or past their eligibility year?
# Summarize release data by parole eligibility status
# - Calculate proportions within each group (on parole eligibility vs. past parole eligibility)
ncrp_pe_releases_by_year <- ncrp_releases_filtered_pop |>
  filter(estimated_pey_status %in% c("past", "current_year")) |> ######################################## ASK SEBA both currently and past parole eligibility????
  group_by(state, rptyear, year_to_use, estimated_pey_status) |>
  summarise(n = n(), .groups = "drop") |>  # Count the number of releases
  group_by(state, rptyear, year_to_use) |>
  mutate(
    prop = n / sum(n),  # Calculate proportion of releases for each group
    estimated_pey_status = case_when(
      estimated_pey_status == "past" ~ "Past Parole Eligibility",
      estimated_pey_status == "current_year" ~ "On Parole Eligibility"
    )
  ) |>
  filter(rptyear >= 2010 & rptyear <= year_to_use)  # Include data from 2010 onwards up to the year by state

# Extract a list of unique states for iteration
states <- unique(ncrp_pe_releases_by_year$state)

# VISUALIZATION: Percentage of Parole-Eligible People Released On or Past Their Parole Eligibility Year
# Generate stacked bar charts for each state
all_stackedbar_pe_release <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- ncrp_pe_releases_by_year |> filter(state == x)

  # Define chart title and accessibility text
  title <- "People Released On Parole Eligibility Year vs. Past Parole Eligibility Year"
  hc_accessibility_text <- "This stacked bar chart shows the proportion of parole-eligible people released in each year, either on or past their parole eligibility year."

  # Create Highcharts stacked bar chart
  highcharts <- df1 |>
    hchart(
      type = "column",  # Stacked column chart
      hcaes(x = rptyear, y = prop, group = estimated_pey_status)  # X-axis: year, Y-axis: proportion, Group: status
    ) |>
    hc_yAxis(
      title = list(text = ""),
      max = 1,  # Set maximum value for proportions (100%)
      labels = list(formatter = JS("function() { return (this.value * 100) + '%'; }"))  # Display as percentages
    ) |>
    hc_xAxis(categories = unique(df1$rptyear), title = "") |>
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
    hc_plotOptions(series = list(stacking = "normal",  # Enable stacking
                                 animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4)) |>
    fnc_add_hc_accessibility(hc_accessibility_text) |>
    hc_caption(text = paste0(ncrp_source, ", ", min(df1$rptyear), "-", max(df1$rptyear), " and ", csg_source))

  return(highcharts)
})

# Assign state names to the list of charts for easy access
all_stackedbar_pe_release <- setNames(all_stackedbar_pe_release, states)

# Example states:
all_stackedbar_pe_release$Alabama
all_stackedbar_pe_release$Arkansas
all_stackedbar_pe_release$Colorado
all_stackedbar_pe_release$Connecticut
all_stackedbar_pe_release$Georgia
all_stackedbar_pe_release$Hawaii ########### look weird
all_stackedbar_pe_release$Idaho  ########### look weird
all_stackedbar_pe_release$Iowa
all_stackedbar_pe_release$Kentucky
all_stackedbar_pe_release$Louisiana
all_stackedbar_pe_release$Maryland ######### missing data in 2014 and 2015
all_stackedbar_pe_release$Massachusetts
all_stackedbar_pe_release$Michigan
all_stackedbar_pe_release$Mississippi
all_stackedbar_pe_release$Missouri
all_stackedbar_pe_release$Montana
all_stackedbar_pe_release$Nevada
all_stackedbar_pe_release$`New Hampshire`
all_stackedbar_pe_release$`New Jersey`
all_stackedbar_pe_release$`New York`
all_stackedbar_pe_release$`North Dakota`
all_stackedbar_pe_release$Oklahoma
all_stackedbar_pe_release$Pennsylvania
all_stackedbar_pe_release$`Rhode Island`
all_stackedbar_pe_release$`South Carolina`
all_stackedbar_pe_release$`South Dakota`
all_stackedbar_pe_release$Tennessee
all_stackedbar_pe_release$Texas
all_stackedbar_pe_release$`West Virginia`
all_stackedbar_pe_release$Wyoming

# Generate summary sentences describing the proportion of people released past their parole eligibility year
# "In the most recent year of data available, 76 percent of parole-eligible people
#  released in Georgia were released past their parole eligibility year, which is
#  an increase of 3 percent from 2010."
all_sentence_pe_proportion_released <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- ncrp_pe_releases_by_year |> filter(state == x)

  # Identify the earliest and latest years
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)

  # Extract data for the earliest and latest years
  df_earliest <- df1 |> filter(rptyear == earliest_year)
  df_latest <- df1 |> filter(rptyear == latest_year)

  # Check if both years have data
  if (nrow(df_earliest) == 0 || nrow(df_latest) == 0) {
    # Return a message if data is missing
    sentence <- paste0("Data for ", earliest_year, " or ", latest_year, " is missing for ", x, ".")
  } else {
    # Extract proportions for 'Past Parole Eligibility' in the earliest and latest years
    prop_past_parole_earliest <- df_earliest |> filter(estimated_pey_status == "Past Parole Eligibility") |> pull(prop)
    prop_past_parole_latest <- df_latest |> filter(estimated_pey_status == "Past Parole Eligibility") |> pull(prop)

    # Check if proportions exist
    if (length(prop_past_parole_earliest) == 0 || length(prop_past_parole_latest) == 0) {
      sentence <- paste0("Proportion data for Past Parole Eligibility is missing for ", earliest_year, " or ", latest_year, " in ", x, ".")
    } else {
      # Calculate percentage change in proportions
      prop_change <- (prop_past_parole_latest - prop_past_parole_earliest) / prop_past_parole_earliest * 100

      # Generate sentence based on whether there was an increase or decrease
      if (prop_change > 0) {
        sentence <- paste0(
          # "In ", latest_year, ", ", round(prop_past_parole_latest * 100, 0),
          "In the most recent year of data available, ", round(prop_past_parole_latest * 100, 0),
          " percent of parole-eligible people released in ", x, " were released past their parole eligibility year, ",
          "which is an increase of ", round(prop_change, 0), " percent from ", earliest_year, "."
        )
      } else {
        sentence <- paste0(
          # "In ", latest_year, ", ", round(prop_past_parole_latest * 100, 0),
          "In the most recent year of data available, ", round(prop_past_parole_latest * 100, 0),
          " percent of parole-eligible people released in ", x, " were released past their parole eligibility year, ",
          "which is a decrease of ", round(abs(prop_change), 0), " percent from ", earliest_year, "."
        )
      }
    }
  }

  return(sentence)
})

# Assign state names to the list of sentences
all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
rm(states)  # Cleanup: Remove the temporary `states` variable

# Example state:
all_sentence_pe_proportion_released$Georgia



# ---------------------------------------------------------------------------- #
# Releases by Release Type
# ---------------------------------------------------------------------------- #

# Filter NCRP release data to include only "Conditional" and "Unconditional" releases
# Remove "Other releases," although some states (e.g., Alabama) have a high proportion in this category
release_types <- ncrp_releases_filtered_pop |>
  filter(reltype == "Conditional release" | reltype == "Unconditional release") |>
  mutate(
    # Standardize release type labels for consistency
    reltype = case_when(
      reltype == "Conditional release" ~ "Conditional Release",
      reltype == "Unconditional release" ~ "Unconditional Release",
      TRUE ~ reltype
    )
  ) |>
  # Filter to include only data for the selected reporting year
  filter(rptyear == year_to_use) |>
  group_by(state, rptyear) |>
  count(reltype) |>
  # Calculate proportions of each release type for each state
  mutate(prop = n / sum(n))

# Extract a list of unique states for iteration
states <- unique(release_types$state)

# VISUALIZATION: Proportion of Conditional vs Unconditional Releases
# Generate a pie chart for each state
all_pie_release_type <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- release_types |>
    ungroup() |>
    filter(state == x)

  # Extract the reporting year for the state
  year <- unique(df1$rptyear)

  # Accessibility text for the chart
  hc_accessibility_text <- paste0(
    "This graph shows the proportion of the prison population released by release type (unconditional release vs. conditional release) in ",
    year, " in the state of ", x, "."
  )

  # Check if 100% of the releases are "Conditional Release"
  is_100_conditional <- all(df1$reltype == "Conditional Release")

  # Create a pie chart visualization
  highcharts <- highchart() |>
    hc_chart(type = "pie") |>
    hc_title(text = paste0("Conditional vs. Unconditional Releases")) |>
    hc_plotOptions(pie = list(
      # Rotate the chart by 90 degrees if all releases are conditional
      startAngle = if (is_100_conditional) 90 else 0,
      endAngle = if (is_100_conditional) 450 else 360,
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
      )
    )) |>
    hc_series(list(
      name = "Release Type",
      colorByPoint = TRUE,
      data = list_parse2(
        df1 |> mutate(y = n) |> select(name = reltype, y)
      )
    )) |>
    hc_add_theme(base_hc_theme) |>
    hc_colors(c(color4, color2)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(pointFormat = 'Number of People Released: {point.y}<br>Percentage of People Released: {point.percentage:.0f}%') |>
    hc_caption(text = paste0(ncrp_source, ", ", year)) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})

# Assign state names to the pie chart list for easy access
all_pie_release_type <- setNames(all_pie_release_type, states)

# Example states:
all_pie_release_type$Georgia
all_pie_release_type$`South Dakota`

# Generate sentences describing the proportion of conditional vs. unconditional releases
all_sentence_release_type <- map(.x = states, .f = function(x) {
  # Filter data for "Conditional Release" in the specific state
  df1 <- release_types |>
    filter(state == x & reltype == "Conditional Release")

  # Construct the summary sentence for the state
  sentences <- paste0(
    "Conditional release involves an individual’s release under specific conditions ",
    "and supervision, whereas unconditional release means the individual is released ",
    "without further obligations or restrictions. ",
    round(df1$prop * 100, 0),
    " percent of people released from prison were ", tolower(df1$reltype), "s."
  )

  return(sentences)
})

# Assign state names to the sentences list for easy access
all_sentence_release_type <- setNames(all_sentence_release_type, states)
rm(states)  # Cleanup: Remove the temporary `states` variable

# Example state:
all_sentence_release_type$Georgia



# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Filter release data to include only the appropriate year for each state
# This ensures that the analysis uses the best available year based on `which_overall_year`
current_releases <- ncrp_releases_consolidated |>  ################################## Will Replace with `ncrp_releases_consolidated` when ready
  fnc_filter_by_year(which_overall_year)

# Create a second filtered dataset for non-consolidated data
# May include variables not present in the consolidated dataset
current_releases_not_consolidated <- ncrp_releases_not_consolidated |>
  fnc_filter_by_year(which_overall_year)

# Summarize the prison releases by various attributes
ncrp_releases_race <- fnc_summarize_data(current_releases, "race") |>
  # Exclude states with high missingness for race data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
ncrp_releases_sex <- fnc_summarize_data(current_releases, "sex")
ncrp_releases_agerlse <- fnc_summarize_data(current_releases, "agerlse")  ################### Consider changing to `agerelease` if necessary
ncrp_releases_fbi_index <- fnc_summarize_data(current_releases, "fbi_index") |>
  # Group offenses into categories like "Violent" and "Nonviolent"
  fnc_group_offense_type()
ncrp_releases_sentlgth <- fnc_summarize_data(current_releases, "sentlgth")

# Create a list of parameters for each category to streamline chart and sentence generation
categories <- list(
  list(data = ncrp_releases_race,
       x_var = "race",
       metric = "Race and Ethnicity",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_releases_sex,
       x_var = "sex",
       metric = "Sex",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_releases_agerlse,
       x_var = "agerlse",
       metric = "Age",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_releases_sentlgth,
       x_var = "sentlgth",
       metric = "Sentence Length",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_releases_fbi_index,
       x_var = "fbi_index",
       metric = "Offense Type",
       source1 = ncrp_source,
       source2 = NULL)
)

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
    y_var      = "prop",
    source1    = category$source1,
    source2    = category$source2
  )

  all_sentence_releases[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type_desc = "released from prison"
  )
}

# Access specific bar charts and sentences
all_bar_releases_race           <- all_bar_releases[["race"]]
all_sentence_releases_race      <- all_sentence_releases[["race"]]
all_bar_releases_sex            <- all_bar_releases[["sex"]]
all_sentence_releases_sex       <- all_sentence_releases[["sex"]]
all_bar_releases_agerlse        <- all_bar_releases[["agerlse"]]
all_sentence_releases_agerlse   <- all_sentence_releases[["agerlse"]]
all_bar_releases_sentlgth       <- all_bar_releases[["sentlgth"]]
all_sentence_releases_sentlgth  <- all_sentence_releases[["sentlgth"]]
all_bar_releases_fbi_index      <- all_bar_releases[["fbi_index"]]
all_sentence_releases_fbi_index <- all_sentence_releases[["fbi_index"]]

# Example state:
all_bar_releases_race$Georgia
all_sentence_releases_fbi_index$Georgia

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
