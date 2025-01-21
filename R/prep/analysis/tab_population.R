################################################################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Last Updated: January 16, 2025 (MAR)
# Description:
#   This script analyzes and visualizes trends in the prison population across
#   states and generates summary sentences and charts for key demographic and
#   offense-related categories. It includes functionality to filter and process
#   data from various sources (BJS and NCRP) to prepare state-specific insights
#   for year-end prison population trends and proportions.
#
#   - Filtering and summarizing prison population data by race, sex, age,
#     sentence length, and offense type.
#   - Generating summary sentences for population changes across years for
#     individual states.
#   - Creating bar charts for demographic breakdowns, offense types and sentence lengths.
#   - Generating line charts visualizing prison population trends over time.
#   - Saving all outputs (sentences and visualizations) to `.rds` files.
################################################################################

# ---------------------------------------------------------------------------- #
# Prison Population By Year
# ---------------------------------------------------------------------------- #

# Filter BJS prison population data
# Exclude states with high missingness or abolished parole (in `states_to_exclude`)
bjs_prison_pop_by_rptyear_filtered <- bjs_prison_pop_by_rptyear |>
  filter(!state %in% states_to_exclude$state)

# Get a list of unique states for iteration
states <- bjs_prison_pop_by_rptyear_filtered |>
  distinct(state) |>
  arrange(state) |>
  pull(state)

# Generate summary sentences for the prison population trends in each state
# Example sentence format:
# "From 2010 to 2022, the prison population decreased 17 percent,
#  changing from 56,432 in 2010 to 47,010 in 2022."
all_sentence_population_by_year <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- bjs_prison_pop_by_rptyear_filtered |>
    filter(state == x)

  # If no data is available for the state, return an appropriate message
  if (nrow(df1) == 0) {
    return(paste0("No valid data available for ", x, "."))
  }

  # Identify the earliest year with valid population data
  earliest_year <- min(df1$rptyear, na.rm = TRUE)
  earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]

  # Handle missing population data for the earliest year by finding the next available year
  if (is.na(earliest_year_population) | length(earliest_year_population) == 0) {
    earliest_year <- min(df1$rptyear[!is.na(df1$bjs_prison_population)], na.rm = TRUE)
    earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]
  }

  # Identify the most recent year with valid population data
  latest_year <- max(df1$rptyear, na.rm = TRUE)
  latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]

  # Handle missing population data for the latest year by finding the previous available year
  if (is.na(latest_year_population) | length(latest_year_population) == 0) {
    latest_year <- max(df1$rptyear[!is.na(df1$bjs_prison_population) & df1$rptyear < latest_year], na.rm = TRUE)
    latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]
  }

  # If population data is still missing after adjustments, return an appropriate message
  if (is.na(earliest_year_population) | is.na(latest_year_population)) {
    return(paste0("Population data is missing for ", x, "."))
  }

  # Calculate the percentage change in population between the earliest and latest years
  percent_change <- (latest_year_population - earliest_year_population) / earliest_year_population * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")  # Determine if the change is positive or negative
  percent_change_abs <- abs(round(percent_change, 0))  # Use absolute value for reporting

  # Construct the summary sentence
  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population ",
                      change_type, " ", percent_change_abs, " percent, changing from ",
                      format(earliest_year_population, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_population, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Assign state names to the generated sentences for easy access
all_sentence_population_by_year <- setNames(all_sentence_population_by_year, states)
all_sentence_population_by_year$Georgia

# Generate line charts for each state's prison population trends over time
all_line_population_by_year <- map(.x = states, .f = function(x) {

  # Filter data for the specific state
  df1 <- bjs_prison_pop_by_rptyear_filtered |>
    ungroup() |>
    filter(state == x) |>
    distinct() |>  # Ensure unique rows
    mutate(tooltip = paste0("Year: ", rptyear, "<br>",
                            "Year-End Population: ", bjs_prison_population))  # Add tooltips for chart interactivity

  # Add a margin to the maximum value for better chart visualization
  max_value <- max(df1$bjs_prison_population) * 1.1

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This line chart shows the year-end prison population in ",
                                  x, " from ", min(df1$rptyear), " to ",
                                  max(df1$rptyear), ". Each point on the chart represents the prison population for a specific year, ",
                                  "showing trends over time. The y-axis represents the number of people in prison, ",
                                  "and the x-axis represents the years. ",
                                  "The tooltip provides the exact year and the corresponding prison population.")

  # Define the chart title
  title <- "Prison Population by Year"

  # Download file title
  download_title <- paste0(gsub(" ", "_", tolower(title)), "_",
                           min(df1$rptyear), "-", max(df1$rptyear))

  # Space below chart to accompany logo
  bottom_margin_value <- 120

  # Create the Highchart
  highcharts <-
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = paste0(title, ", ", min(df1$rptyear), "-", max(df1$rptyear))) |>
    hc_yAxis(title = list(text = ""),
             min = 0,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "Population",  # Name of the series
        data = df1$bjs_prison_population,  # Data to visualize
        tooltip = list(
          pointFormat = "<b>Prison Population:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_caption(text = paste0("Source: ", bjs_source, ", ",
                             min(df1$rptyear), "-", max(df1$rptyear), "."),
               y = -30) |>
    fnc_add_logo_and_export(download_title, bottom_margin_value) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})

# Assign state names to the generated charts for easy access
all_line_population_by_year <- setNames(all_line_population_by_year, states)
rm(states)  # Cleanup: Remove the temporary `states` variable

# Example states:
all_line_population_by_year$Georgia
all_line_population_by_year$Hawaii

# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Filter the consolidated NCRP year-end population data for the selected year
current_yearendpop <- ncrp_yearendpop_consolidated |>
  fnc_filter_by_year(which_overall_year)  # Ensure only data for the best year is included

# Filter the non-consolidated NCRP year-end population data for the selected year
# This includes variables like `ageyrend` which are not present in the consolidated file
current_yearendpop_not_consolidated <- ncrp_yearendpop_not_consolidated |>
  fnc_filter_by_year(which_overall_year)

# Summarize the prison population data by various attributes for visualization and analysis

# NCRP data: Summarize population by age (using non-consolidated data due to `ageyrend` availability)
ncrp_population_ageyrend <- fnc_summarize_data(current_yearendpop_not_consolidated, "ageyrend")

# NCRP data: Summarize population by offense type (FBI Index)
ncrp_population_fbi_index <- fnc_summarize_data(current_yearendpop, "fbi_index") |>
  fnc_group_offense_type()  # Group offenses into broader categories like "Violent" or "Nonviolent"

# NCRP data: Summarize population by sentence length
ncrp_population_sentlgth <- fnc_summarize_data(current_yearendpop, "sentlgth")

# Create a list of categories to streamline chart and sentence generation
categories <- list(
  list(data = bjs_prison_pop_by_race,
       x_var = "race",
       metric = "Race and Ethnicity",
       source1 = bjs_source,
       source2 = NULL),
  list(data = bjs_prison_pop_by_sex,
       x_var = "sex",
       metric = "Sex",
       source1 = bjs_source,
       source2 = NULL),
  list(data = ncrp_population_ageyrend,
       x_var = "ageyrend",
       metric = "Age",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_population_sentlgth,
       x_var = "sentlgth",
       metric = "Sentence Length",
       source1 = ncrp_source,
       source2 = NULL),
  list(data = ncrp_population_fbi_index,
       x_var = "fbi_index",
       metric = "Offense Type",
       source1 = ncrp_source,
       source2 = NULL)
)



# ---------------------------------------------------------------------------- #
# Generate Sentences and Column Charts (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Initialize empty lists to store bar charts and sentences for each category
all_bar_population <- list()
all_sentence_population <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  # Generate bar charts for the current category
  all_bar_population[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,
    x_var      = category$x_var,
    metric     = category$metric,
    type       = "the prison population",
    title_type = "People in Prison",
    y_var      = "prop",
    source1    = category$source1,
    source2    = category$source2
  )

  # Generate sentences for the current category
  all_sentence_population[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type      = "in prison"
  )
}

# Access specific bar charts and sentences
all_bar_population_race           <- all_bar_population[["race"]]
all_sentence_population_race      <- all_sentence_population[["race"]]
all_bar_population_sex            <- all_bar_population[["sex"]]
all_sentence_population_sex       <- all_sentence_population[["sex"]]
all_bar_population_ageyrend       <- all_bar_population[["ageyrend"]]
all_sentence_population_ageyrend  <- all_sentence_population[["ageyrend"]]
all_bar_population_sentlgth       <- all_bar_population[["sentlgth"]]
all_sentence_population_sentlgth  <- all_sentence_population[["sentlgth"]]
all_bar_population_fbi_index      <- all_bar_population[["fbi_index"]]
all_sentence_population_fbi_index <- all_sentence_population[["fbi_index"]]

# Example states:
all_bar_population_sentlgth$Georgia
all_bar_population_race$Georgia
all_bar_population_sex$Georgia
all_bar_population_ageyrend$Georgia
all_bar_population_sentlgth$Arkansas
all_sentence_population_fbi_index$`New York`
all_sentence_population_fbi_index$Georgia

#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_population_by_year   = "all_sentence_population_by_year.rds",
  all_line_population_by_year       = "all_line_population_by_year.rds",
  all_sentence_population_race      = "all_sentence_population_race.rds",
  all_bar_population_race           = "all_bar_population_race.rds",
  all_sentence_population_sex       = "all_sentence_population_sex.rds",
  all_bar_population_sex            = "all_bar_population_sex.rds",
  all_sentence_population_ageyrend  = "all_sentence_population_ageyrend.rds",
  all_bar_population_ageyrend       = "all_bar_population_ageyrend.rds",
  all_sentence_population_fbi_index = "all_sentence_population_fbi_index.rds",
  all_bar_population_fbi_index      = "all_bar_population_fbi_index.rds",
  all_sentence_population_sentlgth  = "all_sentence_population_sentlgth.rds",
  all_bar_population_sentlgth       = "all_bar_population_sentlgth.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

