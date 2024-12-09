################################################################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Last Updated: November 15, 2024 (MAR)
# Description:
#   This script analyzes and visualizes prison population data related to parole
#   eligibility. It prepares data for various demographic and offense-related
#   analyses, generates pie charts and bar charts, constructs summary sentences,
#   and saves the outputs for downstream use.
#
#   - Filtering and summarizing prison population data based on parole eligibility
#     status, including current, future, and missing eligibility categories.
#   - Generating pie charts to visualize proportions of the prison population
#     by parole eligibility status across states.
#   - Creating bar charts and summary sentences for demographics (race, sex,
#     age), offense types, and sentence lengths.
#   - Analyzing trends and projections for people past their parole eligibility
#     date, with summary sentences and line charts for states.
#   - Saving all outputs (charts, sentences, and data summaries) as `.rds` files
#     for integration with the AV Parole web tool's Parole Eligibility tab.
################################################################################

# ---------------------------------------------------------------------------- #
# Pie charts of the prison population by parole eligibility status
# ---------------------------------------------------------------------------- #

# Filter the prison population data based on specified criteria
# The function filters to include only:
#   - People in prison for new crimes with sentence lengths of 1+ years (except life)
#   - States with active parole systems and low missingness (not in `states_to_exclude`)
#   - States that don't require filtering for admission type or sentence length (`states_nofilter`)
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)

# Calculate the total prison population by state and reporting year
# This serves as the denominator for proportion calculations later
total_pe_pop_by_rptyear <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop")

# Compute the prison population proportions by parole eligibility status
# Includes statuses: "Missing," "Current," or "Future"
# Joins with the total population to calculate percentages (`prop`) and adds tooltips
pe_status_pop <- ncrp_yearendpop_filtered |>
  mutate(parelig_status = case_when(
    parelig_status == "Current" ~ "Past Parole Eligibility at End of Year",
    parelig_status == "Future" ~ "Will Be Eligible Next Year",
    TRUE ~ parelig_status
  )) |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(total_pe_pop_by_rptyear, by = c("state", "rptyear")) |>
  mutate(prop = (n / yearendpop) * 100) |> # Calculate proportion
  fnc_create_tooltip(variable_label = "Parole Eligibility Status", variable = parelig_status) |> # Add tooltips
  fnc_filter_by_year(which_overall_year) # Filter data based on the best year for each state

# Generate pie charts visualizing parole eligibility status proportions for each state
# `fnc_hc_pie_chart` creates individual charts with data and accessibility text for each state
all_pie_pe_type <- fnc_hc_pie_chart(
  df = pe_status_pop,
  variable = "parelig_status"
)

# State example:
all_pie_pe_type$Georgia
all_pie_pe_type$Michigan

# Generate summary sentences for each state describing parole eligibility proportions
#  "Most recent data shows that 69 percent of people in prison were eligible for
#   parole and incarcerated past parole eligibility at the end of the year, while
#   another 31 will reach their parole eligibility next year."
all_sentence_pe_type <- {
  # Get the list of unique states from the filtered data
  states <- unique(pe_status_pop$state)

  # Use `map` to iterate over each state and generate a summary sentence
  map(states, function(state_name) {
    # Filter the data for the current state
    df <- pe_status_pop |> filter(state == state_name)

    # Extract the reporting year for the current state (assumes consistency across rows)
    year <- unique(df$rptyear)

    # Get proportions of people currently eligible and those eligible in the future
    current_prop <- df |> filter(parelig_status == "Past Parole Eligibility at End of Year") |> pull(prop)
    future_prop <- df |> filter(parelig_status == "Will Be Eligible Next Year") |> pull(prop)

    # Construct the summary sentence for the state
    paste0(
      "Most recent data shows that ",
      round(current_prop, 0),
      " percent of people in prison were eligible for parole and incarcerated ",
      "past parole eligibility at the end of the year,",
      " while another ", round(future_prop, 0),
      " were expected to reach their parole eligibility in the following year."
    )
  }) |> setNames(states) # Assign state names to the generated sentences
}

# State example:
all_sentence_pe_type$Georgia



# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #
# Description:
#   This script generates trend data, summary sentences, and line charts
#   for the percentage of people in prison who are past their parole eligibility.
#   It includes projections for years beyond 2020.
# ---------------------------------------------------------------------------- #

# Transform projection data for past parole eligibility
pe_proj_pop <- ncrp_projections |>
  select(state, year, pcnt_ppey_rules_wf, proj_pcnt_ppey, excl_state_year) |>
  mutate(
    # Assign observed percentage past parole eligibility (2010–2020)
    pct_past_pe = if_else(
      year >= 2010 & year <= 2020 & !is.na(pcnt_ppey_rules_wf),
      pcnt_ppey_rules_wf, # Use observed data
      if_else(year >= 2019 & year <= 2020, NA_real_, NA_real_) # Set as NA for specific years if no data exists
    ),
    # Assign projected percentages for 2021–2023
    proj_pct_past_pe = if_else(
      year > 2020 & year <= 2023 | (year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf)),
      proj_pcnt_ppey, # Use projections
      NA_real_ # Set as NA otherwise
    ),
    # Flag for whether projections were used instead of observed data
    used_projected_flag = year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf)
  ) |>
  # Remove years that should be excluded
  filter(excl_state_year == 0 | is.na(excl_state_year)) |>
  # Remove states we aren't including in the app
  filter(!state %in% states_to_exclude$state)

# Extract the list of unique states
states <- unique(pe_proj_pop$state)

# Generate summary sentences for all states
# "From 2010 to 2020, the percent of people in prison past parole eligibility
#  increased by 22 percent. Our forcasting model projects that the percentage of
#  people past their initial parole eligibility will remain around 68 percent."
# The `fnc_generate_projection_sentence` function creates state-specific summaries
all_sentence_pop_pe_by_year <- map(states, ~ fnc_generate_projection_sentence(.x, pe_proj_pop)) |>
  setNames(states) # Assign state names to the sentences for easy retrieval

# State examples:
all_sentence_pop_pe_by_year$Georgia
all_sentence_pop_pe_by_year$Colorado
all_sentence_pop_pe_by_year$Idaho
all_sentence_pop_pe_by_year$Hawaii

# Generate Line Charts for Past Parole Eligibility Projections
all_line_pop_pe_by_year <- map(states, function(x) {

  # Filter to state
  df1 <- pe_proj_pop |>
    filter(state == x)

  # Determine max year before projection line
  max_year <- df1 |>
    filter(!is.na(pct_past_pe)) |>
    group_by(state) |>
    summarize(max_year = max(year, na.rm = TRUE)) |>
    pull(max_year)

  # Adjust the years to include all desired ones
  all_years <- seq(min(df1$year, na.rm = TRUE), max(df1$year, na.rm = TRUE))

  # Filter data for the current state and prepare for charting
  df1 <- df1 |>
    complete(year = all_years, fill = list(pct_past_pe = NA, proj_pct_past_pe = NA)) |> # Ensure all years are included in graph
    mutate(
      # Get the last observed value for percentage past parole eligibility
      last_value_past_pe = last(na.omit(pct_past_pe)),

      # Identify the first year needing projection filling (if any)
      year_to_fill = if (any(!is.na(proj_pct_past_pe))) {
        min(year[!is.na(proj_pct_past_pe)], na.rm = TRUE) - 1 # Fill one year before the first projected year
      } else {
        NA_real_
      },

      # Fill projected values with the last observed value for identified years
      proj_pct_past_pe = if_else(
        is.na(proj_pct_past_pe) & year == year_to_fill,
        last_value_past_pe,
        proj_pct_past_pe
      )
    ) |>
    select(-last_value_past_pe, -year_to_fill) # Remove helper columns after processing

  # Define chart properties
  title <- "People in Prison Past Parole Eligibility by Year"
  hc_accessibility_text <- "This chart shows the percentage of people in prison who
  are past their parole eligibility year, with projections highlighted in red."

  # Create Highcharts object
  highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = title) |>
    hc_xAxis(categories = all_years, lineWidth = 1) |>
    hc_yAxis(
      title = list(text = "Percent Past Parole Eligibility"),
      min = 0, max = 100, # Define Y-axis range (0–100%)
      labels = list(format = "{value}%") # Add percentage format to Y-axis labels
    ) |>
    hc_add_series(
      name = "Past Parole Eligibility",
      data = round(df1$pct_past_pe, 0), # Add observed data
      color = teal, # Set line color
      marker = list(enabled = TRUE), # Enable markers on data points
      connectNulls = TRUE, # Connect lines even if there are missing values
      tooltip = list(valueSuffix = "%") # Tooltip showing percentage
    ) |>
    hc_add_series(
      name = "Projected Past Parole Eligibility",
      data = round(df1$proj_pct_past_pe, 0), # Add projected data
      color = red, # Set line color for projections
      marker = list(enabled = TRUE),
      connectNulls = TRUE,
      tooltip = list(valueSuffix = "%")
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(
      enabled = TRUE,
      filename = paste0(gsub(" ", "_", tolower(title)), "_") # Set export file name
    ) |>
    hc_caption(text = paste0(ncrp_source, ", ", min(df1$year), "-", max_year, " and ", csg_source)) |> # Add source caption
    fnc_add_hc_accessibility(hc_accessibility_text) # Add accessibility text
})

# Assign state names to the generated charts
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)

# Example state:
all_line_pop_pe_by_year$Georgia
all_line_pop_pe_by_year$Colorado
all_line_pop_pe_by_year$Idaho
all_line_pop_pe_by_year$Hawaii
rm(states)  # Cleanup: Remove the temporary `states` variable



# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Filter data to include only individuals who are currently eligible for parole
current_pe <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>  # Currently eligible for parole
  fnc_filter_by_year(which_overall_year)  # Ensure only data from the year needed by state

# Use the unconsolidated file for age since `ageyrend` is not available in the consolidated file
current_pe_unconsolidated <- fnc_filter_pe_population_criteria(
  ncrp_yearendpop_not_consolidated,  # Use unconsolidated data
  exclude = states_to_exclude,       # Exclude states with missing or invalid data
  dont_filter = states_nofilter) |>  # States that don't require filtering
  filter(parelig_status == "Current") |>
  fnc_filter_by_year(which_overall_year)  # Ensure only data from the year needed by state

# Summarize the prison population by various attributes
# This step aggregates data and calculates proportions for visualization and reporting
current_pe_race     <- fnc_summarize_data(current_pe, "race") |>                      # Summarize by race and ethnicity
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)                 # Exclude states with high missingness for race data
current_pe_sex       <- fnc_summarize_data(current_pe, "sex")                         # Summarize by sex
current_pe_ageyrend  <- fnc_summarize_data(current_pe_unconsolidated, "ageyrend")     # Summarize by age
current_pe_sentlgth  <- fnc_summarize_data(current_pe, "sentlgth")                    # Summarize by sentence length
current_pe_fbi_index <- fnc_summarize_data(current_pe, "fbi_index") |>                # Summarize by offense type
  fnc_group_offense_type()                                                            # Group offense types into broader categories

# Create a list of parameters for each category to streamline chart and sentence generation
categories <- list(
  list(data = current_pe_race,
       x_var = "race",
       metric = "Race and Ethnicity",
       source1 = ncrp_source, # Source 1 (NCRP)
       source2 = csg_source), # Source 2 (CSG Estimates)
  list(data = current_pe_sex,
       x_var = "sex",
       metric = "Sex",
       source1 = ncrp_source,
       source2 = csg_source),
  list(data = current_pe_ageyrend,
       x_var = "ageyrend",
       metric = "Age",
       source1 = ncrp_source,
       source2 = csg_source),
  list(data = current_pe_sentlgth,
       x_var = "sentlgth",
       metric = "Sentence Length",
       source1 = ncrp_source,
       source2 = csg_source),
  list(data = current_pe_fbi_index,
       x_var = "fbi_index",
       metric = "Offense Type",
       source1 = ncrp_source,
       source2 = csg_source))

# Initialize empty lists to store bar charts and sentences for each category
all_bar_pe <- list()
all_sentence_pe <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  # Generate bar charts for the current category
  all_bar_pe[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,                                    # Data for the current category
    x_var      = category$x_var,                                   # X-axis variable (e.g., race, sex)
    metric     = category$metric,                                  # Metric label (e.g., "Race and Ethnicity")
    type_desc  = "the prison population past parole eligibility",  # Description of the data type
    title_type = "People in Prison Past Parole Eligibility",       # Chart title prefix
    y_var      = "prop",                                           # Y-axis variable
    source1    = category$source1,                                 # Source 1 (NCRP)
    source2    = category$source2                                  # Source 2 (CSG Estimates)
  )

  # Generate sentences for the current category
  all_sentence_pe[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,                       # Data for the current category
    x_var     = category$x_var,                      # X-axis variable
    type_desc = "in prison past parole eligibility"  # Description of the data type
  )
}

# Assign chart and sentence names
all_bar_pe_race           <- all_bar_pe[["race"]]
all_sentence_pe_race      <- all_sentence_pe[["race"]]
all_bar_pe_sex            <- all_bar_pe[["sex"]]
all_sentence_pe_sex       <- all_sentence_pe[["sex"]]
all_bar_pe_ageyrend       <- all_bar_pe[["ageyrend"]]
all_sentence_pe_ageyrend  <- all_sentence_pe[["ageyrend"]]
all_bar_pe_sentlgth       <- all_bar_pe[["sentlgth"]]
all_sentence_pe_sentlgth  <- all_sentence_pe[["sentlgth"]]
all_bar_pe_fbi_index      <- all_bar_pe[["fbi_index"]]
all_sentence_pe_fbi_index <- all_sentence_pe[["fbi_index"]]

# Example states:
all_bar_pe_race$Georgia
all_sentence_pe_race$Georgia
all_bar_pe_race$Hawaii
all_sentence_pe_race$Hawaii

# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_type        = "all_sentence_pe_type.rds",
  all_pie_pe_type             = "all_pie_pe_type.rds",
  all_sentence_pop_pe_by_year = "all_sentence_pop_pe_by_year.rds",
  all_line_pop_pe_by_year     = "all_line_pop_pe_by_year.rds",
  all_sentence_pe_race        = "all_sentence_pe_race.rds",
  all_bar_pe_race             = "all_bar_pe_race.rds",
  all_sentence_pe_sex         = "all_sentence_pe_sex.rds",
  all_bar_pe_sex              = "all_bar_pe_sex.rds",
  all_sentence_pe_ageyrend    = "all_sentence_pe_ageyrend.rds",
  all_bar_pe_ageyrend         = "all_bar_pe_ageyrend.rds",
  all_sentence_pe_fbi_index   = "all_sentence_pe_fbi_index.rds",
  all_bar_pe_fbi_index        = "all_bar_pe_fbi_index.rds",
  all_sentence_pe_sentlgth    = "all_sentence_pe_sentlgth.rds",
  all_bar_pe_sentlgth         = "all_bar_pe_sentlgth.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))


