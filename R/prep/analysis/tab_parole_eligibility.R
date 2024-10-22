#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: October 11, 2024 (MAR)
# Description:
#    This script generates parole eligibility visualizations and corresponding
#    summary sentences for the "Parole Eligibility" tab in state-specific reports.
#
#    Key Components:
#    1. Prison Population by Parole Eligibility Status:
#       - Filters NCRP prison population data for individuals serving sentences of 1+ years
#         (excluding life sentences) for new crimes. Analyzes those in prison past their
#         parole eligibility date.
#       - Generates pie charts and descriptive sentences for each state, detailing the prop of
#         individuals in each parole eligibility category (Current, Future, Missing).
#
#    2. Demographics:
#       - Breaks down the population in prison past their parole eligibility date by race, sex, and age.
#       - Produces bar charts and descriptive summaries for each state based on these demographics.
#
#    3. Offense Types:
#       - Classifies offenses into violent, nonviolent, or unknown categories.
#       - Creates visualizations and sentences summarizing the breakdown of people in prison past their
#         eligibility date by offense type.
#
#    4. Sentence Length:
#       - Analyzes sentence length distribution of those in prison past their parole
#         eligibility date, generating visualizations and sentences by state.
#
#    5. Trends Over Time:
#       - Tracks trends in the prop of people incarcerated past their parole eligibility across
#         multiple years (2010 onward), creating stacked bar charts and summary sentences for each state.
#
#    Output:
#       All visualizations and generated sentences are saved as `.rds` files for integration into the
#       interactive tool.
#######################################

# ---------------------------------------------------------------------------- #
# Pie charts of the prison population by parole eligibility status
# ---------------------------------------------------------------------------- #

# Function that filters the population data to include only people in prison for new crimes
# with sentence lengths 1+ years except life
# Only includes states with parole systems and without high misingness
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop)

# Total prison population by state and year
total_pe_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop")

# Prison population by parole eligibility status (missing, current, eligible in the future)
pe_status_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(total_pe_pop, by = c("state", "rptyear")) |>
  mutate(prop = (n / yearendpop)*100) |>
  fnc_create_tooltip(variable_label = "Parole Eligibility Status", variable = parelig_status)

# Extract unique states from the dataset
states <- unique(pe_status_pop$state)

# VISUALIZATION
# Create pie charts for each state based on parole eligibility status for the selected year
all_pie_pe_type <- map(.x = states, .f = function(x) {

  # Map colors to parole eligibility status categories
  color_mapping <- c("Future" = color2,
                     "Missing" = darkgray,
                     "Current" = color4)

  # Filter the dataset for the current state and selected year,
  # ensuring the correct color for each category
  df1 <- pe_status_pop |>
    ungroup() |>
    filter(state == x) |>
    filter(rptyear == select_year) |>
    mutate(color = color_mapping[parelig_status])

  # Accessibility text for screen readers
  hc_accessibility_text <-
    paste0("This pie chart shows the distribution of the prison population in ",
            x, " by parole eligibility status for the year ", select_year,
           ". The categories include those currently eligible, those eligible in the ",
           "future, and those with missing parole eligibility information.")

  # Chart title
  title <- "Prison Population by Parole Eligibility Status"

  # Create the highchart pie visualization
  highcharts <- fnc_hc_pie(df1, "parelig_status", title, hc_accessibility_text)

  return(highcharts)
})
# Assign state names to list
all_pie_pe_type <- setNames(all_pie_pe_type, states)
all_pie_pe_type$Georgia

# SENTENCE: In YEAR, 76 percent of people in prison were currently past their
#           parole eligibility. Another 23 percent will reach
#           their parole eligibility after YEAR.
all_sentence_pe_type <- map(states, function(x) {
  df <- pe_status_pop |>
    filter(state == x, rptyear == select_year)

  # Get the prop of people currently eligible and those eligible in the future
  current_prop <- df |> filter(parelig_status == "Current") |> pull(prop)
  future_prop <- df |> filter(parelig_status == "Future") |> pull(prop)

  # Generate the summary sentence
  paste0(
    "In ", select_year, ", ",
    round(current_prop, 0),
    " percent of people in prison were currently past their parole eligibility.",
    " Another ", round(future_prop, 0),
    " percent will reach their parole eligibility after ", select_year, "."
  )
})
# Assign state names to list
all_sentence_pe_type <- setNames(all_sentence_pe_type, states)
all_sentence_pe_type$Georgia
rm(states)




# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

# Get the number of people currently eligible for parole who are still incarcerated
# Group by state and report year, and create a summary count for each group
current_pe_pop <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(type = "Current")

# Get the total prison population by state and year
# Group by state and report year, and summarize the total population
total_pe_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(total_n = n(), .groups = "drop") |>
  mutate(type = "Total Population")

# Merge the PCE population with the total population data by state and year
# Calculate the prop of people past parole eligibility out of the total prison population
pe_pop_prop <- current_pe_pop |>
  left_join(total_pe_pop, by = c("state", "rptyear")) |>
  mutate(prop = n / total_n)

# SENTENCE: "From 2014 to YEAR, the percent of people in prison past parole eligibility increased by 14 percent."
# Loop through each unique state
states <- unique(pe_pop_prop$state)
all_sentence_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter data for the current state
  df <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010 & max(rptyear)) #############################################

  # Extract the earliest and latest years for each state
  earliest_year <- min(df$rptyear)
  latest_year <- max(df$rptyear)

  # Calculate the prop of people past parole eligibility for the earliest and latest years
  prop_earliest <- df |>
    filter(rptyear == earliest_year) |>
    pull(prop) * 100

  prop_latest <- df |>
    filter(rptyear == latest_year) |>
    pull(prop) * 100

  # Calculate the change in prop and determine if it increased, decreased, or stayed the same
  change <- prop_latest - prop_earliest

  # Determine if it increased, decreased, or stayed the same
  if (change > 0) {
    direction <- paste0("increased by ", abs(round(change, 0)), " percent")
  } else if (change < 0) {
    direction <- paste0("decreased by ", abs(round(change, 0)), " percent")
  } else {
    direction <- "stayed the same"
  }

  # Generate the final sentence
  sentence <- paste0(
    "From ", earliest_year, " to ", latest_year,
    ", the percent of people in prison past parole eligibility ", direction, "."
  )

  return(sentence)
})
# Assign state names to list
all_sentence_pop_pe_by_year <- setNames(all_sentence_pop_pe_by_year, states)
all_sentence_pop_pe_by_year$Georgia

# VISUALIZATION: Create a stacked bar chart showing the percentage of people past parole eligibility (PCE)
# and the remaining total prison population for each state over time
all_stackedbar_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter the data for the current state and only analyze data from 2010 onwards
  df1 <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010 & max(rptyear)) |>
    mutate(rptyear_fac = factor(rptyear))  # Convert years to a factor for the x-axis

  # Define chart title and accessibility text
  title <- "Percentage of Prison Population Incarcerated Past Parole Eligibility"
  hc_accessibility_text <- paste0("This chart shows the percentage of people past parole eligibility ",
                                  "and the remaining prison population for the state of ", x,
                                  " from ", min(df1$rptyear), " to ", max(df1$rptyear), ". ",
                                  "The bars represent the total prison population, with the portion of people ",
                                  "past their parole eligibility highlighted in a different color.")

  # Create the highchart visualization
  highcharts <- highchart() |>
    hc_xAxis(categories = df1$rptyear_fac) |>
    hc_yAxis(
      title = list(text = ""),
      max = 100,
      labels = list(format = "{value}%")
    ) |>
    hc_add_series(
      name = "Total Population (Remaining)",
      data = (1 - df1$prop) * 100,
      type = "column",
      stacking = "percent") |>
    hc_add_series(
      name = "In Prison Past Parole Eligibility",
      data = df1$prop * 100,
      type = "column",
      stacking = "percent") |>
    hc_plotOptions(series = list(stacking = "normal",
                                 pointWidth = 40,
                                 borderWidth = 3,
                                 borderColor = "#FFFFFF",
                                 minPointLength = 5)) |>
    hc_colors(c(color3, color4)) |>
    hc_legend(reversed = TRUE) |>
    hc_add_theme(base_hc_theme) |>
    hc_tooltip(pointFormat = '<b>{series.name}</b>: {point.y:.0f}%') |>
    hc_title(text = paste0(title, ", ",
                           min(df1$rptyear), "-", max(df1$rptyear))) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_",
                                   min(df1$rptyear), "_", max(df1$rptyear))) |>
    hc_caption(text = ncrp_csg_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})
# Assign state names to list
all_stackedbar_pop_pe_by_year <- setNames(all_stackedbar_pop_pe_by_year, states)
all_stackedbar_pop_pe_by_year$Georgia
rm(states)




# ---------------------------------------------------------------------------- #
# Demographics
# ---------------------------------------------------------------------------- #

# Prepare data for people in prison past their parole eligibility date by race, sex, and age
current_pe <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current")

# Use unconsolidated file for age - ageyrend not in consolidated file
current_pe_unconsolidated <- fnc_filter_pe_population_criteria(ncrp_yearendpop_not_consolidated) |>
  filter(parelig_status == "Current")

# Race data: Exclude states with high missingness in race categories
current_ped_race     <- fnc_summarize_data(current_pe, "race") |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
current_ped_sex      <- fnc_summarize_data(current_pe, "sex")
current_ped_ageyrend <- fnc_summarize_data(current_pe_unconsolidated, "ageyrend")

# ---------------------------------------------------------------------------- #
# Race
# ---------------------------------------------------------------------------- #

# VISUALIZATION: Bar charts for parole eligibility by race for each state
# Generate graph for each state
states <- unique(current_ped_race$state)
all_bar_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  this_metric <- "Race and Ethnicity"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_race,
                                   x_var      = "race",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility")

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_race <- setNames(all_bar_parole_eligibility_race, states)
all_bar_parole_eligibility_race$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = current_ped_race,
                                                 x_var      = "race",
                                                 type       = "in prison past parole eligibility")

  return(sentences)
})
all_sentence_parole_eligibility_race <- setNames(all_sentence_parole_eligibility_race, states)
all_sentence_parole_eligibility_race$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Sex
# ---------------------------------------------------------------------------- #

# VISUALIZATION: Bar charts for parole eligibility by sex for each state
# Generate graph for each state
states <- unique(current_ped_sex$state)
all_bar_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  this_metric <- "Sex"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_sex,
                                   x_var      = "sex",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility")

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_sex <- setNames(all_bar_parole_eligibility_sex, states)
all_bar_parole_eligibility_sex$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = current_ped_sex,
                                                 x_var      = "sex",
                                                 type       = "in prison past parole eligibility")

  return(sentences)
})
all_sentence_parole_eligibility_sex <- setNames(all_sentence_parole_eligibility_sex, states)
all_sentence_parole_eligibility_sex$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Age
# ---------------------------------------------------------------------------- #

# VISUALIZATION: Bar charts for parole eligibility by ageyrend for each state
# Generate graph for each state
states <- unique(current_ped_ageyrend$state)
all_bar_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {

  this_metric <- "Age"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_ageyrend,
                                   x_var      = "ageyrend",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility")

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_ageyrend <- setNames(all_bar_parole_eligibility_ageyrend, states)
all_bar_parole_eligibility_ageyrend$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var = x,
                                                 df        = current_ped_ageyrend,
                                                 x_var     = "ageyrend",
                                                 type      = "in prison past parole eligibility")

  return(sentences)
})
all_sentence_parole_eligibility_ageyrend <- setNames(all_sentence_parole_eligibility_ageyrend, states)
all_sentence_parole_eligibility_ageyrend$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# OFFENSE TYPE
# ---------------------------------------------------------------------------- #

# Get number and prop of people in prison past their parole eligibility year
# by offense
current_ped_fbi_index <- fnc_summarize_data(current_pe, "fbi_index") |>
  # Group offenses into violent vs nonviolent
  mutate(group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ "Other or Unknown"))

# Get prop of offenses that were violent and nonviolent
current_ped_offense_group <- current_ped_fbi_index |>
  select(state, fbi_index, group, n) |>
  filter(group == "Violent" | group == "Nonviolent") |>
  group_by(state, group) |>
  summarise(total_offenses = sum(n), .groups = 'drop') |>
  group_by(state) |>
  mutate(prop = total_offenses / sum(total_offenses))

# VISUALIZATION: Bar charts for parole eligibility by fbi_index for each state
# Generate graph for each state
states <- unique(current_ped_fbi_index$state)
all_bar_ped_fbi_index <- map(.x = states,  .f = function(x) {

  this_metric <- "Offense Type"
  highcharts <- fnc_hc_columnchart(state_var   = x,
                                   df          = current_ped_fbi_index,
                                   x_var       = "fbi_index",
                                   y_var       = "prop",
                                   metric      = this_metric,
                                   type        = "the prison population past parole eligibility",
                                   title_type  = "People in Prison Past Parole Eligibility",
                                   orientation = "horizontal")

  return(highcharts)
})
# Assign state names to list
all_bar_ped_fbi_index <- setNames(all_bar_ped_fbi_index, states)
all_bar_ped_fbi_index$Georgia

# SENTENCE: In YEAR, 69 percent of people in prison past parole eligibility were
#           in prison for violent offenses and 31 percent for nonviolent offenses.
#           Most people who were incarcerated past parole eligibility were serving
#           time for aggravated or simple assault (21%) and robbery (20%) offenses.
# Generate sentence for each state
all_sentence_parole_eligibility_fbi_index <- map(.x = states,  .f = function(x) {

  # Get the top group
  df1 <- current_ped_offense_group  |>
    filter(state == x) |>
    arrange(-prop)

  # Check if there's missing data in df1
  if (nrow(df1) < 2 || any(is.na(df1$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing for the top offense groups."))
  }

  # Violent vs Nonviolent breakdown sentence
  violent_prop <- df1 |> filter(group == "Violent") |> pull(prop) * 100
  nonviolent_prop <- df1 |> filter(group == "Nonviolent") |> pull(prop) * 100

  group_sentence <- paste0("In ", select_year, ", ", round(violent_prop, 0),
                           " percent of people in prison past parole eligibility were in prison for violent offenses and ",
                           round(nonviolent_prop, 0), " percent for nonviolent offenses.")

  # Get the top two FBI index categories
  df2 <- current_ped_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check if there's missing data in df2
  if (nrow(df2) < 2 || any(is.na(df2$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing."))
  }

  # Construct the sentence for the FBI index breakdown
  fbi_sentence <- paste0("Most people who were incarcerated past parole eligibility were serving time for ",
                         tolower(df2$fbi_index[1]), " (", round(df2$prop[1], 0), " percent) and ",
                         tolower(df2$fbi_index[2]), " (", round(df2$prop[2], 0), " percent) offenses.")

  # Combine the sentences
  sentences <- paste0(group_sentence, " ", fbi_sentence)

  return(sentences)
})
# Assign state names to list
all_sentence_parole_eligibility_fbi_index <- setNames(all_sentence_parole_eligibility_fbi_index, states)
all_sentence_parole_eligibility_fbi_index$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# SENTENCE LENGTH
# ---------------------------------------------------------------------------- #

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_summarize_data(current_pe, "sentlgth")

# VISUALIZATION: Bar charts for parole eligibility by sentlgth for each state
# Generate graph for each state
states <- unique(current_ped_sentlgth$state)
all_bar_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  this_metric <- "Sentence Length"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_sentlgth,
                                   x_var      = "sentlgth",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility")

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_sentlgth <- setNames(all_bar_parole_eligibility_sentlgth, states)
all_bar_parole_eligibility_sentlgth$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var = x,
                                                 df        = current_ped_sentlgth,
                                                 x_var     = "sentlgth",
                                                 type      = "in prison past parole eligibility")

  return(sentences)
})
all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia
rm(states)





# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_type                      = "all_sentence_pe_type.rds",
  all_pie_pe_type                           = "all_pie_pe_type.rds",
  all_sentence_pop_pe_by_year               = "all_sentence_pop_pe_by_year.rds",
  all_stackedbar_pop_pe_by_year             = "all_stackedbar_pop_pe_by_year.rds",
  all_sentence_parole_eligibility_race      = "all_sentence_parole_eligibility_race.rds",
  all_bar_parole_eligibility_race           = "all_bar_parole_eligibility_race.rds",
  all_sentence_parole_eligibility_sex       = "all_sentence_parole_eligibility_sex.rds",
  all_bar_parole_eligibility_sex            = "all_bar_parole_eligibility_sex.rds",
  all_sentence_parole_eligibility_ageyrend  = "all_sentence_parole_eligibility_ageyrend.rds",
  all_bar_parole_eligibility_ageyrend       = "all_bar_parole_eligibility_ageyrend.rds",
  all_sentence_parole_eligibility_fbi_index = "all_sentence_parole_eligibility_fbi_index.rds",
  all_bar_ped_fbi_index                     = "all_bar_ped_fbi_index.rds",
  all_sentence_parole_eligibility_sentlgth  = "all_sentence_parole_eligibility_sentlgth.rds",
  all_bar_parole_eligibility_sentlgth       = "all_bar_parole_eligibility_sentlgth.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

