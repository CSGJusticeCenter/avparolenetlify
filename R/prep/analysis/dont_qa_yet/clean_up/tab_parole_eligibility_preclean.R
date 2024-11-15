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
#       - Classifies offenses into violent and nonviolent categories.
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
# Only includes states with parole systems and without high missingness
# Includes states don't need to be filtered by admission type or sentence length
# These states are in states_nofilter
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)

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

  select_year <- fnc_determine_select_year(x, which_overall_year)

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
            x, " by parole eligibility status in ", select_year,
           ". The categories include those currently eligible for parole, those eligible in the ",
           "future, and those with missing parole eligibility information.")

  # Chart title
  title <- "Prison Population by Parole Eligibility Status"

  # Create the highchart pie visualization
  highcharts <- fnc_hc_pie(df = df1,
                           variable = "parelig_status",
                           title = title,
                           accessibility_text = hc_accessibility_text,
                           year = select_year,
                           source = ncrp_csg_source)

  return(highcharts)
})
# Assign state names to list
all_pie_pe_type <- setNames(all_pie_pe_type, states)
all_pie_pe_type$Georgia
all_pie_pe_type$Connecticut
all_pie_pe_type$Hawaii

# SENTENCE: In YEAR, 76 percent of people in prison were currently past their
#           parole eligibility. Another 23 percent will reach
#           their parole eligibility after YEAR.
all_sentence_pe_type <- map(states, function(x) {

  select_year <- fnc_determine_select_year(x, which_overall_year)

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
all_sentence_pe_type$Connecticut
all_sentence_pe_type$Hawaii
rm(states)




# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

# Use imputed and projected data for people past parole eligibility
# Created by Seba Guzman (CSG Research) in Stata
# Imported in import_format.R
pe_pop_prop <- ncrp_projections |>
  select(state, year, pcnt_ppey_rules_wf, proj_pcnt_ppey) |>
  mutate(
    # Use pcnt_ppey_rules_wf for years between 2010 and 2020, with a fallback to projected values for 2019-2020
    pct_past_pe = case_when(
      year >= 2010 & year <= 2020 & !is.na(pcnt_ppey_rules_wf) ~ pcnt_ppey_rules_wf,
      year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf) ~ NA_real_,  # Set to NA if using projection
      TRUE ~ NA_real_
    ),
    # Use proj_pct_ppey for years from 2021 to 2023 or when projections are needed for 2019-2020
    proj_pct_past_pe = case_when(
      year > 2020 & year <= 2023 ~ proj_pcnt_ppey,
      year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf) ~ proj_pcnt_ppey,
      TRUE ~ NA_real_
    ),
    # Flag to indicate when projected data is used
    used_projected_flag = case_when(
      year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf) ~ TRUE,
      TRUE ~ FALSE
    )
  )

generate_projection_sentence <- function(state_name, data) {
  # Filter data for the given state
  state_data <- data |> filter(state == state_name)

  # Identify the earliest and latest years for pct_past_pe
  earliest_year_past <- min(state_data |> filter(!is.na(pct_past_pe)) |> pull(year), na.rm = TRUE)
  latest_year_past <- max(state_data |> filter(!is.na(pct_past_pe)) |> pull(year), na.rm = TRUE)

  # Extract the values for these years
  pct_earliest <- state_data |> filter(year == earliest_year_past) |> pull(pct_past_pe)
  pct_latest <- state_data |> filter(year == latest_year_past) |> pull(pct_past_pe)

  # Calculate the percentage change for pct_past_pe and round
  change_past <- if (!is.na(pct_earliest) && !is.na(pct_latest)) {
    round(((pct_latest - pct_earliest) / pct_earliest) * 100, 1)
  } else {
    NA
  }

  # Identify the earliest and latest years for proj_pct_past_pe
  valid_proj_years <- state_data |> filter(!is.na(proj_pct_past_pe)) |> pull(year)
  if (length(valid_proj_years) > 0) {
    earliest_year_proj <- min(valid_proj_years, na.rm = TRUE)
    latest_year_proj <- max(valid_proj_years, na.rm = TRUE)
  } else {
    earliest_year_proj <- NA
    latest_year_proj <- NA
  }

  # Extract the projection values for these years
  if (!is.na(earliest_year_proj) && !is.na(latest_year_proj)) {
    proj_earliest <- state_data |> filter(year == earliest_year_proj) |> pull(proj_pct_past_pe)
    proj_latest <- state_data |> filter(year == latest_year_proj) |> pull(proj_pct_past_pe)

    # Calculate the percentage change for proj_pct_past_pe and round
    change_proj <- if (!is.na(proj_earliest) && !is.na(proj_latest)) {
      round(((proj_latest - proj_earliest) / proj_earliest) * 100, 1)
    } else {
      NA
    }
  } else {
    change_proj <- NA
  }

  # Check if 2019 or 2020 uses projected data
  used_projected_2019 <- state_data |> filter(year == 2019) |> pull(used_projected_flag)
  used_projected_2020 <- state_data |> filter(year == 2020) |> pull(used_projected_flag)

  # Construct a combined note for 2019 and 2020
  note <- if (used_projected_2019 & used_projected_2020) {
    " Note: 2019 and 2020 data use projections."
  } else if (used_projected_2019) {
    " Note: 2019 data uses projections."
  } else if (used_projected_2020) {
    " Note: 2020 data uses projections."
  } else {
    ""
  }

  # Form the sentence based on the changes
  sentence <- paste0(
    "From ", earliest_year_past, " to ", latest_year_past,
    ", the percent of people in prison past parole eligibility ",
    if (!is.na(change_past)) {
      if (change_past > 0) {
        paste0("increased by ", change_past, " percent. ")
      } else if (change_past < 0) {
        paste0("decreased by ", abs(change_past), " percent. ")
      } else {
        "remained the same. "
      }
    } else {
      "has insufficient data to determine a change. "
    },
    if (!is.na(earliest_year_proj) && !is.na(latest_year_proj)) {
      paste0(
        "We've projected that from ", earliest_year_proj, " to ", latest_year_proj,
        ", the percent of people past parole eligibility ",
        if (!is.na(change_proj)) {
          if (change_proj > 0) {
            paste0("will increase by ", change_proj, " percent")
          } else if (change_proj < 0) {
            paste0("will decrease by ", abs(change_proj), " percent")
          } else {
            "will not change (0 percent change)"
          }
        } else {
          "has insufficient data to project a change"
        },
        "."
      )
    } else {
      "Projected data is insufficient to provide a future change."
    },
    note
  )

  return(sentence)
}

# Generate sentences for all states
states <- unique(pe_pop_prop$state)
all_sentence_pop_pe_by_year <- map(states, ~ generate_projection_sentence(.x, pe_pop_prop))
all_sentence_pop_pe_by_year <- setNames(all_sentence_pop_pe_by_year, states)

# Example sentences for Michigan
all_sentence_pop_pe_by_year$Michigan
all_sentence_pop_pe_by_year$Georgia
all_sentence_pop_pe_by_year$Connecticut
all_sentence_pop_pe_by_year$Hawaii

# VISUALIZATION: Prison Population Past Parole Eligibility by Year
# Generate chart for each state
all_line_pop_pe_by_year <- map(.x = states, .f = function(x) {
  # Filter data for the state
  df1 <- pe_pop_prop |>
    filter(state == x) |>
    mutate(
      # Find the last non-NA value in pct_past_pe
      last_value_past_pe = last(na.omit(pct_past_pe)),

      # Identify the year just before the projections start
      year_to_fill = ifelse(
        any(!is.na(proj_pct_past_pe)),
        min(year[!is.na(proj_pct_past_pe)]) - 1,
        NA
      ),

      # Fill the NA in proj_pct_past_pe for the identified year
      proj_pct_past_pe = ifelse(
        is.na(proj_pct_past_pe) & year == year_to_fill,
        last_value_past_pe,
        proj_pct_past_pe
      )
    ) |>
    select(-last_value_past_pe, -year_to_fill)  # Remove temporary columns

  title <- "People in Prison Past Parole Eligibility by Year"
  hc_accessibility_text <- "This chart shows the percentage of people in prison who are
  past their parole eligibility year, with projections highlighted in red."

  # Create the Highcharts object with updated configurations
  highcharts <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = paste(title)) |>
    hc_xAxis(categories = df1$year, lineWidth = 1) |>
    hc_yAxis(
      title = list(text = "Percent Past Parole Eligibility"),
      min = 0,
      max = 100,
      labels = list(format = "{value}%")  # Add percentage to y-axis labels
    ) |>
    # Series for actual data
    hc_add_series(
      name = "Past Parole Eligibility",
      data = round(df1$pct_past_pe, 1),  # Round to one decimal place
      color = blue,
      marker = list(enabled = TRUE),
      connectNulls = TRUE,
      tooltip = list(valueSuffix = "%")  # Add % to the tooltip
    ) |>
    # Series for projected data
    hc_add_series(
      name = "Projected Past Parole Eligibility",
      data = round(df1$proj_pct_past_pe, 1),  # Round to one decimal place
      color = red,
      marker = list(enabled = TRUE),
      connectNulls = TRUE,
      tooltip = list(valueSuffix = "%")  # Add % to the tooltip
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(
      enabled = TRUE,
      filename = paste0(gsub(" ", "_", tolower(title)), "_")
    ) |>
    hc_caption(text = ncrp_csg_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})

# Assign state names to the list
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)

# Example usage
all_line_pop_pe_by_year$Georgia
all_line_pop_pe_by_year$Connecticut
all_line_pop_pe_by_year$Michigan
all_line_pop_pe_by_year$Idaho

# ---------------------------------------------------------------------------- #
# Demographics
# ---------------------------------------------------------------------------- #

# Prepare data for people in prison past their parole eligibility date by race, sex, and age
# Current = currently eligible for parole release (includes people past parole eligibility)
current_pe <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current")

# Use unconsolidated file for age - ageyrend not in consolidated file
current_pe_unconsolidated <-
  fnc_filter_pe_population_criteria(ncrp_yearendpop_not_consolidated,
                                    exclude = states_to_exclude,
                                    dont_filter = states_nofilter) |>
  filter(parelig_status == "Current")

# Race data: Exclude states with high missingness in race categories
current_ped_race <- fnc_summarize_data(current_pe, "race", which_overall_year) |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
current_ped_sex      <- fnc_summarize_data(current_pe, "sex", which_overall_year)
current_ped_ageyrend <- fnc_summarize_data(current_pe_unconsolidated, "ageyrend", which_overall_year)

# ---------------------------------------------------------------------------- #
# Race
# ---------------------------------------------------------------------------- #

# VISUALIZATION: Bar charts for parole eligibility by race for each state
# Generate graph for each state
states <- unique(current_ped_race$state)
all_bar_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  this_metric <- "Race and Ethnicity"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_race,
                                   x_var      = "race",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility",
                                   year       = select_year)

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_race <- setNames(all_bar_parole_eligibility_race, states)
all_bar_parole_eligibility_race$Georgia
all_bar_parole_eligibility_race$Hawaii

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = current_ped_race,
                                                 x_var      = "race",
                                                 type       = "in prison past parole eligibility",
                                                 year       = select_year)

  return(sentences)
})
all_sentence_parole_eligibility_race <- setNames(all_sentence_parole_eligibility_race, states)
all_sentence_parole_eligibility_race$Georgia
all_sentence_parole_eligibility_race$Hawaii
rm(states)



# ---------------------------------------------------------------------------- #
# Sex
# ---------------------------------------------------------------------------- #

# VISUALIZATION: Bar charts for parole eligibility by sex for each state
# Generate graph for each state
states <- unique(current_ped_sex$state)
all_bar_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  this_metric <- "Sex"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_sex,
                                   x_var      = "sex",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility",
                                   year       = select_year)

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_sex <- setNames(all_bar_parole_eligibility_sex, states)
all_bar_parole_eligibility_sex$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = current_ped_sex,
                                                 x_var      = "sex",
                                                 type       = "in prison past parole eligibility",
                                                 year       = select_year)

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

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  this_metric <- "Age"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_ageyrend,
                                   x_var      = "ageyrend",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility",
                                   year       = select_year)

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_ageyrend <- setNames(all_bar_parole_eligibility_ageyrend, states)
all_bar_parole_eligibility_ageyrend$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {

  # Determine year based on state
  select_year <- fnc_determine_select_year(x, which_overall_year)

  sentences <- fnc_generate_columnchart_sentence(state_var = x,
                                                 df        = current_ped_ageyrend,
                                                 x_var     = "ageyrend",
                                                 type      = "in prison past parole eligibility",
                                                 year       = select_year)

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
current_ped_fbi_index <- fnc_summarize_data(current_pe, "fbi_index", which_overall_year) |>
  # Group offenses into violent vs nonviolent
  mutate(offense_group = case_when(
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
  select(state, fbi_index, offense_group, n) |>
  filter(offense_group == "Violent" | offense_group == "Nonviolent") |>
  group_by(state, offense_group) |>
  summarise(total_offenses = sum(n), .groups = 'drop') |>
  group_by(state) |>
  mutate(prop = total_offenses / sum(total_offenses))

# VISUALIZATION: Bar charts for parole eligibility by fbi_index for each state
# Generate graph for each state
states <- unique(current_ped_fbi_index$state)
all_bar_ped_fbi_index <- map(.x = states,  .f = function(x) {

  select_year <- fnc_determine_select_year(x, which_overall_year)
  this_metric <- "Offense Type"
  highcharts <- fnc_hc_columnchart(state_var   = x,
                                   df          = current_ped_fbi_index,
                                   x_var       = "fbi_index",
                                   y_var       = "prop",
                                   metric      = this_metric,
                                   type        = "the prison population past parole eligibility",
                                   title_type  = "People in Prison Past Parole Eligibility",
                                   orientation = "horizontal",
                                   year        = select_year)

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
all_sentence_parole_eligibility_fbi_index <- map(.x = states, .f = function(x) {

  select_year <- fnc_determine_select_year(x, which_overall_year)

  # Get the top offense_group
  df1 <- current_ped_offense_group |>
    filter(state == x) |>
    arrange(-prop)

  # Check if there's missing data in df1
  if (nrow(df1) < 2 || any(is.na(df1$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing for the top offense groups."))
  }

  # Violent vs Nonviolent breakdown sentence
  violent_prop <- df1 |> filter(offense_group == "Violent") |> pull(prop) * 100
  nonviolent_prop <- df1 |> filter(offense_group == "Nonviolent") |> pull(prop) * 100

  offense_group_sentence <- paste0("In ", select_year, ", ", round(violent_prop, 0),
                           " percent of people in prison past parole eligibility were in prison for violent offenses and ",
                           round(nonviolent_prop, 0), " percent for nonviolent offenses.")

  # Get the top FBI index categories
  df2 <- current_ped_fbi_index |>
    filter(state == x) |>
    arrange(-prop)

  # Check if there's missing data in df2
  if (nrow(df2) < 2 || any(is.na(df2$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing."))
  }

  # Get the maximum proportion and filter for categories with that value
  max_prop <- max(round(df2$prop, 0))

  top_categories <- df2 |>
    filter(round(prop, 0) == max_prop) |> # Select categories with the highest proportion
    arrange(desc(prop)) # Ensure they are sorted

  # Construct the sentence for the FBI index breakdown
  fbi_sentences <- top_categories |>
    mutate(fbi_sentence = paste0(tolower(fbi_index), " (", round(prop, 0), " percent)")) |>
    pull(fbi_sentence)

  # Use commas to separate categories, adding "and" before the last item with a space
  fbi_sentence_final <- if (length(fbi_sentences) > 1) {
    paste(paste(fbi_sentences[-length(fbi_sentences)], collapse = ", "),
          ", and ", fbi_sentences[length(fbi_sentences)], sep = "")
  } else {
    fbi_sentences
  }

  # Combine the sentences
  sentences <- paste0(offense_group_sentence, " Most people who were incarcerated past parole eligibility were serving time for ", fbi_sentence_final, " offenses.")

  return(sentences)
})

# Assign state names to the list
all_sentence_parole_eligibility_fbi_index <- setNames(all_sentence_parole_eligibility_fbi_index, states)
all_sentence_parole_eligibility_fbi_index$Georgia
rm(states)





# ---------------------------------------------------------------------------- #
# SENTENCE LENGTH
# ---------------------------------------------------------------------------- #

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_summarize_data(current_pe, "sentlgth", which_overall_year)

# VISUALIZATION: Bar charts for parole eligibility by sentlgth for each state
# Generate graph for each state
states <- unique(current_ped_sentlgth$state)
all_bar_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  select_year <- fnc_determine_select_year(x, which_overall_year)

  this_metric <- "Sentence Length"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = current_ped_sentlgth,
                                   x_var      = "sentlgth",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population past parole eligibility",
                                   title_type = "People in Prison Past Parole Eligibility",
                                   year       = select_year)

  return(highcharts)
})
# Assign state names to list
all_bar_parole_eligibility_sentlgth <- setNames(all_bar_parole_eligibility_sentlgth, states)
all_bar_parole_eligibility_sentlgth$Georgia

# SENTENCE: "In YEAR, 60 percent of people in prison past parole eligibility were male."
# Generate sentence for each state
all_sentence_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  select_year <- fnc_determine_select_year(x, which_overall_year)

  sentences <- fnc_generate_columnchart_sentence(state_var = x,
                                                 df        = current_ped_sentlgth,
                                                 x_var     = "sentlgth",
                                                 type      = "in prison past parole eligibility",
                                                 year      = select_year)

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
  all_line_pop_pe_by_year                   = "all_line_pop_pe_by_year.rds",
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
