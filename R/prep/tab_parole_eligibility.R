#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: October 1, 2024 (MAR)
# Description:
#    This script generates parole eligibility visualizations and corresponding
#    summary sentences for the "Parole Eligibility" tab in state-specific reports.
#
#    Key Components:
#    1. Prison Population by Parole Eligibility Status:
#       - Filters NCRP prison population data for individuals serving sentences of 1+ years
#         (excluding life sentences) for new crimes. Analyzes those in prison past their
#         parole eligibility date.
#       - Generates pie charts and descriptive sentences for each state, detailing the proportion of
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
#       - Tracks trends in the proportion of people incarcerated past their parole eligibility across
#         multiple years (2010 onward), creating stacked bar charts and summary sentences for each state.
#
#    Output:
#       All visualizations and generated sentences are saved as `.rds` files for integration into the
#       interactive tool.
#######################################

# ---------------------------------------------------------------------------- #
# Prison Population by PE Status
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
  mutate(prop = n / yearendpop) |>
  fnc_create_tooltip(variable_label = "Parole Eligibility Status", variable = parelig_status)

# VISUALIZATION: Prison Population by Parole Eligibility Status
states <- unique(pe_status_pop$state)
all_pie_pe_type <- map(.x = states, .f = function(x) {

  # Define color mapping for categories
  color_mapping <- c("Future" = color2,
                     "Missing" = darkgray,
                     "Current" = color4)

  df1 <- pe_status_pop |>
    ungroup() |>
    filter(state == x) |>
    filter(rptyear == select_year) |>
    # Ensure each category gets the correct color
    mutate(color = color_mapping[parelig_status])

  hc_accessibility_text <- paste0("This pie chart shows the distribution of the prison population in ",
                                  x, " by parole eligibility status for the year ", select_year,
                                  ". The categories include those currently eligible, those eligible in the future, and those with missing parole eligibility information.")
  title <- "Prison Population by Parole Eligibility Status"

  highcharts <- fnc_hc_pie(df1, "parelig_status", hc_accessibility_text)

  return(highcharts)
})
all_pie_pe_type <- setNames(all_pie_pe_type, states)
all_pie_pe_type$Georgia

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison.
all_sentence_pe_type <- map(states, function(x) {
  df <- pe_status_pop |>
    filter(state == x, rptyear == select_year)

  current_prop <- df |> filter(parelig_status == "Current") |> pull(prop)
  future_prop <- df |> filter(parelig_status == "Future") |> pull(prop)

  paste0(
    "In ", select_year, ", ",
    round(current_prop * 100, 0),
    " percent of people in prison were currently past their parole eligibility.",
    " Another ", round(future_prop * 100, 0),
    " percent will reach their parole eligibility after ", select_year, "."
  )
})
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
# Calculate the proportion of people past parole eligibility out of the total prison population
pe_pop_prop <- current_pe_pop |>
  left_join(total_pe_pop, by = c("state", "rptyear")) |>
  mutate(proportion = n / total_n)

# Generate sentences summarizing the proportion of PCE population changes over time for each state
# Loop through each unique state
states <- unique(pe_pop_prop$state)
all_sentence_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter data for the current state
  df <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010)

  # Get the earliest and latest years
  earliest_year <- min(df$rptyear)
  latest_year <- max(df$rptyear)

  # Get the proportion of people past parole eligibility for the earliest and latest years
  proportion_earliest <- df |>
    filter(rptyear == earliest_year) |>
    pull(proportion) * 100

  proportion_latest <- df |>
    filter(rptyear == latest_year) |>
    pull(proportion) * 100

  # Calculate the change in proportion
  change <- proportion_latest - proportion_earliest

  # Determine if it increased, decreased, or stayed the same
  if (change > 0) {
    direction <- paste0("increased by ", abs(round(change, 0)), " percent")
  } else if (change < 0) {
    direction <- paste0("decreased by ", abs(round(change, 0)), " percent")
  } else {
    direction <- "stayed the same"
  }

  # Generate the sentence
  sentence <- paste0(
    "From ", earliest_year, " to ", latest_year,
    ", the percent of people in prison past parole eligibility ", direction, "."
  )

  return(sentence)
})
all_sentence_pop_pe_by_year <- setNames(all_sentence_pop_pe_by_year, states)
all_sentence_pop_pe_by_year$Georgia

# VISUALIZATION: Create a stacked bar chart showing the percentage of people past parole eligibility (PCE)
# and the remaining total prison population for each state over time
all_stackedbar_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter the data for the current state and limit the analysis to years from 2010 onward
  df_state <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010) |>
    mutate(rptyear_fac = factor(rptyear))  # Convert years to a factor for the x-axis

  title <- "Percentage of Prison Population Incarcerated Past Parole Eligibility"

  # Accessibility text describing the chart content
  hc_accessibility_text <- paste0("This chart shows the percentage of people past parole eligibility ",
                                  "and the remaining prison population for the state of ", x,
                                  " from ", min(df_state$rptyear), " to ", max(df_state$rptyear), ". ",
                                  "The bars represent the total prison population, with the portion of people ",
                                  "past their parole eligibility highlighted in a different color.")

  # Create the highchart visualization
  highcharts <- highchart() |>
    hc_xAxis(categories = df_state$rptyear_fac) |>
    hc_yAxis(
      title = list(text = ""),
      max = 100,
      labels = list(format = "{value}%")
    ) |>
    hc_add_series(
      name = "Total Population (Remaining)",
      data = (1 - df_state$proportion) * 100,
      type = "column",
      stacking = "percent") |>
    hc_add_series(
      name = "In Prison Past Parole Eligibility",
      data = df_state$proportion * 100,
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
                           min(df_state$rptyear), "-", max(df_state$rptyear))) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_",
                                   min(df_state$rptyear), "_", max(df_state$rptyear))) |>
    hc_caption(text = ncrp_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})
all_stackedbar_pop_pe_by_year <- setNames(all_stackedbar_pop_pe_by_year, states)
all_stackedbar_pop_pe_by_year$Georgia
rm(states)




# ---------------------------------------------------------------------------- #
# DEMOGRAPHICS
# ---------------------------------------------------------------------------- #

# Get number and proportion of people in prison past their parole eligibility year
# by demographic
current_ped_race     <- fnc_prepare_pe_data(ncrp_yearendpop, race) |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) # exclude states with high missingness
current_ped_sex      <- fnc_prepare_pe_data(ncrp_yearendpop, sex)
current_ped_ageyrend <- fnc_prepare_pe_data(ncrp_yearendpop, ageyrend)

# Generate graph for each state
states <- unique(current_ped_race$state)
all_bar_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_race |>
    filter(state == x) |>
    fnc_create_tooltip(variable_label = "Race and Ethnicity", variable = race) |>
    arrange(desc(prop))

  title <- "People in Prison Past Parole Eligibility"

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their race and ethnicity in ",
                                  select_year, " in the state of ", x, ".")

  highcharts <- fnc_hc_columnchart(df1, "race", "prop", hc_accessibility_text) |>
    hc_colors(c(color4)) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_subtitle(text = paste0(title, ", ", select_year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_race_ethnicity_", select_year)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
all_bar_parole_eligibility_race <- setNames(all_bar_parole_eligibility_race, states)
all_bar_parole_eligibility_race$Georgia

# Generate sentence for each state
all_sentence_parole_eligibility_race <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_race |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)

  sentences <- paste0("In ", select_year, ", ", round(df1$prop, 0),
                      " percent of people in prison past parole eligibility were ",
                      df1$race, ".")

  return(sentences)
})
all_sentence_parole_eligibility_race <- setNames(all_sentence_parole_eligibility_race, states)
all_sentence_parole_eligibility_race$Georgia
rm(states)

# Generate graph for each state
states <- unique(current_ped_sex$state)
all_bar_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_sex |>
    filter(state == x) |>
    fnc_create_tooltip(variable_label = "Sex", variable = sex) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their sex in ",
                                  select_year, " in the state of ", x, ".")

  title <- "People in Prison Past Parole Eligibility"

  highcharts <- fnc_hc_columnchart(df1, "sex", "prop", hc_accessibility_text) |>
    hc_colors(c(color4)) |>
    hc_title(text = "Sex") |>
    hc_subtitle(text = paste0(title, ", ", select_year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_sex_", select_year)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
all_bar_parole_eligibility_sex <- setNames(all_bar_parole_eligibility_sex, states)
all_bar_parole_eligibility_sex$Georgia

# Generate sentence for each state
all_sentence_parole_eligibility_sex <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_sex |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)

  sentences <- paste0("In ", select_year, ", ", round(df1$prop, 0),
                      " percent of people in prison past parole eligibility were ",
                      tolower(df1$sex), ".<br><br>")
  return(sentences)
})
all_sentence_parole_eligibility_sex <- setNames(all_sentence_parole_eligibility_sex, states)
all_sentence_parole_eligibility_sex$Georgia
rm(states)

# Generate graph for each state
states <- unique(current_ped_ageyrend$state)
all_bar_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_ageyrend |>
    filter(state == x) |>
    fnc_create_tooltip(variable_label = "Age", variable = ageyrend) |>
    arrange(desc(ageyrend))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their age in ",
                                  select_year, " in the state of ", x, ".")

  title <- "People in Prison Past Parole Eligibility"

  highcharts <- fnc_hc_columnchart(df1, "ageyrend", "prop", hc_accessibility_text) |>
    hc_colors(c(color4)) |>
    hc_title(text = "Age") |>
    hc_subtitle(text = paste0(title, ", ", select_year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_age_", select_year)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
all_bar_parole_eligibility_ageyrend <- setNames(all_bar_parole_eligibility_ageyrend, states)
all_bar_parole_eligibility_ageyrend$Georgia

# Generate sentence for each state
all_sentence_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_ageyrend |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1) |>
    mutate(ageyrend = gsub("-", " to ", ageyrend))

  sentences <- paste0("In ", select_year, ", ", round(df1$prop, 0),
                      " percent of people in prison past parole eligibility were between the ages of ",
                      df1$ageyrend, " old.")

  return(sentences)
})
all_sentence_parole_eligibility_ageyrend <- setNames(all_sentence_parole_eligibility_ageyrend, states)
all_sentence_parole_eligibility_ageyrend$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# OFFENSE TYPE
# ---------------------------------------------------------------------------- #

# Get number and proportion of people in prison past their parole eligibility year
# by offense
current_ped_fbi_index <- fnc_prepare_pe_data(ncrp_yearendpop, fbi_index)
current_ped_fbi_index <- current_ped_fbi_index |>
  mutate(group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ "Other or Unknown"
  ))

# Generate graph for each state
states <- unique(current_ped_fbi_index$state)
all_bar_ped_fbi_index <- map(.x = states, .f = function(x) {

  df1 <- current_ped_fbi_index |>
    filter(state == x & fbi_index != "Unknown") |>
    fnc_create_tooltip(variable_label = "Offense", variable = fbi_index) |>
    arrange(fbi_index)

  # Accessibility text describing the chart content
  hc_accessibility_text <- paste0(
    "This chart shows the proportion of people in prison past parole eligibility in the state of ", x,
    " categorized by offense type for the year ", select_year, ". ",
    "The bars represent different FBI Index offense types, excluding unknown offenses.")

  title <- "People in Prison Past Parole Eligibility"

  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_colors(c(color4)) |>
    hc_title(text = "Offense Type") |>
    hc_subtitle(text = paste0(title, ", ", select_year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_offense_", select_year)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
all_bar_ped_fbi_index <- setNames(all_bar_ped_fbi_index, states)
all_bar_ped_fbi_index$Georgia

# Get proportion of offenses that were violent and nonviolent
current_ped_offense_group <- current_ped_fbi_index |>
  select(state, fbi_index, group, n) |>
  filter(group == "Violent" | group == "Nonviolent") |>
  group_by(state, group) |>
  summarise(total_offenses = sum(n), .groups = 'drop') |>
  group_by(state) |>
  mutate(prop = total_offenses / sum(total_offenses))

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
                         tolower(df2$fbi_index[1]), " (", round(df2$prop[1], 0), "%) and ",
                         tolower(df2$fbi_index[2]), " (", round(df2$prop[2], 0), "%) offenses.")

  # Combine the sentences
  sentences <- paste0(group_sentence, " ", fbi_sentence)

  return(sentences)
})
all_sentence_parole_eligibility_fbi_index <- setNames(all_sentence_parole_eligibility_fbi_index, states)
all_sentence_parole_eligibility_fbi_index$Georgia
rm(states)







# ---------------------------------------------------------------------------- #
# SENTENCE LENGTH
# ---------------------------------------------------------------------------- #

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data(ncrp_yearendpop, sentlgth)

# Generate graph for each state
states <- unique(current_ped_sentlgth$state)
all_bar_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    fnc_create_tooltip(variable_label = "Sentence Length", variable = sentlgth)

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their sentence length in ",
                                  select_year, " in the state of ", x, ".")

  title <- "People in Prison Past Parole Eligibility"

  highcharts <- fnc_hc_columnchart(df1, "sentlgth", "prop", hc_accessibility_text) |>
    hc_colors(c(color4)) |>
    hc_title(text = "Sentence Length") |>
    hc_subtitle(text = paste0(title, ", ", select_year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_sentence_length_", select_year)) |>
    hc_caption(text = ncrp_source)

  return(highcharts)
})
all_bar_parole_eligibility_sentlgth <- setNames(all_bar_parole_eligibility_sentlgth, states)
all_bar_parole_eligibility_sentlgth$Georgia

# Generate sentence for each state
all_sentence_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {

  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1) |>
    mutate(sentlgth = gsub("-", " to ", sentlgth))

  sentences <- paste0("In ", select_year, ", ", round(df1$prop, 0),
                      " percent of people in prison past parole eligibility had sentence lengths between ",
                      df1$sentlgth, ".")

  return(sentences)
})
all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia
rm(states)





# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

save(all_sentence_pe_type,                         file = file.path(app_folder, "all_sentence_pe_type.rds"))
save(all_pie_pe_type,                              file = file.path(app_folder, "all_pie_pe_type.rds"))

save(all_sentence_pop_pe_by_year,                  file = file.path(app_folder, "all_sentence_pop_pe_by_year.rds"))
save(all_stackedbar_pop_pe_by_year,                file = file.path(app_folder, "all_stackedbar_pop_pe_by_year.rds"))

save(all_sentence_parole_eligibility_race,         file = file.path(app_folder, "all_sentence_parole_eligibility_race.rds"))
save(all_bar_parole_eligibility_race,              file = file.path(app_folder, "all_bar_parole_eligibility_race.rds"))

save(all_sentence_parole_eligibility_sex,          file = file.path(app_folder, "all_sentence_parole_eligibility_sex.rds"))
save(all_bar_parole_eligibility_sex,               file = file.path(app_folder, "all_bar_parole_eligibility_sex.rds"))

save(all_sentence_parole_eligibility_ageyrend,     file = file.path(app_folder, "all_sentence_parole_eligibility_ageyrend.rds"))
save(all_bar_parole_eligibility_ageyrend,          file = file.path(app_folder, "all_bar_parole_eligibility_ageyrend.rds"))

save(all_sentence_parole_eligibility_fbi_index,    file = file.path(app_folder, "all_sentence_parole_eligibility_fbi_index.rds"))
save(all_bar_ped_fbi_index,                        file = file.path(app_folder, "all_bar_ped_fbi_index.rds"))

save(all_sentence_parole_eligibility_sentlgth,     file = file.path(app_folder, "all_sentence_parole_eligibility_sentlgth.rds"))
save(all_bar_parole_eligibility_sentlgth,          file = file.path(app_folder, "all_bar_parole_eligibility_sentlgth.rds"))
