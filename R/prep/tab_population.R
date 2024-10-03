#######################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    Prison population visualizations and findings for population tab
#    Uses BJS Prisoners Data
#######################################

# ---------------------------------------------------------------------------- #
# PRISON POPULATION BY YEAR
# ---------------------------------------------------------------------------- #

# Get unique states from the bjs_prison_pop_by_rptyear dataset to ensure we're working
# with all the states present in the dataset.
states <- unique(bjs_prison_pop_by_rptyear$state)

# Filter out states that have abolished parole (abolished_parole_16_total == "N")
# from the state_notes dataset, retaining only states still practicing parole.
states <- state_notes |>
  filter(abolished_parole == "N", state %in% states) |>
  pull(state)

# Loop through each state and generate a sentence summarizing the change in prison population
# from the earliest available year to the most recent one.
all_sentence_population <- map(.x = states, .f = function(x) {
  # Filter bjs_prison_pop_by_rptyear data for the specific state
  df1 <- bjs_prison_pop_by_rptyear %>% filter(state == x)

  # Identify the earliest year with valid population data
  earliest_year <- min(df1$rptyear)
  earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]

  # Handle cases where the population data for the earliest year is missing
  if(is.na(earliest_year_population) | length(earliest_year_population) == 0) {
    earliest_year <- min(df1$rptyear[!is.na(df1$bjs_prison_population)])
    earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]
  }

  # Identify the most recent year with valid population data
  latest_year <- max(df1$rptyear)
  latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]

  # Handle cases where the population data for the latest year is missing
  if(is.na(latest_year_population) | length(latest_year_population) == 0) {
    latest_year <- max(df1$rptyear[!is.na(df1$bjs_prison_population) & df1$rptyear < latest_year])
    latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]
  }

  # Calculate the percentage change in population from the earliest to latest year
  percent_change <- (latest_year_population - earliest_year_population) / earliest_year_population * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")  # Determine if population increased or decreased
  percent_change_abs <- abs(round(percent_change, 0))

  # Construct a sentence summarizing the population change over the years
  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population ",
                      change_type, " ", percent_change_abs, " percent, changing from ",
                      format(earliest_year_population, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_population, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Name each entry in the list of sentences by state for easy reference
all_sentence_population <- setNames(all_sentence_population, states)
all_sentence_population$Georgia

# Generate a line chart for the prison population by year for each state
all_line_population_by_year <- map(.x = states, .f = function(x) {
  # Filter data for the specific state and prepare for the chart
  df1 <- bjs_prison_pop_by_rptyear |>
    ungroup() |>
    filter(state == x) |>
    distinct() |>
    mutate(tooltip =
             paste0(
               "Year: ", rptyear, "<br>",
               "Year-End Population: ", bjs_prison_population))

  # Determine maximum and minimum values for y-axis (with padding for display purposes)
  max_value <- max(df1$bjs_prison_population)*1.1
  min_value <- min(df1$bjs_prison_population)/1.5

  # Placeholder text for chart accessibility
  hc_accessibility_text <- paste0("TBD")

  # Create the Highcharts line chart
  highcharts <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Population by Year") |>
    hc_yAxis(title = list(text = ""),
             min = min_value,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear, lineWidth = 1) |>
    hc_series(
      list(
        name = "population",
        data = df1$bjs_prison_population,
        tooltip = list(
          pointFormat = "<b>Prison Population:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})

# Name each chart in the list by state
all_line_population_by_year <- setNames(all_line_population_by_year, states)
all_sentence_population$Georgia

all_line_population_by_year <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_rptyear |>
    ungroup() |>
    filter(state == x) |>
    distinct() |>
    mutate(tooltip =
             paste0(
               "Year: ", rptyear, "<br>",
               "Year-End Population: ", bjs_prison_population))

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$bjs_prison_population)*1.1
  min_value <- min(df1$bjs_prison_population)/1.5

  hc_accessibility_text <- paste0("TBD")

  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Population by Year") |>
    hc_yAxis(title = list(text = ""),
             min = min_value,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "population",
        data = df1$bjs_prison_population,
        tooltip = list(
          # pointFormat = "Year: {point.category}<br>Prison Population: {point.y}"
          pointFormat = "<b>Prison Population:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})
all_line_population_by_year <- setNames(all_line_population_by_year, states)
all_line_population_by_year$Georgia
rm(states)



#------------------------------------------------------------------------------#
# RACE AND ETHNICITY DEMOGRAPHICS
#------------------------------------------------------------------------------#

# Get unique states from the bjs_prison_pop_by_race_2022 dataset to process race data
states <- unique(bjs_prison_pop_by_race_2022$state)

# Filter states that still have parole (abolished_parole_16_total == "N")
states <- state_notes |>
  filter(abolished_parole == "N", state %in% states) |>
  pull(state)

# Generate bar charts for each state based on race data for the prison population
all_bar_population_race <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_race_2022 |>
    filter(state == x) |>
    mutate(prop = prop*100,  # Convert proportion to percentage
           tooltip = paste0("<b>Race:</b> ", race, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))  # Sort by proportion in descending order

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by race and ethnicity in 2022 in the state of ", x, ".")

  # Create a Highcharts bar chart for racial demographics
  highcharts <- fnc_hc_columnchart(df1, "race", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,  # Set max value for y-axis to 100% for better readability
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_subtitle(text = "Prison Population, 2022") |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

# Name the charts for easy reference by state
all_bar_population_race <- setNames(all_bar_population_race, states)
all_bar_population_race$Georgia

# Generate sentences summarizing race demographics for each state
all_sentence_population_race <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_race_2022 |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)  # Select the race with the highest proportion

  # Generate a sentence summarizing the racial breakdown for the prison population
  sentences <- paste0("In 2022, ", round(df1$prop*100, 0), " percent of people in prison were ", df1$race, " people.")
  return(sentences)
})

# Name the sentence list for easy reference by state
all_sentence_population_race <- setNames(all_sentence_population_race, states)
all_sentence_population_race$Georgia
rm(states)



#------------------------------------------------------------------------------#
# SEX DEMOGRAPHICS
#------------------------------------------------------------------------------#

# Get unique states from the bjs_prison_pop_by_sex_2022 dataset to process sex data
states <- unique(bjs_prison_pop_by_sex_2022$state)

# Filter states that still have parole
states <- state_notes |>
  filter(abolished_parole == "N", state %in% states) |>
  pull(state)

# Generate bar charts for each state based on sex demographics in the prison population
all_bar_population_sex <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_sex_2022 |>
    filter(state == x) |>
    mutate(prop = prop*100,  # Convert proportion to percentage
           tooltip = paste0("<b>Sex:</b> ", sex, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))  # Sort by proportion in descending order

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by sex in 2022 in the state of ", x, ".")

  # Create a Highcharts bar chart for sex demographics
  highcharts <- fnc_hc_columnchart(df1, "sex", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,  # Set max value for y-axis to 100% for better readability
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>
    hc_title(text = "Sex") |>
    hc_subtitle(text = "Prison Population, 2022") |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

# Name the charts for easy reference by state
all_bar_population_sex <- setNames(all_bar_population_sex, states)
all_bar_population_sex$Georgia

# Generate sentences summarizing sex demographics for each state
all_sentence_population_sex <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_sex_2022 |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)  # Select the sex with the highest proportion

  # Generate a sentence summarizing the sex breakdown for the prison population
  sentences <- paste0("In 2022, ", round(df1$prop*100, 0), " percent of people in prison were ", tolower(df1$sex), "s.")
  return(sentences)
})

# Name the sentence list for easy reference by state
all_sentence_population_sex <- setNames(all_sentence_population_sex, states)
all_sentence_population_sex$Georgia
rm(states)



#------------------------------------------------------------------------------#
# AGE DEMOGRAPHICS
#------------------------------------------------------------------------------#

# Process age data for the prison population for each state
ncrp_population_ageyrend <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(ageyrend) & ageyrend != "Unknown") |>
  count(ageyrend) |>
  mutate(
    prop = n/sum(n),  # Calculate proportion of each age group
    yearendpop_ped = sum(n),  # Total population for the year
    prop_label = paste0(round(prop*100, 0), "%"),  # Create labels for display
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate bar charts for age distribution for each state
states <- unique(ncrp_population_ageyrend$state)
all_bar_population_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_ageyrend |>
    filter(state == x) |>
    mutate(prop = prop*100,  # Convert proportion to percentage
           tooltip = paste0("<b>Age:</b> ", ageyrend, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(ageyrend))  # Sort by age group

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by age in ", select_year, " in the state of ", x, ".")

  # Create a Highcharts bar chart for age distribution
  highcharts <- fnc_hc_columnchart(df1, "ageyrend", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,  # Set max value for y-axis to 100% for better readability
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>
    hc_title(text = paste0("Prison Population by Age, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})

# Name the charts for easy reference by state
all_bar_population_ageyrend <- setNames(all_bar_population_ageyrend, states)
all_bar_population_ageyrend$Georgia

# Generate sentences summarizing age demographics for each state
all_sentence_population_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_ageyrend |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)  # Select the age group with the highest proportion

  df1$ageyrend <- gsub("-", " to ", df1$ageyrend)  # Format age range for readability
  # Generate a sentence summarizing the age breakdown for the prison population
  sentences <- paste0("In 2022, ", round(df1$prop*100, 0), " percent of people in prison were between the ages of ", df1$ageyrend, " old.")
  return(sentences)
})

# Name the sentence list for easy reference by state
all_sentence_population_ageyrend <- setNames(all_sentence_population_ageyrend, states)
all_sentence_population_ageyrend$Georgia
rm(states)



#------------------------------------------------------------------------------#
# OFFENSE TYPES
#------------------------------------------------------------------------------#

# Process offense type data for the prison population
ncrp_population_fbi_index <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(fbi_index) & fbi_index != "Unknown") |>
  count(fbi_index) |>
  mutate(
    prop = n/sum(n),  # Calculate proportion of each offense type
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate bar charts for offense types for each state
states <- unique(ncrp_population_fbi_index$state)
all_bar_population_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_fbi_index |>
    filter(state == x) |>
    mutate(prop = prop*100,  # Convert proportion to percentage
           tooltip = paste0("<b>Offense Type:</b> ", fbi_index, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by offense type in ", select_year, " in the state of ", x, ".")

  # Create a Highcharts bar chart for offense type distribution
  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,  # Set max value for y-axis to 100% for better readability
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>
    hc_title(text = paste0("Prison Population by Offense Type, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})

# Name the charts for easy reference by state
all_bar_population_fbi_index <- setNames(all_bar_population_fbi_index, states)
all_bar_population_fbi_index$Georgia

# Generate sentences summarizing offense types for each state
all_sentence_population_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)  # Select the offense type with the highest proportion

  # Generate a sentence summarizing the offense breakdown for the prison population
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of people in prison were incarcerated for ", tolower(df1$fbi_index), " offenses.")
  return(sentences)
})

# Name the sentence list for easy reference by state
all_sentence_population_fbi_index <- setNames(all_sentence_population_fbi_index, states)
all_sentence_population_fbi_index$Georgia
rm(states)



#------------------------------------------------------------------------------#
# SENTENCE LENGTHS
#------------------------------------------------------------------------------#

# Process sentence length data for the prison population
ncrp_population_sentlgth <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(sentlgth) & sentlgth != "Unknown") |>
  count(sentlgth) |>
  mutate(
    prop = n/sum(n),  # Calculate proportion of each sentence length
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate bar charts for sentence lengths for each state
states <- unique(ncrp_population_sentlgth$state)
all_bar_population_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_sentlgth |>
    filter(state == x) |>
    mutate(prop = prop*100,  # Convert proportion to percentage
           tooltip = paste0("<b>Sentence Length:</b> ", sentlgth, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  # Accessibility description for the chart
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by sentence length in ", select_year, " in the state of ", x, ".")

  # Create a Highcharts bar chart for sentence length distribution
  highcharts <- fnc_hc_columnchart(df1, "sentlgth", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,  # Set max value for y-axis to 100% for better readability
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>
    hc_title(text = paste0("Prison Population by Sentence Length, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})

# Name the charts for easy reference by state
all_bar_population_sentlgth <- setNames(all_bar_population_sentlgth, states)
all_bar_population_sentlgth$Georgia

# Generate sentences summarizing sentence lengths for each state
all_sentence_population_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_population_sentlgth |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)  # Select the sentence length with the highest proportion

  df1$sentlgth <- gsub("-", " to ", df1$sentlgth)  # Format sentence range for readability
  # Generate a sentence summarizing the sentence length breakdown for the prison population
  sentences <- paste0("In ", select_year, ", ", round(df1$prop*100, 0), " percent of people in prison had sentence lengths between ", tolower(df1$sentlgth), ".")
  return(sentences)
})

# Name the sentence list for easy reference by state
all_sentence_population_sentlgth <- setNames(all_sentence_population_sentlgth, states)
all_sentence_population_sentlgth$Georgia
rm(states)

#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

save(all_sentence_population,           file = file.path(app_folder, "all_sentence_population.rds"))
save(all_line_population_by_year,       file = file.path(app_folder, "all_line_population_by_year.rds"))

save(all_sentence_population_race,      file = file.path(app_folder, "all_sentence_population_race.rds"))
save(all_bar_population_race,           file = file.path(app_folder, "all_bar_population_race.rds"))

save(all_sentence_population_sex,       file = file.path(app_folder, "all_sentence_population_sex.rds"))
save(all_bar_population_sex,            file = file.path(app_folder, "all_bar_population_sex.rds"))

save(all_sentence_population_ageyrend,  file = file.path(app_folder, "all_sentence_population_ageyrend.rds"))
save(all_bar_population_ageyrend,       file = file.path(app_folder, "all_bar_population_ageyrend.rds"))

save(all_sentence_population_fbi_index, file = file.path(app_folder, "all_sentence_population_fbi_index.rds"))
save(all_bar_population_fbi_index,      file = file.path(app_folder, "all_bar_population_fbi_index.rds"))

save(all_sentence_population_sentlgth,  file = file.path(app_folder, "all_sentence_population_sentlgth.rds"))
save(all_bar_population_sentlgth,       file = file.path(app_folder, "all_bar_population_sentlgth.rds"))

