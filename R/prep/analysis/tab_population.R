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
# Prison Population By Year
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
# Only states that submitted data to BJS and not in exclusion
# list (high missingness or abolished parole)
states <- bjs_prison_pop_by_rptyear |>
  filter(!state %in% states_to_exclude$state) |>
  distinct(state) |>
  arrange(state) |>
  pull(state)

# SENTENCE: "From 2010 to 2022, the prison population decreased 17 percent,
#            changing from 56,432 in 2010 to 47,010 in 2022."
# Generate sentence for each state
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
# Assign state names to list
all_sentence_population <- setNames(all_sentence_population, states)
all_sentence_population$Georgia

# VISUALIZATION: Prison Population by Year
# Generate chart for each state
all_line_population_by_year <- map(.x = states,  .f = function(x) {

  df1 <- bjs_prison_pop_by_rptyear |>
    ungroup() |>
    filter(state == x) |>
    distinct() |>
    mutate(tooltip = paste0("Year: ", rptyear, "<br>",
                            "Year-End Population: ", bjs_prison_population))

  # Adds a small margin space at the top
  max_value <- max(df1$bjs_prison_population)*1.1

  hc_accessibility_text <- paste0("This line chart shows the year-end prison population in ",
                                  x, " from ", min(df1$rptyear), " to ",
                                  max(df1$rptyear),
                                  ". Each point on the chart represents the prison population for a specific year, ",
                                  "showing trends over time. The y-axis represents the number of people in prison, ",
                                  "and the x-axis represents the years. ",
                                  "The tooltip provides the exact year and the corresponding prison population.")

  title <- "Prison Population by Year"

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
        name = "population",
        data = df1$bjs_prison_population,
        tooltip = list(
          pointFormat = "<b>Prison Population:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_",
                                   min(df1$rptyear), "_", max(df1$rptyear))) |>
    hc_caption(text = bjs_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)

  return(highcharts)
})
# Assign state names to list
all_line_population_by_year <- setNames(all_line_population_by_year, states)
all_line_population_by_year$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Prison Population By Race
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- bjs_prison_pop_by_race_2020 |>
  filter(!state %in% states_to_exclude$state) |>
  distinct(state) |>
  arrange(state) |>
  pull(state)

# VISUALIZATION: Bar charts for parole eligibility by race for each state
# Generate graph for each state
all_bar_population_race <- map(.x = states,  .f = function(x) {

  this_metric <- "Race and Ethnicity"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = bjs_prison_pop_by_race_2020,
                                   x_var      = "race",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population",
                                   title_type = "People in Prison",
                                   source     = bjs_source)

  return(highcharts)
})
# Assign state names to list
all_bar_population_race <- setNames(all_bar_population_race, states)
all_bar_population_race$Georgia

# SENTENCE: "In 2020, 60 percent of people in prison were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_population_race <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = bjs_prison_pop_by_race_2020,
                                                 x_var      = "race",
                                                 type       = "in prison")

  return(sentences)
})
all_sentence_population_race <- setNames(all_sentence_population_race, states)
all_sentence_population_race$Georgia
rm(states)




# ---------------------------------------------------------------------------- #
# Prison Population By Sex
# ---------------------------------------------------------------------------- #

# Get unique states to iterate over
states <- bjs_prison_pop_by_sex_2022 |>
  filter(!state %in% states_to_exclude$state) |>
  pull(state)

# VISUALIZATION: Prison Population by Sex
# Generate chart for each state
all_bar_population_sex <- map(.x = states,  .f = function(x) {

  this_metric <- "Sex"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = bjs_prison_pop_by_sex_2022,
                                   x_var      = "sex",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population",
                                   title_type = "People in Prison",
                                   source     = bjs_source)

  return(highcharts)
})
# Assign state names to list
all_bar_population_sex <- setNames(all_bar_population_sex, states)
all_bar_population_sex$Georgia

# SENTENCE: "In 2020, 60 percent of people in prison were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_population_sex <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = bjs_prison_pop_by_sex_2022,
                                                 x_var      = "sex",
                                                 type       = "in prison")

  return(sentences)
})
all_sentence_population_sex <- setNames(all_sentence_population_sex, states)
all_sentence_population_sex$Georgia
rm(states)



# ---------------------------------------------------------------------------- #
# Prison Population By Age
# ---------------------------------------------------------------------------- #

# Summarize age data for the prison population for each state
ncrp_population_ageyrend <- fnc_summarize_data(ncrp_yearendpop, "ageyrend")

# Get unique states to iterate over
states <- unique(ncrp_population_ageyrend$state)

# VISUALIZATION: Prison Population by Age
# Generate chart for each state
all_bar_population_ageyrend <- map(.x = states,  .f = function(x) {

  this_metric <- "Age"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = ncrp_population_ageyrend,
                                   x_var      = "ageyrend",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population",
                                   title_type = "People in Prison",
                                   source     = ncrp_source)

  return(highcharts)
})
# Assign state names to list
all_bar_population_ageyrend <- setNames(all_bar_population_ageyrend, states)
all_bar_population_ageyrend$Georgia

# SENTENCE: "In 2020, 60 percent of people in prison were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_population_ageyrend <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = ncrp_population_ageyrend,
                                                 x_var      = "ageyrend",
                                                 type       = "in prison")

  return(sentences)
})
all_sentence_population_ageyrend <- setNames(all_sentence_population_ageyrend, states)
all_sentence_population_ageyrend$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Prison Population By Offense Type
# ---------------------------------------------------------------------------- #

# Summarize offense data for the prison population for each state
ncrp_population_fbi_index <- fnc_summarize_data(ncrp_yearendpop, "fbi_index")

# Get unique states to iterate over
states <- unique(ncrp_population_fbi_index$state)

# VISUALIZATION: Prison Population by Offense
# Generate chart for each state
all_bar_population_fbi_index <- map(.x = states,  .f = function(x) {

  this_metric <- "Offense Type"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = ncrp_population_fbi_index,
                                   x_var      = "fbi_index",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population",
                                   title_type = "People in Prison",
                                   source     = ncrp_source)

  return(highcharts)
})
# Assign state names to list
all_bar_population_fbi_index <- setNames(all_bar_population_fbi_index, states)
all_bar_population_fbi_index$Georgia

# SENTENCE: "In 2020, 60 percent of people in prison were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_population_fbi_index <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = ncrp_population_fbi_index,
                                                 x_var      = "fbi_index",
                                                 type       = "in prison")

  return(sentences)
})
all_sentence_population_fbi_index <- setNames(all_sentence_population_fbi_index, states)
all_sentence_population_fbi_index$Georgia
rm(states)


# ---------------------------------------------------------------------------- #
# Prison Population By Sentence Length
# ---------------------------------------------------------------------------- #

# Process sentence length data for the prison population
ncrp_population_sentlgth <- fnc_summarize_data(ncrp_yearendpop, "sentlgth")

# Get unique states to iterate over
states <- unique(ncrp_population_sentlgth$state)

# VISUALIZATION: Prison Population by Sentence Length
# Generate chart for each state
all_bar_population_sentlgth <- map(.x = states,  .f = function(x) {

  this_metric <- "Sentence Length"
  highcharts <- fnc_hc_columnchart(state_var  = x,
                                   df         = ncrp_population_sentlgth,
                                   x_var      = "sentlgth",
                                   y_var      = "prop",
                                   metric     = this_metric,
                                   type       = "the prison population",
                                   title_type = "People in Prison",
                                   source     = ncrp_source)

  return(highcharts)
})
# Assign state names to list
all_bar_population_sentlgth <- setNames(all_bar_population_sentlgth, states)
all_bar_population_sentlgth$Georgia

# SENTENCE: "In 2020, 60 percent of people in prison were Black, non-Hispanic."
# Generate sentence for each state
all_sentence_population_sentlgth <- map(.x = states,  .f = function(x) {

  sentences <- fnc_generate_columnchart_sentence(state_var  = x,
                                                 df         = ncrp_population_sentlgth,
                                                 x_var      = "sentlgth",
                                                 type       = "in prison")

  return(sentences)
})
all_sentence_population_sentlgth <- setNames(all_sentence_population_sentlgth, states)
all_sentence_population_sentlgth$Georgia
rm(states)



#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_population           = "all_sentence_population.rds",
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

