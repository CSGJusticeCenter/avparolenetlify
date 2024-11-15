################################################################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    Prison population visualizations and findings for population tab
#    Uses BJS Prisoners Data
################################################################################

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
all_line_population_by_year <- setNames(all_line_population_by_year, states)
all_line_population_by_year$Georgia
all_line_population_by_year$Hawaii
rm(states)

# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

current_yearendpop <- ncrp_yearendpop_consolidated |>
  fnc_filter_by_year(which_overall_year)

current_yearendpop_not_consolidated <- ncrp_yearendpop_not_consolidated |>
  fnc_filter_by_year(which_overall_year)

bjs_population_race       <- bjs_prison_pop_by_race_2019 |> mutate(rptyear = 2019)
bjs_population_sex        <- bjs_prison_pop_by_sex_2019 |> mutate(rptyear = 2019)
ncrp_population_ageyrend  <- fnc_summarize_data(current_yearendpop_not_consolidated, "ageyrend")
ncrp_population_fbi_index <- fnc_summarize_data(current_yearendpop, "fbi_index") |>
  fnc_group_offense_type()
ncrp_population_sentlgth  <- fnc_summarize_data(current_yearendpop, "sentlgth")

# List of parameters for each category
categories <- list(
  list(data = bjs_population_race, x_var = "race", metric = "Race and Ethnicity"),
  list(data = bjs_population_sex, x_var = "sex", metric = "Sex"),
  list(data = ncrp_population_ageyrend, x_var = "ageyrend", metric = "Age"),
  list(data = ncrp_population_sentlgth, x_var = "sentlgth", metric = "Sentence Length"),
  list(data = ncrp_population_fbi_index, x_var = "fbi_index", metric = "Offense Type")
)

# ---------------------------------------------------------------------------- #
# Generate Sentences and Column Charts (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Initialize empty lists to store bar charts and sentences
all_bar_population <- list()
all_sentence_population <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  all_bar_population[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,
    x_var      = category$x_var,
    metric     = category$metric,
    type_desc  = "the prison population",
    title_type = "People in Prison",
    y_var      = "prop"
  )

  all_sentence_population[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type_desc = "in prison"
  )
}

# Access specific bar charts and sentences
all_bar_population_race <- all_bar_population[["race"]]
all_sentence_population_race <- all_sentence_population[["race"]]
all_bar_population_sex <- all_bar_population[["sex"]]
all_sentence_population_sex <- all_sentence_population[["sex"]]
all_bar_population_ageyrend <- all_bar_population[["ageyrend"]]
all_sentence_population_ageyrend <- all_sentence_population[["ageyrend"]]
all_bar_population_sentlgth <- all_bar_population[["sentlgth"]]
all_sentence_population_sentlgth <- all_sentence_population[["sentlgth"]]
all_bar_population_fbi_index <- all_bar_population[["fbi_index"]]
all_sentence_population_fbi_index <- all_sentence_population[["fbi_index"]]



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
