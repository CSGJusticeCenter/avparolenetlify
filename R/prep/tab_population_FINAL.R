#######################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    Prison population visualizations and findings for population tab
#    Uses BJS Prisoners Data
#######################################

#------ Prison Population by Year ------#

# Get states with data
states <- unique(bjs_prison_pop_by_rptyear$state)

# Generate sentence for each state
all_sentence_population <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- bjs_prison_pop_by_rptyear %>% filter(state == x)

  # Find the earliest year and handle missing data
  earliest_year <- min(df1$rptyear)
  earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]

  if(is.na(earliest_year_population) | length(earliest_year_population) == 0) {
    # If earliest year data is missing, find the next available year
    earliest_year <- min(df1$rptyear[!is.na(df1$bjs_prison_population)])
    earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]
  }

  # Find the latest year and handle missing data
  latest_year <- max(df1$rptyear)
  latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]

  if(is.na(latest_year_population) | length(latest_year_population) == 0) {
    # If latest year data is missing, find the most recent available year
    latest_year <- max(df1$rptyear[!is.na(df1$bjs_prison_population) & df1$rptyear < latest_year])
    latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]
  }

  # Calculate the percent change
  percent_change <- (latest_year_population - earliest_year_population) / earliest_year_population * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population ",
                      change_type, " ", percent_change_abs, "%, changing from ",
                      format(earliest_year_population, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_population, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Set names for the list elements
all_sentence_population <- setNames(all_sentence_population, states)

# Check the sentence for Georgia
all_sentence_population$Georgia


# Generate graph for each state
states <- unique(bjs_prison_pop_by_rptyear$state)
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







#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

save(all_sentence_population,      file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_population.rds"))
save(all_line_population_by_year, file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_line_population_by_year.rds"))




# # Create a dataframe with our filtered criteria
# # Only interested in people in prison for new court commitments and
# # with sentence lengths between 1-25 years
# ncrp_yearendpop_filtered <- filter_population_criteria(ncrp_yearendpop)
#
# # Generate graph for each state
# states <- unique(bjs_prison_pop_by_rptyear$state)
# all_line_population_by_year <- map(.x = states,  .f = function(x) {
#
#   df1 <- bjs_prison_pop_by_rptyear |>
#     ungroup() |>
#     filter(state == x) |>
#     distinct() |>
#     mutate(tooltip =
#              paste0(
#                "Year: ", rptyear, "<br>",
#                "Year-End Population: ", bjs_prison_population))
#
#   df2 <- ncrp_yearendpop_filtered |>
#     filter(parelig_status == "Current") |>
#     group_by(rptyear, state) |>
#     summarise(n = n())
#
#   # Determine the maximum value for the y-axis in the visualization
#   # Adds a small margin space at the top
#   max_value <- max(df1$bjs_prison_population)*1.1
#   min_value <- min(df1$bjs_prison_population)/1.5
#
#   hc_accessibility_text <- paste0("TBD")
#
#   highcharts <- # Create the line chart
#     hc <- highchart() |>
#     hc_chart(type = "line") |>
#     hc_title(text = "Prison Population by Year") |>
#     hc_yAxis(title = list(text = ""),
#              min = min_value,
#              max = max_value) |>
#     hc_xAxis(categories = df1$rptyear,
#              lineWidth = 1) |>
#     hc_series(
#       list(
#         name = "population",
#         data = df1$bjs_prison_population,
#         tooltip = list(
#           # pointFormat = "Year: {point.category}<br>Prison Population: {point.y}"
#           pointFormat = "<b>Prison Population:</b> {point.y}"
#         )
#       )
#     ) |>
#     hc_add_theme(hc_theme_with_line) |>
#     hc_legend(enabled = FALSE) |>
#     hc_exporting(enabled = TRUE) |>
#     hc_colors(c(color2))
#
#   return(highcharts)
# })
# all_line_population_by_year <- setNames(all_line_population_by_year, states)
# all_line_population_by_year$Georgia
# ---------------------------------------------------------------------------- #
# PE Prison Population by Demographics, Offense Type, Sentence Length
# ---------------------------------------------------------------------------- #

# state_pe_race      <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, race)
# state_pe_sex       <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, sex)
# state_pe_ageyrend  <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, ageyrend)
# state_pe_sentlgth  <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, sentlgth)
# state_pe_fbi_index <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, fbi_index)
#
# # Example of calling the function for race
# all_stacked_bar_pe_race <- map(.x = states, .f = function(x) {
#   state_data <- state_pe_race |> filter(state == x)
#   fnc_hc_stackedbar_pe_population(
#     df = state_data,
#     count_column = race,
#     title = "Race and Ethnicity",
#     subtitle = "Prison Population by Parole Eligibility Status",
#     categories_col = "race",
#     colors = c(darkgray, color2, color4))
# })
# all_stacked_bar_pe_race <- setNames(all_stacked_bar_pe_race, states)
# all_stacked_bar_pe_race$Georgia
#
# # Example of calling the function for sex
# all_stacked_bar_pe_sex <- map(.x = states, .f = function(x) {
#   state_data <- state_pe_sex |> filter(state == x)
#   fnc_hc_stackedbar_pe_population(df = state_data,
#                                   count_column = sex,
#                                   title = "Sex",
#                                   subtitle = "Prison Population by Parole Eligibility Status",
#                                   categories_col = "sex",
#                                   colors = c(darkgray, color2, color4))
# })
# all_stacked_bar_pe_sex <- setNames(all_stacked_bar_pe_sex, states)
# all_stacked_bar_pe_sex$Georgia
#
#
# # Example of calling the function for age
# all_stacked_bar_pe_ageyrend <- map(.x = states, .f = function(x) {
#   state_data <- state_pe_ageyrend |> filter(state == x)
#   fnc_hc_stackedbar_pe_population(df = state_data,
#                                   count_column = ageyrend,
#                                   title = "Age",
#                                   subtitle = "Prison Population by Parole Eligibility Status",
#                                   categories_col = "ageyrend",
#                                   colors = c(darkgray, color2, color4))
# })
# all_stacked_bar_pe_ageyrend <- setNames(all_stacked_bar_pe_ageyrend, states)
# all_stacked_bar_pe_ageyrend$Georgia
#
# # Example of calling the function for sentence length
# all_stacked_bar_pe_sentlgth <- map(.x = states, .f = function(x) {
#   state_data <- state_pe_sentlgth |> filter(state == x)
#   fnc_hc_stackedbar_pe_population(df = state_data,
#                                   count_column = sentlgth,
#                                   title = "Sentence Length",
#                                   subtitle = "Prison Population by Parole Eligibility Status",
#                                   categories_col = "sentlgth",
#                                   colors = c(darkgray, color2, color4))
# })
# all_stacked_bar_pe_sentlgth <- setNames(all_stacked_bar_pe_sentlgth, states)
# all_stacked_bar_pe_sentlgth$Georgia
#
# # Example of calling the function for offense type
# all_stacked_bar_pe_fbi_index <- map(.x = states, .f = function(x) {
#   state_data <- state_pe_fbi_index |> filter(state == x)
#   state_data$fbi_index <- factor(state_data$fbi_index, levels = rev(levels(state_data$fbi_index)))
#   fnc_hc_stackedbar_pe_population(
#     df = state_data,
#     count_column = fbi_index,
#     title = "Offense Type",
#     subtitle = "Prison Population by Parole Eligibility Status",
#     categories_col = "fbi_index",
#     colors = c(darkgray, color2, color4))
# })
# all_stacked_bar_pe_fbi_index <- setNames(all_stacked_bar_pe_fbi_index, states)
# all_stacked_bar_pe_fbi_index$Georgia


