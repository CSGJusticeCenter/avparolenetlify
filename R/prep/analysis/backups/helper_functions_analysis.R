#' Filter Population Data
#'
#' Filters the input data by excluding states with missing data and states that have abolished parole.
#'
#' @param data A data frame containing population data to filter.
#'
#' @return A filtered data frame with states that did not abolish parole and have valid data.
#' @export
fnc_filter_population <- function(data) {
  # Get states to exclude - missing data and abolished parole
  exclude <- states_to_exclude |>
    pull(state)

  # Filter data based on the admission type, sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(!(state %in% exclude))  # Only keep states that did not abolish parole

  return(filtered_data)
}


#' Filter Parole Eligibility Population
#'
#' Filters the input data based on specific parole eligibility criteria such as admission type,
#' sentence length, and exclusion of states with missing data or abolished parole.
#'
#' @param data A data frame containing population data to filter.
#'
#' @return A filtered data frame based on parole eligibility criteria.
#' @export
fnc_filter_pe_population_criteria <- function(data) {
  # Get states to exclude - missing data and abolished parole
  exclude <- states_to_exclude |>
    pull(state)

  # Filter data based on the admission type, valid sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(admtype == "New court commitment") |>
    filter(sentlgth %in% c("1-1.9 years",
                           "2-4.9 years",
                           "5-9.9 years",
                           "10-24.9 years",
                           ">=25 years")) |>
    filter(!(state %in% exclude))

  # Return the filtered data
  return(filtered_data)
}


#' Create Tooltip for Highcharts
#'
#' Creates a tooltip for a Highchart object by formatting the variable label,
#' number of people, and the percentage of people in the population.
#'
#' @param df A data frame containing the data to be used for tooltips.
#' @param variable_label A string representing the label of the variable to be displayed in the tooltip.
#' @param variable A column in the data frame representing the variable used in the tooltip.
#'
#' @return A data frame with an added tooltip column.
#' @export
fnc_create_tooltip <- function(df, variable_label, variable) {
  df |>
    dplyr::mutate(
      tooltip = paste0(
        "<b>", variable_label, ":</b> ", {{ variable }}, "<br>",
        "<b>People:</b> ", formattable::comma(n, 0), "<br>",
        "<b>Percentage of People:</b> ", round(prop, 0), "%"
      )
    )
}


#' Filter Data by Excluding States with High Missing Race Data
#'
#' Filters the input data by excluding states that have high levels of missing race data.
#'
#' @param data A data frame containing population data.
#' @param states_with_high_missing_race A list or vector of states to exclude due to high missing race data.
#'
#' @return A filtered data frame with states with high missing race data excluded.
#' @export
fnc_filter_exclude_high_missing_race <- function(data, states_with_high_missing_race) {
  # Convert to character vector if it's a list
  if (is.list(states_with_high_missing_race)) {
    states_with_high_missing_race <- unlist(states_with_high_missing_race)
  }

  # Debugging step: Print the list of states to be excluded
  print("States with high missing race data:")
  print(states_with_high_missing_race)

  # Ensure both 'state' in data and 'states_with_high_missing_race' are in the same format
  filtered_data <- data |>
    filter(!(state %in% states_with_high_missing_race))

  # Return the filtered data
  return(filtered_data)
}


#' Summarize Data by Count and Proportion
#'
#' Summarizes the input data by counting the occurrences of a specified column, calculating proportions,
#' and formatting the labels for use in visualizations.
#'
#' @param df A data frame containing the data to summarize.
#' @param count_column The column to count occurrences for summarization.
#' @param year The year to filter data by (default: select_year).
#'
#' @return A summarized data frame with counts, proportions, and formatted labels.
#' @export
fnc_summarize_data <- function(df, count_column, year = select_year) {
  count_column <- sym(count_column)  # Convert the string column name to a symbol

  df1 <- df |>
    filter(rptyear == year) |>  # Ensure 'rptyear' exists or use the correct year column
    group_by(state) |>

    # Conditionally exclude "Unknown" only if the count_column is not "race"
    filter(!is.na(!!count_column) &
             (!(deparse(substitute(count_column)) != "race" & (!!count_column == "Unknown")))) |>

    count(!!count_column) |>

    # Calculate proportions and create labels for visualization
    mutate(
      prop = (n / sum(n)) * 100,                # Calculate proportion
      n_total = sum(n),                         # Calculate total population
      prop_label = paste0(round(prop, 0), "%"), # Create proportion label as percentage
      n_label = formattable::comma(n, 0)        # Format count labels with commas
    ) |>
    ungroup()

  return(df1)
}


#' Round Numbers to Nearest Power of 10
#'
#' Rounds numbers to the nearest power of 10 based on the number of digits.
#' If a number has 3 or more digits, it rounds down to the nearest power of 10 below the number.
#' For numbers with fewer than 3 digits, it rounds to the nearest 10.
#'
#' @param x A numeric vector of values to round.
#'
#' @return A numeric vector with values rounded to the nearest power of 10.
#' @export
fnc_round_to_power <- function(x) {
  sapply(x, function(val) {
    # Check if the value is NA, and return NA if true
    if (is.na(val)) {
      return(NA)
    }

    # Determine the number of digits in the number
    digits <- nchar(floor(val))

    # Define the rounding level: if digits >= 3, round to the next power of 10 down, else round to 10
    if (digits >= 3) {
      power <- 10^(digits - 2) # This rounds down to the next power of 10 below the number
      floor(val / power) * power  # Use floor to always round down
    } else {
      round(val, -1)
    }
  })
}
