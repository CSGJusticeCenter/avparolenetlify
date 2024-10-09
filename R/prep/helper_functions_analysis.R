fnc_filter_population <- function(data) {
  # Get states to exclude - missing data and abolished parole
  exclude <- states_to_exclude |>
    pull(state)

  # Filter data based on the admission type, sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(!(state %in% exclude))  # Only keep states that did not abolish parole

  return(filtered_data)
}

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

fnc_prepare_pe_data <- function(df, count_column) {
  df1 <- fnc_filter_pe_population_criteria(df) |>
    # Filter for the selected year and 'Current' parole eligibility status
    filter(rptyear == select_year & parelig_status == "Current") |>
    # Group by state and count occurrences of the specified column
    group_by(state) |>
    filter(!is.na({{ count_column }})) |>
    count({{ count_column }}) |>
    # Calculate proportions and create labels for visualization
    mutate(
      prop = (n/sum(n))*100,                     # Calculate proportion
      yearendpop_ped = sum(n),                   # Calculate total population
      prop_label = paste0(round(prop, 0), "%"),  # Create proportion label as percentage
      n_label = formattable::comma(n, 0)         # Format count labels with commas
    ) |>
    ungroup()

  return(df1)
}

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
