fnc_filter_pe_population_criteria <- function(data, exclude, dont_filter) {
  # Get states to exclude - missing data and abolished parole
  exclude <- exclude |>
    pull(state)

  # Get states that don't need to filter admtype and sentlgth
  dont_filter <- dont_filter |>
    pull(state)

  # Filter data based on criteria, applying admtype and sentlgth filters only if state is not in dont_filter
  filtered_data <- data |>
    filter(!(state %in% exclude)) |>
    filter(
      (state %in% dont_filter) | # Skip filtering if in dont_filter
        (admtype == "New court commitment" & sentlgth %in% c("1-1.9 years",
                                                             "2-4.9 years",
                                                             "5-9.9 years",
                                                             "10-24.9 years",
                                                             ">=25 years"))
    )

  # Return the filtered data
  return(filtered_data)
}
