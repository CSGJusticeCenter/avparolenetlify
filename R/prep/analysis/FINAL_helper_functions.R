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

fnc_filter_by_year <- function(df, which_state_year) {
  df |>
    # Join with `which_state_year` to add `year_to_use`
    left_join(which_state_year, by = "state") |>
    # Filter rows where `rptyear` matches `year_to_use`
    filter(rptyear == year_to_use)
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

fnc_summarize_data <- function(df, count_column) {
  count_column <- sym(count_column)  # Convert the string column name to a symbol

  # Summarize the data and include `rptyear` in the grouping
  df1 <- df |>
    group_by(state, rptyear) |>

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

fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type) {

  df1 <- df |>
    filter(state == state_var) |>
    arrange(-prop)

  year <- unique(df1$rptyear)

  # If there's not enough data, return a missing data message
  if (nrow(df1) < 1 || is.na(df1$prop[1])) {
    return(paste0("Data for ", state_var, " is missing or incomplete."))
  }

  # Check if x_var is "sex" to format to lowercase
  if (x_var == "sex") {
    df1[[x_var]] <- tolower(df1[[x_var]])
  }

  # Special handling for "fbi_index"
  if (x_var == "fbi_index") {
    # Get the top categories based on the highest proportion
    max_prop <- max(round(df1$prop, 0))
    top_categories <- df1 |>
      filter(round(prop, 0) == max_prop) |>
      arrange(desc(prop))

    # Construct sentences for each top category
    fbi_sentences <- top_categories |>
      mutate(fbi_sentence = paste0(tolower(fbi_index), " (", round(prop, 0), " percent)")) |>
      pull(fbi_sentence)

    # Use commas and "and" to format the final sentence correctly
    fbi_sentence_final <- if (length(fbi_sentences) > 1) {
      paste(paste(fbi_sentences[-length(fbi_sentences)], collapse = ", "),
            ", and ", fbi_sentences[length(fbi_sentences)], sep = "")
    } else {
      fbi_sentences
    }

    # Prepare offense group data
    current_ped_offense_group <- df |>
      select(state, fbi_index, offense_group, n) |>
      filter(offense_group == "Violent" | offense_group == "Nonviolent") |>
      group_by(state, offense_group) |>
      summarise(total_offenses = sum(n), .groups = 'drop') |>
      group_by(state) |>
      mutate(prop = total_offenses / sum(total_offenses))

    # Get the proportions for violent and nonviolent offenses
    violent_prop <- current_ped_offense_group |>
      filter(state == state_var, offense_group == "Violent") |>
      pull(prop) |>
      round(2) * 100
    nonviolent_prop <- current_ped_offense_group |>
      filter(state == state_var, offense_group == "Nonviolent") |>
      pull(prop) |>
      round(2) * 100

    # Construct the full sentence
    sentences <- paste0("In ", year, ", ", violent_prop, " percent of people in prison past parole eligibility were in prison for violent offenses and ",
                        nonviolent_prop, " percent for nonviolent offenses. ",
                        "Most people ", type, " were incarcerated for ", fbi_sentence_final, " offenses.")

  } else if (x_var == "ageyrend" | x_var == "agerlse") {
    # Handle age-related variables
    age_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " were between the ages of ",
                        age_range[1], " and ", age_range[2], " old.")
  } else if (x_var == "sentlgth") {
    # Handle sentence length variables
    sent_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " had sentence lengths between ",
                        sent_range[1], " and ", sent_range[2], ".")
  } else {
    # General case for other variables
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type, " were ",
                        df1[[x_var]][1], ".")
  }

  return(sentences)
}

# Helper function for bar chart visualization
fnc_generate_bar_charts <- function(data, x_var, metric, type_desc, title_type, y_var = "prop") {
  states <- unique(data$state)
  charts <- map(states, function(state_name) {
    # Set orientation based on the x_var
    orientation <- if (x_var == "fbi_index") "horizontal" else "vertical"

    fnc_hc_columnchart(
      state_var  = state_name,
      df         = data,
      x_var      = x_var,
      y_var      = y_var,
      metric     = metric,
      type       = type_desc,
      title_type = title_type,
      orientation = orientation
    )
  })
  setNames(charts, states)
}

# Helper function for sentence generation
fnc_generate_sentences <- function(data, x_var, type_desc) {
  states <- unique(data$state)
  sentences <- map(states, function(state_name) {
    fnc_generate_columnchart_sentence(
      state_var = state_name,
      df        = data,
      x_var     = x_var,
      type      = type_desc
    )
  })
  setNames(sentences, states)
}

fnc_group_offense_type <- function(data) {
  data %>%
    mutate(offense_group = case_when(
      fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                       "Negligent Manslaughter",
                       "Rape or Sexual Assault",
                       "Robbery",
                       "Aggravated or Simple Assault",
                       "Other Violent Offenses") ~ "Violent",
      fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
      TRUE ~ "Other or Unknown"
    ))
}
# fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type) {
#
#   df1 <- df |>
#     filter(state == state_var) |>
#     arrange(-prop) |>
#     slice(1)
#
#   year <- unique(df1$rptyear)
#
#   # Modify df1[[x_var]] to lowercase if x_var is "sex"
#   if (x_var == "sex") {
#     df1[[x_var]] <- tolower(df1[[x_var]])
#   }
#
#   # Check if x_var is "ageyrend" to format the sentence differently
#   if (x_var == "ageyrend" | x_var == "agerlse") {
#     age_range <- strsplit(as.character(df1[[x_var]]), "-")[[1]]
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were between the ages of ",
#                         age_range[1], " and ", age_range[2], " old.")
#   } else if (x_var == "fbi_index") {
#     # Lowercase the crime for proper sentence structure
#     crime <- tolower(df1[[x_var]])
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were incarcerated for ",
#                         crime, " offenses.")
#   } else if (x_var == "sentlgth") {
#     # Split the sentence length range
#     sent_range <- strsplit(as.character(df1[[x_var]]), "-")[[1]]
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " had sentence lengths between ",
#                         sent_range[1], " and ", sent_range[2], ".")
#   } else {
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were ",
#                         df1[[x_var]], ".")
#   }
#
#   return(sentences)
# }













fnc_generate_projection_sentence <- function(state_name, data) {
  state_data <- data |> filter(state == state_name)

  # Extract years and values for past and projected data
  valid_past_years <- state_data |> filter(!is.na(pct_past_pe)) |> pull(year)
  valid_proj_years <- state_data |> filter(!is.na(proj_pct_past_pe)) |> pull(year)

  # Determine earliest and latest years for past and projected data
  earliest_year_past <- min(valid_past_years, na.rm = TRUE)
  latest_year_past <- max(valid_past_years, na.rm = TRUE)
  earliest_year_proj <- if (length(valid_proj_years) > 0) min(valid_proj_years, na.rm = TRUE) else NA
  latest_year_proj <- if (length(valid_proj_years) > 0) max(valid_proj_years, na.rm = TRUE) else NA

  # Extract percentage values and calculate changes
  pct_earliest <- state_data |> filter(year == earliest_year_past) |> pull(pct_past_pe)
  pct_latest <- state_data |> filter(year == latest_year_past) |> pull(pct_past_pe)
  change_past <- if (!is.na(pct_earliest) && !is.na(pct_latest)) {
    round(((pct_latest - pct_earliest) / pct_earliest) * 100, 1)
  } else NA

  proj_earliest <- if (!is.na(earliest_year_proj)) state_data |> filter(year == earliest_year_proj) |> pull(proj_pct_past_pe) else NA
  proj_latest <- if (!is.na(latest_year_proj)) state_data |> filter(year == latest_year_proj) |> pull(proj_pct_past_pe) else NA
  change_proj <- if (!is.na(proj_earliest) && !is.na(proj_latest)) {
    round(((proj_latest - proj_earliest) / proj_earliest) * 100, 1)
  } else NA

  # Generate note for projected data usage
  note <- case_when(
    state_data |> filter(year == 2019) |> pull(used_projected_flag) ~ " Note: 2019 data uses projections.",
    state_data |> filter(year == 2020) |> pull(used_projected_flag) ~ " Note: 2020 data uses projections.",
    TRUE ~ ""
  )

  # Construct the sentence with simplified logic
  sentence <- paste0(
    "From ", earliest_year_past, " to ", latest_year_past,
    ", the percent of people in prison past parole eligibility ",
    if (!is.na(change_past)) {
      if (change_past > 0) paste0("increased by ", change_past, " percent. ")
      else if (change_past < 0) paste0("decreased by ", abs(change_past), " percent. ")
      else "remained the same. "
    } else "has insufficient data to determine a change. ",
    if (!is.na(earliest_year_proj) && !is.na(latest_year_proj)) {
      paste0(
        "We've projected that from ", earliest_year_proj, " to ", latest_year_proj,
        ", the percent of people past parole eligibility ",
        if (!is.na(change_proj)) {
          if (change_proj > 0) paste0("will increase by ", change_proj, " percent")
          else if (change_proj < 0) paste0("will decrease by ", abs(change_proj), " percent")
          else "will not change (0 percent change)"
        } else "has insufficient data to project a change",
        "."
      )
    } else "Projected data is insufficient to provide a future change.",
    note
  )

  return(sentence)
}
