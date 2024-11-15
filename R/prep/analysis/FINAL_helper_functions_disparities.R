
# Helper function to filter data by state and year
fnc_filter_data_by_state_year <- function(df, state_var) {
  year <- df |>
    filter(state == state_var) |>
    pull(rptyear) |>
    max(na.rm = TRUE)

  df_filtered <- df |>
    ungroup() |>
    filter(state == state_var) |>
    filter(rptyear == year)

  list(data = df_filtered, year = year)
}

# Helper function to generate sentences based on sex comparisons
fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
  # Focus on comparisons with males
  df_male <- df1 |> dplyr::filter(sex == "Male")

  # Initialize an empty sentence variable
  sentence <- ""

  # Generate sentence for female vs male comparison
  df_female <- df1 |> dplyr::filter(sex == "Female")
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
    abs_los_diff_female <- abs(los_diff_female)

    if (!is.na(los_diff_female)) {
      if (los_diff_female > 0) {
        sentence <- paste0("In ", year, ", females ",
                           if (type == "in prison") "released" else "who were still incarcerated",
                           " spent on average ", abs_los_diff_female,
                           if (abs_los_diff_female == 1) " more year" else " more years",
                           " ", if (type == "in prison") "in prison" else "past parole eligibility",
                           " compared to males in ", state_var, ".")
      } else if (los_diff_female < 0) {
        sentence <- paste0("In ", year, ", females ",
                           if (type == "in prison") "released" else "who were still incarcerated",
                           " spent on average ", abs_los_diff_female,
                           if (abs_los_diff_female == 1) " less year" else " less years",
                           " ", if (type == "in prison") "in prison" else "past parole eligibility",
                           " compared to males in ", state_var, ".")
      }
    }
  }

  if (sentence != "") {
    return(sentence)
  } else {
    return(paste0("Females and males spent the same average number of years ",
                  if (type == "in prison") "in prison." else "past parole eligibility."))
  }
}

# Main function to generate disparity sentences
fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {

  # Get unique states to iterate over
  states <- unique(df$state)

  # Generate sentence for each state
  all_sentences <- purrr::map(.x = states, .f = function(state_var) {

    # Use helper function to filter data by state and year
    filtered_data <- fnc_filter_data_by_state_year(df, state_var)
    df1 <- filtered_data$data
    year <- filtered_data$year

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var))
    }

    # Check for the comparison variable ("sex" or "race")
    if (compare_var == "sex") {
      # Use the helper function to generate the sentence for sex comparison
      return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))

    } else if (compare_var == "race") {

      # Logic for race comparison remains the same
      df1 <- df1 |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic",
          race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
        ))

      # Focus on comparisons with White individuals
      df_white <- df1 |> dplyr::filter(race == "White")

      # Initialize variables to hold sentences for each race comparison
      black_sentence <- ""
      hispanic_sentence <- ""
      other_sentence <- ""

      # Generate sentence for Black vs White comparison
      df_black <- df1 |> dplyr::filter(race == "Black")
      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_black <- round(abs(los_diff_black), 1)

        if (!is.na(los_diff_black)) {
          if (los_diff_black > 0) {
            black_sentence <- paste0("Black people ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_black,
                                     " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_black < 0) {
            black_sentence <- paste0("Black people ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_black,
                                     if (abs_los_diff_black == 1) " less year" else " less years",
                                     " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Generate sentence for Hispanic vs White comparison
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)

        if (!is.na(los_diff_hispanic)) {
          if (los_diff_hispanic > 0) {
            hispanic_sentence <- paste0("Hispanic people ",
                                        if (type == "in prison") "released" else "who were still incarcerated",
                                        " spent on average ", abs_los_diff_hispanic,
                                        " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_hispanic < 0) {
            hispanic_sentence <- paste0("Hispanic people ",
                                        if (type == "in prison") "released" else "who were still incarcerated",
                                        " spent on average ", abs_los_diff_hispanic,
                                        if (abs_los_diff_hispanic == 1) " less year" else " less years",
                                        " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Generate sentence for Other races vs White comparison
      df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
      if (nrow(df_other) > 0 && nrow(df_white) > 0) {
        los_diff_other <- round(df_other[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_other <- round(abs(los_diff_other), 1)

        if (!is.na(los_diff_other)) {
          if (los_diff_other > 0) {
            other_sentence <- paste0("non-Hispanic people of other races ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_other,
                                     " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
          } else if (los_diff_other < 0) {
            other_sentence <- paste0("non-Hispanic people of other races ",
                                     if (type == "in prison") "released" else "who were still incarcerated",
                                     " spent on average ", abs_los_diff_other,
                                     if (abs_los_diff_other == 1) " less year" else " less years",
                                     " ", if (type == "in prison") "in prison" else "past parole eligibility")
          }
        }
      }

      # Combine sentences or indicate no significant differences
      sentences <- c(black_sentence, hispanic_sentence, other_sentence)
      sentences <- sentences[sentences != ""]
      if (length(sentences) > 0) {
        return(paste0("In ", year, ", ", paste(sentences, collapse = ", and "), " compared to White people."))
      } else {
        return("No significant differences in average years spent compared to White people.")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  # Assign state names to list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}
