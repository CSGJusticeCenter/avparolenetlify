#' Generate Disparity Sentences
#'
#' This function generates disparity sentences comparing average time served or
#' years past parole eligibility by race or sex for a given dataset. The function
#' calculates differences between groups and constructs descriptive sentences to
#' summarize these disparities.
#'
#' @param df A data frame containing the dataset with necessary variables.
#' @param type A string indicating the type of analysis: either `"in prison"`
#'   (for time served) or `"past parole eligibility"`.
#' @param compare_var A string specifying the comparison variable: `"race"` or
#'   `"sex"`.
#' @param los_col A string specifying the column name for the length of stay
#'   (LOS) or years past parole eligibility.
#' @return A named list of sentences, with state names as keys and the corresponding
#'   sentences as values.
#' @examples
#' disparity_sentences <- fnc_generate_disparity_sentences(df, "in prison", "race", "average_los")
#' disparity_sentences$Georgia
#' @export
# fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#
#   # Extract unique states for iteration
#   states <- unique(df$state)
#
#   # Generate sentences for each state
#   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#
#     # Use helper function to filter data by state and year
#     filtered_data <- fnc_filter_data_by_state_year(df, state_var)
#     df1 <- filtered_data$data
#     year <- filtered_data$year
#
#     # Handle missing data for the state
#     if (nrow(df1) == 0) {
#       return(paste0("No data available for ", state_var))
#     }
#
#     # --- Handle Sex Comparison ---
#     if (compare_var == "sex") {
#       # Generate sentence using helper function for sex comparisons
#       return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
#
#     } else if (compare_var == "race") {
#
#       # --- Handle Race Comparison ---
#       # Standardize race categories for consistency
#       df1 <- df1 |>
#         dplyr::mutate(race = dplyr::case_when(
#           race == "White, non-Hispanic" ~ "White",
#           race == "Black, non-Hispanic" ~ "Black",
#           race == "Hispanic, any race" ~ "Hispanic",
#           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#         ))
#
#       # Extract data for White individuals as the comparison group
#       df_white <- df1 |> dplyr::filter(race == "White")
#
#       # Initialize sentences for each comparison group
#       black_sentence <- ""
#       hispanic_sentence <- ""
#       other_sentence <- ""
#
#       # --- Black vs White Comparison ---
#       df_black <- df1 |> dplyr::filter(race == "Black")
#       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#         los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_black <- abs(round(los_diff_black, 1))
#
#         if (!is.na(los_diff_black)) {
#           black_sentence <- if (los_diff_black > 0) {
#             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_black, " more years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           } else {
#             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_black, " less years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # --- Hispanic vs White Comparison ---
#       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#         los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_hispanic <- abs(round(los_diff_hispanic, 1))
#
#         if (!is.na(los_diff_hispanic)) {
#           hispanic_sentence <- if (los_diff_hispanic > 0) {
#             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_hispanic, " more years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           } else {
#             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_hispanic, " less years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # --- Other Races vs White Comparison ---
#       df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
#       if (nrow(df_other) > 0 && nrow(df_white) > 0) {
#         los_diff_other <- round(df_other[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_other <- abs(round(los_diff_other, 1))
#
#         if (!is.na(los_diff_other)) {
#           other_sentence <- if (los_diff_other > 0) {
#             paste0("non-Hispanic people of other races ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_other, " more years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           } else {
#             paste0("non-Hispanic people of other races ", if (type == "in prison") "released" else "still incarcerated",
#                    " spent on average ", abs_los_diff_other, " less years ",
#                    if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # Combine sentences into a single statement
#       sentences <- c(black_sentence, hispanic_sentence, other_sentence)
#       sentences <- sentences[sentences != ""]
#       if (length(sentences) > 0) {
#         # return(paste0("In ", year, ", ", paste(sentences, collapse = ", and "), " compared to White people."))
#         return(paste0(paste(sentences, collapse = ", and "), " compared to White people."))
#       } else {
#         return("No significant differences in average years spent compared to White people.")
#       }
#
#     } else {
#       # Handle invalid comparison variable input
#       return("Invalid comparison variable.")
#     }
#   })
#
#   # Assign state names to the list of generated sentences
#   all_sentences <- setNames(all_sentences, states)
#
#   return(all_sentences)
# }
fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {

  # Extract unique states for iteration
  states <- unique(df$state)

  # Generate sentences for each state
  all_sentences <- purrr::map(.x = states, .f = function(state_var) {

    # Use helper function to filter data by state and year
    filtered_data <- fnc_filter_data_by_state_year(df, state_var)
    df1 <- filtered_data$data
    year <- filtered_data$year

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var))
    }

    if (compare_var == "sex") {
      # Generate sentence using helper function for sex comparisons
      return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))

    } else if (compare_var == "race") {
      # Standardize race categories for consistency
      df1 <- df1 |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic",
          race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
        ))

      # Extract data for White individuals as the comparison group
      df_white <- df1 |> dplyr::filter(race == "White")

      # Initialize sentences for each comparison group
      black_sentence <- ""
      hispanic_sentence <- ""
      other_sentence <- ""

      # --- Black vs White Comparison ---
      df_black <- df1 |> dplyr::filter(race == "Black")
      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
        abs_los_diff_black <- abs(los_diff_black)

        # Convert to months if difference is less than 1 year
        if (!is.na(los_diff_black)) {
          black_sentence <- if (los_diff_black > 0) {
            time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
            time_unit <- if (abs_los_diff_black < 1) "months" else "years"
            paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
                   " spent on average ", time_value, " more ", time_unit,
                   if (type == "in prison") " in prison" else " past parole eligibility")
          } else {
            time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
            time_unit <- if (abs_los_diff_black < 1) "months" else "years"
            paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
                   " spent on average ", time_value, " less ", time_unit,
                   if (type == "in prison") " in prison" else " past parole eligibility")
          }
        }
      }

      # --- Hispanic vs White Comparison ---
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
        abs_los_diff_hispanic <- abs(los_diff_hispanic)

        # Convert to months if difference is less than 1 year
        if (!is.na(los_diff_hispanic)) {
          hispanic_sentence <- if (los_diff_hispanic > 0) {
            time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
            time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
            paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
                   " spent on average ", time_value, " more ", time_unit,
                   if (type == "in prison") " in prison" else " past parole eligibility")
          } else {
            time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
            time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
            paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
                   " spent on average ", time_value, " less ", time_unit,
                   if (type == "in prison") " in prison" else " past parole eligibility")
          }
        }
      }

      # Combine sentences into a single statement
      sentences <- c(black_sentence, hispanic_sentence, other_sentence)
      sentences <- sentences[sentences != ""]
      if (length(sentences) > 0) {
        return(paste0(paste(sentences, collapse = ", and "), " compared to White people."))
      } else {
        return("No significant differences in average years spent compared to White people.")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  # Assign state names to the list of generated sentences
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}

#' Generate Disparity Sentence for Sex Comparison
#'
#' This function generates a sentence comparing the average years spent
#' (either in prison or past parole eligibility) between females and males
#' for a given state and year.
#'
#' @param df1 A filtered data frame containing `sex` and the specified column
#'   (`los_col`) for length of stay (LOS) comparisons.
#' @param year An integer representing the reporting year for the comparison.
#' @param type A string indicating the context of the comparison:
#'   `"in prison"` or `"past parole eligibility"`.
#' @param los_col A string specifying the column name in `df1` that contains
#'   the average length of stay data.
#' @param state_var A string representing the name of the state for the analysis.
#' @return A string summarizing the disparity in average years spent between
#'   females and males for the specified state and year.
#' @examples
#' sentence <- fnc_generate_sentence_sex(filtered_data, 2022, "in prison", "average_los", "Georgia")
#' print(sentence)
#' @export
# fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
#   # Filter the data for males
#   df_male <- df1 |> dplyr::filter(sex == "Male")
#
#   # Initialize an empty sentence variable
#   sentence <- ""
#
#   # Filter the data for females
#   df_female <- df1 |> dplyr::filter(sex == "Female")
#
#   # Check if both male and female data exist
#   if (nrow(df_female) > 0 && nrow(df_male) > 0) {
#     # Calculate the difference in length of stay (LOS) between females and males
#     los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
#     abs_los_diff_female <- abs(los_diff_female)
#
#     # Ensure the LOS difference is not NA
#     if (!is.na(los_diff_female)) {
#       if (los_diff_female > 0) {
#         # Females spent more years on average
#         sentence <- paste0(
#           # "In ", year, ", females ",
#           "Females ",
#           if (type == "in prison") "released" else "who were still incarcerated",
#           " spent on average ", abs_los_diff_female,
#           if (abs_los_diff_female == 1) " more year" else " more years",
#           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#           " compared to males in ", state_var, "."
#         )
#       } else if (los_diff_female < 0) {
#         # Females spent fewer years on average
#         sentence <- paste0(
#           # "In ", year, ", females ",
#           "Females ",
#           if (type == "in prison") "released" else "who were still incarcerated",
#           " spent on average ", abs_los_diff_female,
#           if (abs_los_diff_female == 1) " less year" else " less years",
#           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#           " compared to males in ", state_var, "."
#         )
#       }
#     }
#   }
#
#   # Handle cases where no meaningful disparity exists or data is missing
#   if (sentence != "") {
#     return(sentence)  # Return the constructed sentence if disparity is found
#   } else {
#     return(paste0(
#       # "In ", year, ", females and males spent the same average number of years ",
#       "Females and males spent the same average number of years ",
#       if (type == "in prison") "in prison." else "past parole eligibility."
#     ))
#   }
# }
fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
  # Filter the data for males
  df_male <- df1 |> dplyr::filter(sex == "Male")

  # Initialize an empty sentence variable
  sentence <- ""

  # Filter the data for females
  df_female <- df1 |> dplyr::filter(sex == "Female")

  # Check if both male and female data exist
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    # Calculate the difference in length of stay (LOS) between females and males
    los_diff_female <- df_female[[los_col]] - df_male[[los_col]]
    abs_los_diff_female <- abs(los_diff_female)

    # Ensure the LOS difference is not NA
    if (!is.na(los_diff_female)) {
      if (los_diff_female > 0) {
        # Females spent more time on average
        time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
        time_unit <- if (abs_los_diff_female < 1) "months" else "years"
        sentence <- paste0(
          "Females ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", time_value, " more ", time_unit,
          " ", if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to males in ", state_var, "."
        )
      } else if (los_diff_female < 0) {
        # Females spent less time on average
        time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
        time_unit <- if (abs_los_diff_female < 1) "months" else "years"
        sentence <- paste0(
          "Females ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", time_value, " less ", time_unit,
          " ", if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to males in ", state_var, "."
        )
      }
    }
  }

  # Handle cases where no meaningful disparity exists or data is missing
  if (sentence != "") {
    return(sentence)  # Return the constructed sentence if disparity is found
  } else {
    return(paste0(
      "Females and males spent the same average number of years ",
      if (type == "in prison") "in prison." else "past parole eligibility."
    ))
  }
}


#' Generate Offense-Specific Disparity Sentences
#'
#' This function analyzes disparities in average time served or time spent past parole eligibility
#' for each offense type by grouping variable (e.g., race or sex) within states and generates
#' descriptive sentences highlighting the largest disparities.
#'
#' @param data A data frame containing information on offense types, grouping variables
#'   (e.g., race or sex), and time measures (e.g., average time served).
#' @param grouping_var A string specifying the grouping variable, either `"race"` or `"sex"`.
#' @param time_var A string specifying the measure variable, such as `"average_los"` (average time served).
#' @return A named list of descriptive disparity sentences, with each element corresponding to a state.
#' @export
# fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los") {
#
#   # Extract unique states to iterate over
#   states <- unique(data$state)
#
#   # Generate sentences for each state
#   all_sentences <- purrr::map(.x = states, .f = function(x) {
#
#     # Filter data for the specified state and exclude unspecified offense types
#     df1 <- data |>
#       dplyr::filter(state == x & fbi_index != "Other or Unspecified")
#
#     # Extract the year for this state's data
#     year <- unique(df1$rptyear)
#
#     # Handle missing data: If no data exists for the state, return a message
#     if (nrow(df1) == 0) {
#       return(paste0("No data available for ", x))
#     }
#
#     # Calculate disparities between groups for each offense type
#     df_disparity <- df1 |>
#       dplyr::group_by(fbi_index) |>
#       dplyr::reframe(
#         max_los = max(!!rlang::sym(time_var)),               # Maximum value of time_var
#         min_los = min(!!rlang::sym(time_var)),               # Minimum value of time_var
#         diff_los = max_los - min_los,                        # Difference between max and min
#         group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],  # Group with max value
#         group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]  # Group with min value
#       ) |>
#       dplyr::arrange(dplyr::desc(diff_los))                 # Sort by largest disparities
#
#     # Standardize group labels for race or sex
#     if (grouping_var == "race") {
#       df_disparity <- df_disparity |>
#         dplyr::mutate(
#           group_longest = dplyr::case_when(
#             group_longest == "Black, non-Hispanic" ~ "Black",
#             group_longest == "White, non-Hispanic" ~ "White",
#             group_longest == "Hispanic, any race" ~ "Hispanic",
#             group_longest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#             TRUE ~ group_longest
#           ),
#           group_shortest = dplyr::case_when(
#             group_shortest == "Black, non-Hispanic" ~ "Black",
#             group_shortest == "White, non-Hispanic" ~ "White",
#             group_shortest == "Hispanic, any race" ~ "Hispanic",
#             group_shortest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#             TRUE ~ group_shortest
#           )
#         )
#     }
#
#     # Focus on relevant comparisons: e.g., Black, Hispanic vs. White (for race) or Male vs. Female (for sex)
#     if (grouping_var == "race") {
#       df_disparity_filtered <- df_disparity |>
#         dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic", "non-Hispanic people of other races"))
#     } else {
#       df_disparity_filtered <- df_disparity |>
#         dplyr::filter(group_shortest == "Female" & group_longest == "Male") |>
#         dplyr::mutate(
#           group_longest = "males",
#           group_shortest = "females"
#         )
#     }
#
#     # Handle cases with no significant disparities
#     if (nrow(df_disparity_filtered) == 0) {
#       time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
#       return(paste0("The chart below shows the average ", time_description, " by offense type and ",
#                     ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."))
#     }
#
#     # Exclude "Other Violent Offenses" if it's the largest disparity (and there are other offenses)
#     if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
#       df_disparity_filtered <- df_disparity_filtered |> dplyr::slice(2)
#     }
#
#     # Extract details for the largest disparity
#     largest_disparity <- df_disparity_filtered |> dplyr::slice(1)
#     offense_type <- largest_disparity$fbi_index
#     group_longest <- largest_disparity$group_longest
#     disparity_diff <- round(largest_disparity$diff_los, 1)
#     group_shortest <- largest_disparity$group_shortest
#
#     # Construct the descriptive sentence
#     time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
#     sentence <- paste0(
#       "The chart below shows the average ", time_description, " by offense type and ",
#       ifelse(grouping_var == "race", "race and ethnicity", grouping_var), ". ",
#       "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
#       group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
#       " spent on average ", disparity_diff, " more years in prison compared to ",
#       group_shortest, if (grouping_var == "race") " people" else "", "."
#     )
#
#     return(sentence)
#   })
#
#   # Assign state names to the resulting list
#   all_sentences <- setNames(all_sentences, states)
#
#   return(all_sentences)
# }
fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los") {

  # Extract unique states to iterate over
  states <- unique(data$state)

  # Generate sentences for each state
  all_sentences <- purrr::map(.x = states, .f = function(x) {

    # Filter data for the specified state and exclude unspecified offense types
    df1 <- data |>
      dplyr::filter(state == x & fbi_index != "Other or Unspecified")

    # Extract the year for this state's data
    year <- unique(df1$rptyear)

    # Handle missing data: If no data exists for the state, return a message
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", x))
    }

    # Calculate disparities between groups for each offense type
    df_disparity <- df1 |>
      dplyr::group_by(fbi_index) |>
      dplyr::reframe(
        max_los = max(!!rlang::sym(time_var)),               # Maximum value of time_var
        min_los = min(!!rlang::sym(time_var)),               # Minimum value of time_var
        diff_los = max_los - min_los,                        # Difference between max and min
        group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],  # Group with max value
        group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]  # Group with min value
      ) |>
      dplyr::arrange(dplyr::desc(diff_los))                 # Sort by largest disparities

    # Standardize group labels for race or sex
    if (grouping_var == "race") {
      df_disparity <- df_disparity |>
        dplyr::mutate(
          group_longest = dplyr::case_when(
            group_longest == "Black, non-Hispanic" ~ "Black",
            group_longest == "White, non-Hispanic" ~ "White",
            group_longest == "Hispanic, any race" ~ "Hispanic",
            group_longest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
            TRUE ~ group_longest
          ),
          group_shortest = dplyr::case_when(
            group_shortest == "Black, non-Hispanic" ~ "Black",
            group_shortest == "White, non-Hispanic" ~ "White",
            group_shortest == "Hispanic, any race" ~ "Hispanic",
            group_shortest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
            TRUE ~ group_shortest
          )
        )
    }

    # Focus on relevant comparisons: e.g., Black, Hispanic vs. White (for race) or Male vs. Female (for sex)
    if (grouping_var == "race") {
      df_disparity_filtered <- df_disparity |>
        dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic", "non-Hispanic people of other races"))
    } else {
      df_disparity_filtered <- df_disparity |>
        dplyr::filter(group_shortest == "Female" & group_longest == "Male") |>
        dplyr::mutate(
          group_longest = "males",
          group_shortest = "females"
        )
    }

    # Handle cases with no significant disparities
    if (nrow(df_disparity_filtered) == 0) {
      time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0("The chart below shows the average ", time_description, " by offense type and ",
                    ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."))
    }

    # Exclude "Other Violent Offenses" if it's the largest disparity (and there are other offenses)
    if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
      df_disparity_filtered <- df_disparity_filtered |> dplyr::slice(2)
    }

    # Extract details for the largest disparity
    largest_disparity <- df_disparity_filtered |> dplyr::slice(1)
    offense_type <- largest_disparity$fbi_index
    group_longest <- largest_disparity$group_longest
    group_shortest <- largest_disparity$group_shortest
    disparity_diff <- largest_disparity$diff_los

    # Adjust for months if disparity is less than 1 year
    time_value <- if (disparity_diff < 1) round(disparity_diff * 12) else round(disparity_diff, 1)
    time_unit <- if (disparity_diff < 1) "months" else "years"

    # Construct the descriptive sentence
    time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
    sentence <- paste0(
      "The chart below shows the average ", time_description, " by offense type and ",
      ifelse(grouping_var == "race", "race and ethnicity", grouping_var), ". ",
      "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
      group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
      " spent on average ", time_value, " more ", time_unit, " in prison compared to ",
      group_shortest, if (grouping_var == "race") " people" else "", "."
    )

    return(sentence)
  })

  # Assign state names to the resulting list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}
