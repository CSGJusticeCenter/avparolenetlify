#  WORKS!
# fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#
#   # Get unique states to iterate over
#   states <- unique(df$state)
#
#   # Generate sentence for each state
#   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#
#     year <- df |>
#       filter(state == state_var) |>
#       pull(rptyear) |>
#       max(na.rm = TRUE)
#
#     # Check for the comparison variable ("sex" or "race")
#     if (compare_var == "sex") {
#
#       # Filter data for the specified state
#       df1 <- df |>
#         ungroup() |>
#         filter(state == state_var) |>
#         filter(rptyear == year)
#
#       # Handle missing data for the state
#       if (nrow(df1) == 0) {
#         return(paste0("No data available for ", state_var))
#       }
#
#       # Focus on comparisons with males
#       df_male <- df1 |> dplyr::filter(sex == "Male")
#
#       # Initialize an empty sentence variable
#       sentence <- ""
#
#       # Generate sentence for female vs male comparison
#       df_female <- df1 |> dplyr::filter(sex == "Female")
#       if (nrow(df_female) > 0 && nrow(df_male) > 0) {
#         los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
#         abs_los_diff_female <- abs(los_diff_female)
#
#         if (!is.na(los_diff_female)) {
#           if (los_diff_female > 0) {
#             sentence <- paste0("In ", year, ", females ",
#                                if (type == "in prison") "released" else "who were still incarcerated",
#                                " spent on average ", abs_los_diff_female,
#                                if (abs_los_diff_female == 1) " more year" else " more years",
#                                " ", if (type == "in prison") "in prison" else "past parole eligibility",
#                                " compared to males in ", state_var, ".")
#           } else if (los_diff_female < 0) {
#             sentence <- paste0("In ", year, ", females ",
#                                if (type == "in prison") "released" else "who were still incarcerated",
#                                " spent on average ", abs_los_diff_female,
#                                if (abs_los_diff_female == 1) " less year" else " less years",
#                                " ", if (type == "in prison") "in prison" else "past parole eligibility",
#                                " compared to males in ", state_var, ".")
#           }
#         }
#       }
#
#       if (sentence != "") {
#         return(sentence)
#       } else {
#         return(paste0("Females and males spent the same average number of years ", if (type == "in prison") "in prison." else "past parole eligibility."))
#       }
#
#     } else if (compare_var == "race") {
#
#       # Filter and categorize races within the data
#       df1 <- df |>
#         dplyr::ungroup() |>
#         dplyr::mutate(race = dplyr::case_when(
#           race == "White, non-Hispanic" ~ "White",
#           race == "Black, non-Hispanic" ~ "Black",
#           race == "Hispanic, any race" ~ "Hispanic",
#           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#         )) |>
#         dplyr::filter(state == state_var) |>
#         filter(rptyear == year)
#
#       # Handle missing data for the state
#       if (nrow(df1) == 0) {
#         return(paste0("No data available for ", state_var, "."))
#       }
#
#       # Focus on comparisons with White individuals
#       df_white <- df1 |> dplyr::filter(race == "White")
#
#       # Initialize variables to hold sentences for each race comparison
#       black_sentence <- ""
#       hispanic_sentence <- ""
#       other_sentence <- ""
#
#       # Generate sentence for Black vs White comparison
#       df_black <- df1 |> dplyr::filter(race == "Black")
#       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#         los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_black <- round(abs(los_diff_black), 1)
#
#         if (!is.na(los_diff_black)) {
#           if (los_diff_black > 0) {
#             black_sentence <- paste0("Black people ",
#                                      if (type == "in prison") "released" else "who were still incarcerated",
#                                      " spent on average ", abs_los_diff_black,
#                                      " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
#           } else if (los_diff_black < 0) {
#             black_sentence <- paste0("Black people ",
#                                      if (type == "in prison") "released" else "who were still incarcerated",
#                                      " spent on average ", abs_los_diff_black,
#                                      if (abs_los_diff_black == 1) " less year" else " less years",
#                                      " ", if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # Generate sentence for Hispanic vs White comparison
#       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#         los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)
#
#         if (!is.na(los_diff_hispanic)) {
#           if (los_diff_hispanic > 0) {
#             hispanic_sentence <- paste0("Hispanic people ",
#                                         if (type == "in prison") "released" else "who were still incarcerated",
#                                         " spent on average ", abs_los_diff_hispanic,
#                                         " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
#           } else if (los_diff_hispanic < 0) {
#             hispanic_sentence <- paste0("Hispanic people ",
#                                         if (type == "in prison") "released" else "who were still incarcerated",
#                                         " spent on average ", abs_los_diff_hispanic,
#                                         if (abs_los_diff_hispanic == 1) " less year" else " less years",
#                                         " ", if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # Generate sentence for Other races vs White comparison
#       df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
#       if (nrow(df_other) > 0 && nrow(df_white) > 0) {
#         los_diff_other <- round(df_other[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_other <- round(abs(los_diff_other), 1)
#
#         if (!is.na(los_diff_other)) {
#           if (los_diff_other > 0) {
#             other_sentence <- paste0("non-Hispanic people of other races ",
#                                      if (type == "in prison") "released" else "who were still incarcerated",
#                                      " spent on average ", abs_los_diff_other,
#                                      " more years ", if (type == "in prison") "in prison" else "past parole eligibility")
#           } else if (los_diff_other < 0) {
#             other_sentence <- paste0("non-Hispanic people of other races ",
#                                      if (type == "in prison") "released" else "who were still incarcerated",
#                                      " spent on average ", abs_los_diff_other,
#                                      if (abs_los_diff_other == 1) " less year" else " less years",
#                                      " ", if (type == "in prison") "in prison" else "past parole eligibility")
#           }
#         }
#       }
#
#       # Combine sentences or indicate no significant differences
#       sentences <- c(black_sentence, hispanic_sentence, other_sentence)
#       sentences <- sentences[sentences != ""]
#       if (length(sentences) > 0) {
#         return(paste0("In ", year, ", ", paste(sentences, collapse = ", and "), " compared to White people."))
#       } else {
#         return("No significant differences in average years spent compared to White people.")
#       }
#
#     } else {
#       return("Invalid comparison variable.")
#     }
#   })
#
#   # Assign state names to list
#   all_sentences <- setNames(all_sentences, states)
#
#   return(all_sentences)
# }


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
        return(paste0("Females and males spent the same average number of years ", if (type == "in prison") "in prison." else "past parole eligibility."))
      }

    } else if (compare_var == "race") {

      # # Check if the race data has the expected values
      # print(paste("Available races in", state_var, ":", unique(df1$race)))

      # Filter and categorize races within the data
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


all_sentence_los_race <-
  fnc_generate_disparity_sentences(df = los_race,
                                   type = "in prison",
                                   compare_var = "race",
                                   los_col = "average_los")
all_sentence_los_race$Georgia
all_sentence_los_race$Hawaii

all_sentence_los_sex <-
  fnc_generate_disparity_sentences(df = los_sex,
                                   type = "in prison",
                                   compare_var = "sex",
                                   los_col = "average_los")
all_sentence_los_sex$Georgia









# generate_summary_sentence <- function(data, state_column, year_column, value_column, increase_word = "increased", decrease_word = "decreased") {
#   # Filter data for the specific state
#   earliest_year <- min(data[[year_column]])
#   latest_year <- max(data[[year_column]])
#
#   earliest_value <- data[[value_column]][data[[year_column]] == earliest_year]
#   latest_value <- data[[value_column]][data[[year_column]] == latest_year]
#
#   # Handle cases where values are missing
#   if(is.na(earliest_value) | length(earliest_value) == 0) {
#     earliest_year <- min(data[[year_column]][!is.na(data[[value_column]])])
#     earliest_value <- data[[value_column]][data[[year_column]] == earliest_year]
#   }
#   if(is.na(latest_value) | length(latest_value) == 0) {
#     latest_year <- max(data[[year_column]][!is.na(data[[value_column]]) & data[[year_column]] < latest_year])
#     latest_value <- data[[value_column]][data[[year_column]] == latest_year]
#   }
#
#   # Calculate the percentage change
#   percent_change <- (latest_value - earliest_value) / earliest_value * 100
#   change_type <- ifelse(percent_change < 0, decrease_word, increase_word)
#   percent_change_abs <- abs(round(percent_change, 0))
#
#   # Construct the summary sentence
#   sentence <- paste0("From ", earliest_year, " to ", latest_year, ", the value ",
#                      change_type, " ", percent_change_abs, " percent, changing from ",
#                      format(earliest_value, big.mark = ","), " in ",
#                      earliest_year, " to ", format(latest_value, big.mark = ","), " in ", latest_year, ".")
#   return(sentence)
# }
#
# states <- c("Georgia")
# # For prison population
# all_sentence_population <- map(states, function(x) {
#   df1 <- bjs_prison_pop_by_rptyear %>% filter(state == x)
#   generate_summary_sentence(df1, "state", "rptyear", "bjs_prison_population")
# })
#
# all_sentence_population$Georgia
#
# # Race and Ethnicity
# all_bar_pe_race <- fnc_generate_bar_charts(
#   data       = current_pe_race,
#   x_var      = "race",
#   metric     = "Race and Ethnicity",
#   type_desc  = "the prison population past parole eligibility",
#   title_type = "People in Prison Past Parole Eligibility",
#   y_var      = "prop"
# )
# all_sentence_pe_race <- fnc_generate_sentences(
#   data      = current_pe_race,
#   x_var     = "race",
#   type_desc = "in prison past parole eligibility"
# )
#
# # Sex
# all_bar_pe_sex <- fnc_generate_bar_charts(
#   data       = current_pe_sex,
#   x_var      = "sex",
#   metric     = "Sex",
#   type_desc  = "the prison population past parole eligibility",
#   title_type = "People in Prison Past Parole Eligibility",
#   y_var      = "prop"
# )
# all_sentence_pe_sex <- fnc_generate_sentences(
#   data      = current_pe_sex,
#   x_var     = "sex",
#   type_desc = "in prison past parole eligibility"
# )
#
# # Age Year End
# all_bar_pe_ageyrend <- fnc_generate_bar_charts(
#   data       = current_pe_ageyrend,
#   x_var      = "ageyrend",
#   metric     = "Age",
#   type_desc  = "the prison population past parole eligibility",
#   title_type = "People in Prison Past Parole Eligibility",
#   y_var      = "prop"
# )
# all_sentence_pe_ageyrend <- fnc_generate_sentences(
#   data      = current_pe_ageyrend,
#   x_var     = "ageyrend",
#   type_desc = "in prison past parole eligibility"
# )
#
# # Sentence Length
# all_bar_pe_sentlgth <- fnc_generate_bar_charts(
#   data       = current_pe_sentlgth,
#   x_var      = "sentlgth",
#   metric     = "Sentence Length",
#   type_desc  = "the prison population past parole eligibility",
#   title_type = "People in Prison Past Parole Eligibility",
#   y_var      = "prop"
# )
# all_sentence_pe_sentlgth <- fnc_generate_sentences(
#   data      = current_pe_sentlgth,
#   x_var     = "sentlgth",
#   type_desc = "in prison past parole eligibility"
# )
#
# # FBI Index
# all_bar_pe_fbi_index <- fnc_generate_bar_charts(
#   data       = current_pe_fbi_index,
#   x_var      = "fbi_index",
#   metric     = "Offense Type",
#   type_desc  = "the prison population past parole eligibility",
#   title_type = "People in Prison Past Parole Eligibility",
#   y_var      = "prop"
# )
# all_sentence_pe_fbi_index <- fnc_generate_sentences(
#   data      = current_pe_fbi_index,
#   x_var     = "fbi_index",
#   type_desc = "in prison past parole eligibility"
# )
