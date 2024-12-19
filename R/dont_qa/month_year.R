fnc_time_format <- function(time_in_years) {
  if (time_in_years < 1) {
    # Convert to months for times under 1 year
    months <- round(time_in_years * 12)
    paste0(months, " ", ifelse(months == 1, "month", "months"))
  } else {
    # Calculate years and remaining months for times over 1 year
    years <- floor(time_in_years)
    months <- round((time_in_years - years) * 12)
    year_part <- paste0(years, " ", ifelse(years == 1, "year", "years"))
    if (months > 0) {
      month_part <- paste0(months, " ", ifelse(months == 1, "month", "months"))
      paste0(year_part, " ", month_part)
    } else {
      year_part
    }
  }
}

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
      return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))

    } else if (compare_var == "race") {
      df1 <- df1 |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic",
          race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
        ))

      df_white <- df1 |> dplyr::filter(race == "White")
      black_sentence <- ""
      hispanic_sentence <- ""
      overall_summary <- ""
      groups_more <- c()
      groups_less <- c()

      if (type == "in prison") {
        summary_phrase <- "spent more time behind bars than White people"
        less_phrase <- "spent less time behind bars than White people"
        detail_suffix <- "in prison"
      } else if (type == "past parole eligibility") {
        summary_phrase <- "spent more time behind bars after becoming eligible for parole than White people"
        less_phrase <- "spent less time behind bars after becoming eligible for parole than White people"
        detail_suffix <- "past parole eligibility"
      }

      # Black vs White
      df_black <- df1 |> dplyr::filter(race == "Black")
      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
        if (!is.na(los_diff_black)) {
          formatted_time <- fnc_time_format(abs(los_diff_black))
          black_sentence <- if (los_diff_black > 0) {
            groups_more <- c(groups_more, "Black people")
            paste0("Black people spent on average ", formatted_time, " more behind bars ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "Black people")
            paste0("Black people spent on average ", formatted_time, " less behind bars ", detail_suffix)
          }
        }
      }

      # Hispanic vs White
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
        if (!is.na(los_diff_hispanic)) {
          formatted_time <- fnc_time_format(abs(los_diff_hispanic))
          hispanic_sentence <- if (los_diff_hispanic > 0) {
            groups_more <- c(groups_more, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " more behind bars ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " less behind bars ", detail_suffix)
          }
        }
      }

      # Construct overall summary
      if (length(groups_more) > 0) {
        overall_summary <- paste0(paste(groups_more, collapse = " and "), " ", summary_phrase, ".")
      }
      if (length(groups_less) > 0) {
        if (overall_summary != "") {
          overall_summary <- paste0(overall_summary, " ")
        }
        overall_summary <- paste0(overall_summary, paste(groups_less, collapse = " and "), " ", less_phrase, ".")
      }

      # Combine sentences
      sentences <- c(black_sentence, hispanic_sentence)
      sentences <- sentences[sentences != ""]
      if (length(sentences) > 0) {
        final_sentence <- paste0(overall_summary, " ", paste(sentences, collapse = ", and "), " compared to White people.")
        final_sentence <- gsub("\\. and", " and", final_sentence)
        return(final_sentence)
      } else {
        return("No significant differences in average years spent compared to White people.")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}


all_sentence_los_race <-
  fnc_generate_disparity_sentences(df = los_race,
                                   type = "in prison",
                                   compare_var = "race",
                                   los_col = "average_los")
# Example state:
all_sentence_los_race$Arkansas
all_sentence_los_race$Georgia
