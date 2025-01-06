fnc_time_format1 <- function(time_in_years) {
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

fnc_time_format1_months <- function(time_in_months) {
  time_in_months <- round(time_in_months)  # Round the months
  if (time_in_months < 12) {
    # For times less than 12 months, return only months
    paste0(time_in_months, " ", ifelse(time_in_months == 1, "month", "months"))
  } else {
    # Convert to years and remaining months for times 12 months or more
    years <- floor(time_in_months / 12)
    months <- time_in_months %% 12
    year_part <- paste0(years, " ", ifelse(years == 1, "year", "years"))
    if (months > 0) {
      month_part <- paste0(months, " ", ifelse(months == 1, "month", "months"))
      paste0(year_part, " ", month_part)
    } else {
      year_part
    }
  }
}



fnc_generate_disparity_sentences1 <- function(df, type, compare_var, los_col) {
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
      return(fnc_generate_sentence_sex1(df1, year, type, los_col, state_var))
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
      other_sentence <- ""
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
      converted_black <- round((df_black[[los_col]])*12)
      converted_white <- round((df_white[[los_col]])*12)

      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- converted_black - converted_white
        if (!is.na(los_diff_black)) {
          formatted_time <- fnc_time_format1_months(abs(los_diff_black))
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
      converted_hispanic <- round((df_hispanic[[los_col]])*12)

      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- converted_hispanic - converted_white
        if (!is.na(los_diff_hispanic)) {
          formatted_time <- fnc_time_format1_months(abs(los_diff_hispanic))
          hispanic_sentence <- if (los_diff_hispanic > 0) {
            groups_more <- c(groups_more, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " more behind bars ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " less behind bars ", detail_suffix)
          }
        }
      }

      # Other vs White
      df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
      converted_other <- round((df_other[[los_col]]) * 12)

      if (nrow(df_other) > 0 && nrow(df_white) > 0) {
        los_diff_other <- converted_other - converted_white
        if (!is.na(los_diff_other) && los_diff_other != 0) { # Exclude if no difference
          formatted_time <- fnc_time_format1_months(abs(los_diff_other))
          other_sentence <- if (los_diff_other > 0) {
            groups_more <- c(groups_more, "non-Hispanic people of other races")
            paste0("non-Hispanic people of other races spent on average ", formatted_time, " more behind bars ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "non-Hispanic people of other races")
            paste0("non-Hispanic people of other races spent on average ", formatted_time, " less behind bars ", detail_suffix)
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
      sentences <- c(black_sentence, hispanic_sentence, other_sentence)
      sentences <- sentences[sentences != ""] # Remove empty sentences

      # Exclude sentences mentioning "0 months"
      sentences <- sentences[!grepl("\\b0 months\\b", sentences)]

      if (length(sentences) > 0) {
        # Construct the final sentence with proper formatting
        if (length(sentences) > 1) {
          # Use a comma for the first parts and 'and' before the last item
          sentence_body <- paste0(paste(sentences[-length(sentences)], collapse = ", "), ", and ", sentences[length(sentences)])
        } else {
          # Only one sentence, no need for a conjunction
          sentence_body <- sentences[1]
        }

        # Combine the summary and detailed sentences
        final_sentence <- paste0(overall_summary, " ", sentence_body, " compared to White people.")

        # Correct "Non-Hispanic" capitalization if needed
        final_sentence <- gsub("Non-Hispanic", "non-Hispanic", final_sentence)
        final_sentence <- gsub("\\. and", " and", final_sentence) # Fix edge cases with unnecessary ". and"

        return(final_sentence)
      } else {
        return("")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}

fnc_generate_sentence_sex1 <- function(df1, year, type, los_col, state_var) {
  # Filter the data for males
  df_male <- df1 |> dplyr::filter(sex == "Male")

  # Initialize an empty sentence variable
  sentence <- ""

  # Filter the data for females
  df_female <- df1 |> dplyr::filter(sex == "Female")
  converted_male <- df_male[[los_col]]
  converted_female <- df_female[[los_col]]

  # Check if both male and female data exist
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    # Calculate the difference in length of stay (LOS) between females and males
    los_diff_female <- converted_female - converted_male

    # Ensure the LOS difference is not NA
    if (!is.na(los_diff_female)) {
      formatted_time <- fnc_time_format1(abs(los_diff_female))  # Format the time difference

      if (los_diff_female > 0) {
        # Males spent more time on average
        sentence <- paste0(
          "Men ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", formatted_time, " more ",
          if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to women."
        )
      } else if (los_diff_female < 0) {
        # Females spent less time on average
        sentence <- paste0(
          "Women ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", formatted_time, " less ",
          if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to men."
        )
      }
    }
  }

  # Handle cases where no meaningful disparity exists or data is missing
  if (sentence != "") {
    return(sentence)  # Return the constructed sentence if disparity is found
  } else {
    return(paste0(
      ""
    ))
  }
}

fnc_generate_offense_disparity_sentence1 <- function(data, grouping_var = "race", time_var = "average_los") {

  # Extract unique states to iterate over
  states <- unique(data$state)

  # Generate sentences for each state
  all_sentences <- purrr::map(.x = states, .f = function(x) {

    # Filter data for the specified state and exclude unspecified offense types
    df1 <- data |>
      dplyr::filter(state == x & fbi_index != "Other or Unspecified")

    # Handle missing data: If no data exists for the state, return a message
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", x))
    }

    if (grouping_var == "race") {
      # Transforming data into wide format
      df_wide_largest <- df1 |>
        group_by(state, race, fbi_index) |>
        summarize(avg_time_months = mean(.data[[time_var]] * 12, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = race,
          values_from = avg_time_months,
          names_prefix = "",
          names_glue = "{race}_avg_time_months"
        )

      # Ensure all necessary columns exist by initializing missing columns with NA
      necessary_columns <- c(
        "White, non-Hispanic_avg_time_months",
        "Hispanic, any race_avg_time_months",
        "Black, non-Hispanic_avg_time_months",
        "Other race(s), non-Hispanic_avg_time_months"
      )
      for (col in necessary_columns) {
        if (!col %in% names(df_wide_largest)) {
          df_wide_largest[[col]] <- NA_real_
        }
      }

      # Rename and calculate differences
      df_wide_largest <- df_wide_largest |>
        rename(
          White_avg_time_months = `White, non-Hispanic_avg_time_months`,
          Hispanic_avg_time_months = `Hispanic, any race_avg_time_months`,
          Black_avg_time_months = `Black, non-Hispanic_avg_time_months`,
          Other_avg_time_months = `Other race(s), non-Hispanic_avg_time_months`
        ) |>
        mutate(
          diff_Hispanic_White = Hispanic_avg_time_months - White_avg_time_months,
          diff_Black_White = Black_avg_time_months - White_avg_time_months,
          diff_Other_White = Other_avg_time_months - White_avg_time_months
        ) |>
        select(state, fbi_index, diff_Hispanic_White, diff_Black_White, diff_Other_White) |>
        rowwise() |>
        mutate(
          largest_diff = ifelse(
            all(is.na(c_across(c(diff_Hispanic_White, diff_Black_White, diff_Other_White)))),
            NA_real_,
            max(c_across(c(diff_Hispanic_White, diff_Black_White, diff_Other_White)), na.rm = TRUE)
          ),
          chosen_column = case_when(
            largest_diff == diff_Hispanic_White ~ "diff_Hispanic_White",
            largest_diff == diff_Black_White ~ "diff_Black_White",
            largest_diff == diff_Other_White ~ "diff_Other_White",
            TRUE ~ NA_character_
          )
        ) |>
        ungroup() |>
        filter(!is.na(largest_diff)) |>
        slice_max(largest_diff, with_ties = TRUE)

    } else if (grouping_var == "sex") {
      df_wide_largest <- df1 |>
        group_by(state, sex, fbi_index) |>
        summarize(avg_time_months = mean(.data[[time_var]] * 12, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = sex,
          values_from = avg_time_months,
          names_prefix = "",
          names_glue = "{sex}_avg_time_months"
        ) |>
        mutate(diff_Male_Female = Male_avg_time_months - Female_avg_time_months) |>
        select(state, fbi_index, diff_Male_Female) |>
        rowwise() |>
        mutate(
          largest_diff = ifelse(
            all(is.na(diff_Male_Female)),
            NA_real_,
            max(diff_Male_Female, na.rm = TRUE)
          ),
          chosen_column = ifelse(!is.na(largest_diff), "diff_Male_Female", NA_character_)
        ) |>
        ungroup() |>
        filter(!is.na(largest_diff)) |>
        slice_max(largest_diff, with_ties = TRUE)
    }

    # Handle no significant differences
    if (nrow(df_wide_largest) == 0 || all(is.na(df_wide_largest$largest_diff))) {
      time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0(
        "The chart below shows the average ", time_description, " by offense type and ",
        ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."
      ))
    }

    # Extract details for the largest disparity
    offense_type <- df_wide_largest$fbi_index
    chosen_column <- df_wide_largest$chosen_column
    group_longest <- case_when(
      chosen_column == "diff_Hispanic_White" ~ "Hispanic",
      chosen_column == "diff_Black_White" ~ "Black",
      chosen_column == "diff_Other_White" ~ "non-Hispanic people of other races",
      chosen_column == "diff_Male_Female" ~ "men",
      TRUE ~ NA_character_
    )
    group_shortest <- ifelse(chosen_column %in% c("diff_Hispanic_White", "diff_Black_White", "diff_Other_White"), "White", "women")
    disparity_diff_months <- df_wide_largest$largest_diff

    if (is.na(group_longest)) {
      time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0(
        "The chart below shows the average ", time_description, " by offense type and ",
        ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."
      ))
    }

    # Format time in months and years
    formatted_time <- fnc_time_format1_months(abs(disparity_diff_months))

    # Construct the descriptive sentence
    time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
    sentence <- paste0(
      "The chart below shows the average ", time_description, " by offense type and ",
      ifelse(grouping_var == "race", "race and ethnicity", grouping_var), ". ",
      "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
      group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
      " spent on average ", formatted_time, " more in prison compared to ",
      group_shortest, if (grouping_var == "race") " people" else "", "."
    )

    return(sentence)
  })

  # Assign state names to the resulting list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}



all_sentence_avg_past_pe_race <-
  fnc_generate_disparity_sentences1(df = avg_past_pe_race,
                                   type = "past parole eligibility",
                                   compare_var = "race",
                                   los_col = "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_race$Georgia
all_sentence_avg_past_pe_race$Hawaii

all_sentence_avg_past_pe_sex <-
  fnc_generate_disparity_sentences1(df = avg_past_pe_sex,
                                   type = "past parole eligibility",
                                   compare_var = "sex",
                                   los_col = "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_sex$Georgia

all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence1(avg_past_pe_race_by_offense_type,
                                                                                 "race",
                                                                                 "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_race_offense$Hawaii
all_sentence_avg_past_pe_race_offense$Georgia
