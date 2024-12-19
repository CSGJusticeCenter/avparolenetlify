fnc_generate_offense_disparity_sentence_v2 <- function(data, grouping_var = "race", time_var = "average_los") {

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
        summarize(avg_time = mean(.data[[time_var]], na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = race,
          values_from = avg_time,
          names_prefix = "",
          names_glue = "{race}_average_time"
        ) |>
        rename(
          White_average_time = `White, non-Hispanic_average_time`,
          Hispanic_average_time = `Hispanic, any race_average_time`,
          Black_average_time = `Black, non-Hispanic_average_time`
        ) |>
        mutate(diff_Hispanic_White = Hispanic_average_time - White_average_time,
               diff_Black_White = Black_average_time - White_average_time) |>
        select(state, fbi_index, diff_Hispanic_White, diff_Black_White) |>
        rowwise() |>
        mutate(
          largest_diff = ifelse(
            all(is.na(c_across(c(diff_Hispanic_White, diff_Black_White)))),
            NA_real_,
            max(c_across(c(diff_Hispanic_White, diff_Black_White)), na.rm = TRUE)
          ),
          chosen_column = case_when(
            largest_diff == diff_Hispanic_White ~ "diff_Hispanic_White",
            largest_diff == diff_Black_White ~ "diff_Black_White",
            TRUE ~ NA_character_
          )
        ) |>
        ungroup() |>
        filter(!is.na(largest_diff)) |>
        slice_max(largest_diff, with_ties = TRUE)

    } else if (grouping_var == "sex") {

      df_wide_largest <- df1 |>
        group_by(state, sex, fbi_index) |>
        summarize(avg_time = mean(.data[[time_var]], na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = sex,
          values_from = avg_time,
          names_prefix = "",
          names_glue = "{sex}_average_time"
        ) |>
        mutate(diff_Male_Female = Male_average_time - Female_average_time) |>
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

    # Extract details for the largest disparity
    if (nrow(df_wide_largest) == 0 || all(is.na(df_wide_largest$largest_diff))) {
      # Default sentence when no significant differences are found
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
      chosen_column == "diff_Male_Female" ~ "men",
      TRUE ~ NA_character_
    )
    group_shortest <- ifelse(chosen_column %in% c("diff_Hispanic_White", "diff_Black_White"), "White", "women")
    disparity_diff <- df_wide_largest$largest_diff

    if (is.na(group_longest)) {
      time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0(
        "The chart below shows the average ", time_description, " by offense type and ",
        ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."
      ))
    }

    # Format time to years and months
    formatted_time <- fnc_time_format(abs(disparity_diff))

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







###################################################################################################################

all_sentence_los_race_offense <- fnc_generate_offense_disparity_sentence_v2(los_race_by_offense_type,
                                                                         "race",
                                                                         "average_los")
# Example state:
all_sentence_los_race_offense$Georgia
all_sentence_los_race_offense$Idaho
all_sentence_los_race_offense$Arkansas
all_sentence_los_race_offense$Colorado

all_sentence_los_sex_offense <- fnc_generate_offense_disparity_sentence_v2(los_sex_by_offense_type,
                                                                        "sex",
                                                                        "average_los")
# Example state:
all_sentence_los_sex_offense$Georgia
all_sentence_los_sex_offense$Idaho
all_sentence_los_sex_offense$Arkansas

all_sentence_avg_past_pe_race_offense <- fnc_generate_offense_disparity_sentence_v2(avg_past_pe_race_by_offense_type,
                                                                                 "race",
                                                                                 "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_race_offense$Georgia
all_sentence_avg_past_pe_race_offense$Arkansas

all_sentence_avg_past_pe_sex_offense <- fnc_generate_offense_disparity_sentence_v2(avg_past_pe_sex_by_offense_type,
                                                                                "sex",
                                                                                "avg_years_to_estimated_pey")

# Example state:
all_sentence_avg_past_pe_sex_offense$Georgia

