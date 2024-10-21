#' Retrieve Census Data by State
#'
#' Retrieves decennial census data for a specified state, focusing on race and ethnicity variables.
#' The function uses the `tidycensus` package to get race-specific population estimates and
#' formats the data accordingly.
#'
#' @param state A string representing the state for which to retrieve census data.
#'
#' @return A data frame containing race-specific population estimates for the specified state,
#' with columns cleaned and race categories recoded.
#' @export
fnc_get_census_data <- function(state) {
  df <-
    tidycensus::get_decennial(
      geography = "state",
      state = state,
      variables = race_vars,
      summary_var = "P3_001N",
      year = census_year,
      geometry = FALSE) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race = case_when(
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    )
  return(df)
}

#' Generate Disparity Sentences Based on Length of Stay Data
#'
#' This function generates descriptive sentences that explain disparities in the average length
#' of stay (LOS) for different comparison groups (sex or race) within a specific state.
#'
#' @param state_var The state being analyzed.
#' @param df The dataframe containing the data (must include state, sex or race, and LOS columns).
#' @param type A string indicating the type of data being analyzed (e.g., "incarceration").
#' @param compare_var A string indicating the comparison variable (either "sex" or "race").
#' @param los_col The name of the column containing the length of stay data.
#'
#' @return A string sentence describing the disparities between groups, or a message if no data is available.
#'
#' @details This function can compare average LOS between males and females, or between races
#' (White, Black, and Hispanic) within a state.
# fnc_generate_los_disparity_sentences <- function(df, type, compare_var, los_col, year = select_year) {
#   # Get unique states to iterate over
#   states <- unique(df$state)
#
#   # Generate sentence for each state
#   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#
#     # Check for the comparison variable ("sex" or "race")
#     if (compare_var == "race") {
#
#       # Filter and categorize races within the data
#       df1 <- df |>
#         dplyr::ungroup() |>
#         dplyr::mutate(race = dplyr::case_when(
#           race == "White, non-Hispanic" ~ "White",
#           race == "Black, non-Hispanic" ~ "Black",
#           race == "Hispanic, any race" ~ "Hispanic"
#         )) |>
#         dplyr::filter(state == state_var)
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
#
#       # Generate sentence for Black vs White comparison using rounded values
#       df_black <- df1 |> dplyr::filter(race == "Black")
#       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#         los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_black <- round(abs(los_diff_black), 1)  # round to one decimal place
#
#         if (!is.na(los_diff_black)) {
#           if (los_diff_black > 0) {
#             black_sentence <- paste0("Black people who were still incarcerated spent on average ",
#                                      abs_los_diff_black, " more years ", type)
#           } else if (los_diff_black < 0) {
#             black_sentence <- paste0("Black people who were still incarcerated spent on average ",
#                                      abs_los_diff_black,
#                                      if (abs_los_diff_black == 1) " less year" else " less years",
#                                      " ", type)
#           }
#         }
#       }
#
#       # Generate sentence for Hispanic vs White comparison using rounded values
#       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#         los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
#         abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)  # round to one decimal place
#
#         if (!is.na(los_diff_hispanic)) {
#           if (los_diff_hispanic > 0) {
#             hispanic_sentence <- paste0("Hispanic people who were still incarcerated spent on average ",
#                                         abs_los_diff_hispanic, " more years ", type)
#           } else if (los_diff_hispanic < 0) {
#             hispanic_sentence <- paste0("Hispanic people who were still incarcerated spent on average ",
#                                         abs_los_diff_hispanic,
#                                         if (abs_los_diff_hispanic == 1) " less year" else " less years",
#                                         " ", type)
#           }
#         }
#       }
#
#       # Combine both sentences, or indicate no significant differences
#       if (black_sentence != "" && hispanic_sentence != "") {
#         if (abs_los_diff_black == abs_los_diff_hispanic) {
#           sentence <- paste0("In ", year, ", Black people and Hispanic people who were still incarcerated spent on average ",
#                              abs_los_diff_black, " more years ", type, " compared to White people.")
#         } else {
#           sentence <- paste0("In ", year, ", ", black_sentence, ", and ",
#                              hispanic_sentence, " compared to White people.")
#         }
#       } else if (black_sentence != "") {
#         sentence <- paste0("In ", year, ", ", black_sentence, " compared to White people.")
#       } else if (hispanic_sentence != "") {
#         sentence <- paste0("In ", year, ", ", hispanic_sentence, " compared to White people.")
#       } else {
#         sentence <- "" # No significant differences
#       }
#
#       return(sentence)
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
fnc_generate_los_disparity_sentences <- function(df, type, compare_var, los_col, year = select_year) {
  # Get unique states to iterate over
  states <- unique(df$state)

  # Generate sentence for each state
  all_sentences <- purrr::map(.x = states, .f = function(state_var) {

    # Check for the comparison variable ("sex" or "race")
    if (compare_var == "sex") {

      # Filter data for the specified state
      df1 <- df |>
        dplyr::ungroup() |>
        dplyr::filter(state == state_var)

      # Handle missing data for the state
      if (nrow(df1) == 0) {
        return(paste0("No data available for ", state_var))
      }

      # Focus on comparisons with males
      df_male <- df1 |> dplyr::filter(sex == "Male")

      # Initialize an empty sentence variable
      sentence <- ""

      # Generate sentence for female vs male comparison
      df_female <- df1 |> dplyr::filter(sex == "Female")
      if (nrow(df_female) > 0 && nrow(df_male) > 0) {
        los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
        abs_los_diff_female <- abs(los_diff_female)  # Already rounded difference

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

      # Return the generated sentence, or indicate no disparity found
      if (sentence != "") {
        return(sentence)
      } else {
        return(paste0("Females and males spent the same average number of years ", if (type == "in prison") "in prison." else "past parole eligibility."))
      }

    } else if (compare_var == "race") {

      # Filter and categorize races within the data
      df1 <- df |>
        dplyr::ungroup() |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic"
        )) |>
        dplyr::filter(state == state_var)

      # Handle missing data for the state
      if (nrow(df1) == 0) {
        return(paste0("No data available for ", state_var, "."))
      }

      # Focus on comparisons with White individuals
      df_white <- df1 |> dplyr::filter(race == "White")

      # Initialize variables to hold sentences for each race comparison
      black_sentence <- ""
      hispanic_sentence <- ""

      # Generate sentence for Black vs White comparison using rounded values
      df_black <- df1 |> dplyr::filter(race == "Black")
      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_black <- round(abs(los_diff_black), 1)  # round to one decimal place

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

      # Generate sentence for Hispanic vs White comparison using rounded values
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
        abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)  # round to one decimal place

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

      # Combine both sentences, or indicate no significant differences
      if (black_sentence != "" && hispanic_sentence != "") {
        if (abs_los_diff_black == abs_los_diff_hispanic) {
          sentence <- paste0("In ", year, ", Black people and Hispanic people ",
                             if (type == "in prison") "released" else "who were still incarcerated",
                             " spent on average ", abs_los_diff_black,
                             " more years ", if (type == "in prison") "in prison" else "past parole eligibility",
                             " compared to White people.")
        } else {
          sentence <- paste0("In ", year, ", ", black_sentence, ", and ",
                             hispanic_sentence, " compared to White people.")
        }
      } else if (black_sentence != "") {
        sentence <- paste0("In ", year, ", ", black_sentence, " compared to White people.")
      } else if (hispanic_sentence != "") {
        sentence <- paste0("In ", year, ", ", hispanic_sentence, " compared to White people.")
      } else {
        sentence <- "" # No significant differences
      }

      return(sentence)
    } else {
      return("Invalid comparison variable.")
    }
  })

  # Assign state names to list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}



#' Create Lollipop Chart for Race or Sex Disparities
#'
#' This function creates a lollipop chart to visualize disparities in average length
#' of stay (LOS) by race or sex for a given state.
#'
#' @param df The dataframe containing data (must include state, race/sex, and LOS columns).
#' @param group_var A string specifying whether to group by "race" or "sex".
#' @param group_labels A vector of labels for the groups (e.g., "White", "Black", "Hispanic" for race).
#' @param colors A vector of color codes to use for each group in the chart.
#' @param state_name The name of the state being analyzed.
#' @param height The height of the chart (default is 150).
#'
#' @return A highcharter plot object showing a lollipop chart with the LOS disparities.
#'
fnc_create_lollipop_chart <- function(df, group_var, group_labels, colors, state_name, height = 200, year = select_year, source = ncrp_source) {

  # Determine the title based on the group_var
  chart_title <- if (group_var == "sex") {
    paste("Average Time Served by Sex,", year)
  } else if (group_var == "race") {
    paste("Average Time Served by Race and Ethnicity,", year)
  } else {
    paste("Average Time Served by", group_var, ",", year)
  }

  # Filter data for the specified state
  df1 <- df |>
    ungroup() |>
    filter(state == state_name) |>
    arrange(desc(average_los)) |>
    mutate(group_num = row_number(),
           color = case_when(
             !!sym(group_var) == group_labels[1] ~ colors[1],
             !!sym(group_var) == group_labels[2] ~ colors[2],
             !!sym(group_var) == group_labels[3] ~ colors[3] # NA for the third if only 2 groups
           ))

  # Generate accessibility text based on the data
  accessibility_text <- paste0("The chart below the average time served for different ",
                               group_var, " groups in ", state_name, ". ",
                               group_labels[1], " spent on average ", df1$average_los[df1$group_num == 1],
                               " years, followed by ", group_labels[2], " with on average ", df1$average_los[df1$group_num == 2],
                               " years, and ", group_labels[3], " with ", df1$average_los[df1$group_num == 3],
                               " years.")

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named list for y-axis labels (replaces the deprecated named vector)
  y_labels <- as.list(setNames(as.character(df1[[group_var]]), df1$group_num))

  # Create the dataframe for lines in the lollipop chart
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(group_num, start_x, end_x, !!sym(group_var))

  # Reshape data for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_title(text = chart_title) |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = group_num, group = !!sym(group_var)),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Add scatter series for each group with appropriate marker symbols
  for (i in seq_along(group_labels)) {
    highcharts <- highcharts |>
      hc_add_series(
        df1 %>% filter(!!sym(group_var) == group_labels[i]),
        type = 'scatter',
        color = colors[i],
        hcaes(x = average_los, y = group_num, group = !!sym(group_var), name = !!sym(group_var)),
        marker = list(
          radius = 5,
          symbol = ifelse(i == 1, "square", ifelse(i == 2, "circle", "diamond"))
        ),
        dataLabels = list(
          enabled = TRUE,
          format = '{point.x:.1f} Years',
          align = "left",
          y = 9,
          x = 8,
          style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
        )
      )
  }

  # Add y-axis and x-axis customizations
  highcharts <- highcharts |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
        enabled = TRUE,
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      tickColor = "white",
      categories = y_labels # Named list for labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = height) |>
    fnc_add_hc_accessibility(accessibility_text) |>
    hc_caption(text = source)
  return(highcharts)
}


#' Calculate Length of Stay (LOS) by Variable
#'
#' This function calculates the average length of stay (LOS) for people released from prison, grouped by a specified variable (e.g., race, offense type, etc.) and filtered by specific values. It also returns the count of people released in each group.
#'
#' @param df A data frame containing the population data.
#' @param var A string representing the column name of the variable to group by (e.g., race, sex, etc.).
#' @param filter_values A vector of values used to filter the specified variable.
#' @param time_var A string representing the column name of the time-related variable (e.g., length of stay).
#' @param state_col A string representing the column name for the state (default: "state").
#' @param fbi_col A string representing the column name for the FBI index or offense type (default: "fbi_index").
#'
#' @return A data frame containing the average length of stay (LOS) and the count of people released, grouped by the specified variable, state, and offense type.
#' @export
fnc_calc_los_by_var <- function(df,
                                var,
                                filter_values,
                                time_var,
                                state_col = "state",
                                fbi_col = "fbi_index") {
  fnc_filter_population(df) |>
    filter(!!sym(var) %in% filter_values) |>
    group_by(!!sym(state_col), !!sym(var), !!sym(fbi_col)) |>
    summarise(
      avg_time = mean(!!sym(time_var), na.rm = TRUE),
      people = n(),
      .groups = "drop"
    )
}


#' Generate Sentences Describing Offense Disparities
#'
#' This function generates descriptive sentences that highlight disparities in average length of stay (LOS)
#' between different groups (e.g., race or sex) for each offense type within a state. It calculates the
#' disparity between the group with the longest LOS and the group with the shortest LOS, and constructs
#' a sentence summarizing the largest observed disparity.
#'
#' @param data A data frame containing the population data, including information on offense types and length of stay.
#' @param grouping_var A string representing the column name for the group variable (e.g., "race", "sex") (default: "race").
#' @param time_var A string representing the time-related variable to use (e.g., "avg_los", "time_served") (default: "avg_los").
#'
#' @return A named list of sentences, where each state has a sentence describing the largest disparity in length of stay between groups.
#' @export
fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "avg_los") {
  # Get unique states to iterate over
  states <- unique(data$state)

  # Generate sentence for each state
  all_sentences <- purrr::map(.x = states, .f = function(x) {
    df1 <- data |>
      dplyr::filter(state == x & fbi_index != "Unknown")

    # Handling missing data
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", x))
    }

    # Calculate the difference in average LOS between groups for each offense type
    df_disparity <- df1 %>%
      dplyr::group_by(fbi_index) %>%
      dplyr::reframe(
        max_los = max(!!rlang::sym(time_var)),
        min_los = min(!!rlang::sym(time_var)),
        diff_los = max_los - min_los,
        group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],
        group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]
      ) %>%
      dplyr::arrange(dplyr::desc(diff_los))

    # Adjust group names for race and ethnicity or sex, depending on grouping_var
    if (grouping_var == "race") {
      df_disparity <- df_disparity %>%
        dplyr::mutate(
          group_longest = dplyr::case_when(
            group_longest == "Black, non-Hispanic" ~ "Black",
            group_longest == "White, non-Hispanic" ~ "White",
            group_longest == "Hispanic, any race" ~ "Hispanic",
            TRUE ~ group_longest
          ),
          group_shortest = dplyr::case_when(
            group_shortest == "Black, non-Hispanic" ~ "Black",
            group_shortest == "White, non-Hispanic" ~ "White",
            group_shortest == "Hispanic, any race" ~ "Hispanic",
            TRUE ~ group_shortest
          )
        )
    }

    # Filter for Black or Hispanic vs. White (or Male vs. Female for sex)
    if (grouping_var == "race") {
      df_disparity_filtered <- df_disparity %>%
        dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic"))
    } else {
      df_disparity_filtered <- df_disparity %>%
        dplyr::filter(group_shortest == "Female" & group_longest == "Male") %>%
        dplyr::mutate(
          group_longest = "males",
          group_shortest = "females"
        )
    }

    # If no disparities exist, return default message
    if (nrow(df_disparity_filtered) == 0) {
      time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0("The chart below shows the average ", time_description, " by offense type and ",
                    ifelse(grouping_var == "race", "race and ethnicity", grouping_var), " in 2020."))
    }

    # Remove "Other Violent Offenses" if it has the largest disparity
    if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
      df_disparity_filtered <- df_disparity_filtered %>% dplyr::slice(2)
    }

    # Get the largest remaining disparity
    largest_disparity <- df_disparity_filtered %>% dplyr::slice(1)

    # Extract values for the sentence
    offense_type <- largest_disparity$fbi_index
    group_longest <- largest_disparity$group_longest
    los_longest <- round(largest_disparity$max_los, 1)
    group_shortest <- largest_disparity$group_shortest
    los_shortest <- round(largest_disparity$min_los, 1)
    disparity_diff <- round(largest_disparity$diff_los, 1)

    # Construct the sentence
    time_description <- ifelse(time_var == "avg_los", "time served in prison", "time spent in prison past parole eligibility")
    sentence <- paste0(
      "The chart below shows the average ", time_description, " by offense type and ",
      ifelse(grouping_var == "race", "race and ethnicity", grouping_var), " in 2020. ",
      "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
      group_longest, ifelse(grouping_var == "race", " people", ""),  # Add 'people' only if grouping_var is 'race'
      " spent on average ", disparity_diff, " more years in prison compared to ",
      group_shortest, ifelse(grouping_var == "race", " people", ""), "."
    )


    return(sentence)
  })

  # Assign state names to list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}


fnc_xaxis_labels_right <- list(
  useHTML = TRUE,
  enabled = TRUE,
  formatter = JS(
    "function() {
                    var label = this.value;
                    var maxLength = 23;
                    if (label.length > maxLength) {
                      var words = label.split(' ');
                      var result = [];
                      var line = [];
                      var lineLength = 0;

                      words.forEach(function(word) {
                        if (lineLength + word.length > maxLength) {
                          result.push(line.join(' '));
                          line = [];
                          lineLength = 0;
                        }
                        line.push(word);
                        lineLength += word.length + 1;
                      });
                      if (line.length > 0) {
                        result.push(line.join(' '));
                      }
                      return result.join('<br>');
                    } else {
                      return label;
                    }
                  }"
  ),
  style = list(fontSize = "1em", fontFamily = "Graphik",
               textAlign = "right" )
)


#' Create Scatter Charts by State
#'
#' This function generates scatter charts for each state, showing the average time served or years past parole eligibility by offense type and a grouping variable (e.g., race or sex). The charts include custom tooltips, color schemes, and accessibility text.
#'
#' @param df A data frame containing the data for each state, including variables for offense types, grouping (e.g., race, sex), and time served.
#' @param group_var A string representing the column name of the grouping variable (e.g., "race", "sex").
#' @param measure A string representing the measure to plot on the x-axis (e.g., "avg_los" for average length of stay, or "years past parole eligibility").
#' @param group_labels A character vector containing labels for the groups to be used in the chart (e.g., c("White", "Black", "Hispanic")).
#' @param colors A character vector containing colors for each group in the chart.
#' @param year An integer representing the year of the data (default: 2020).
#' @param source A string representing the data source to be displayed as a caption on the chart.
#'
#' @return A named list of Highcharts objects, where each state has its own scatter chart showing the average time served or years past parole eligibility by offense type and grouping variable.
#' @export
fnc_create_scatter_charts_by_state <- function(df,
                                               group_var,
                                               measure,
                                               group_labels,
                                               colors,
                                               year = 2020,
                                               source = ncrp_source) {

  # Get unique states to iterate over
  states <- unique(df$state)

  # Iterate over each state to generate charts
  all_charts <- purrr::map(.x = states, .f = function(state_name) {

    # Define titles and labels based on the measure
    x_axis_title <- ifelse(measure == "avg_los", "Average Time Served (Years)", "Average Years Past Parole Eligibility")
    chart_title <- paste0("Average ", ifelse(measure == "avg_los", "Time Served", "Years Past Parole Eligibility"),
                          " by Offense and ", ifelse(group_var == "sex", "Gender", "Race and Ethnicity"), ", ", year)
    tooltip_format <- ifelse(measure == "avg_los", "Average LOS: {point.x: .1f} years<br/>",
                             "Years Past Parole: {point.x: .1f} years<br/>")
    accessibility_measure <- ifelse(measure == "avg_los", "average length of stay", "average years past parole eligibility")

    # Filter data for the specified state
    df1 <- df |>
      ungroup() |>
      filter(state == state_name & fbi_index != "Unknown") |>
      mutate(fbi_index = fct_rev(as.factor(fbi_index))) |>
      arrange(desc(!!sym(measure))) |>
      mutate(avg_time = round(!!sym(measure), 1),
             fbi_index_num = as.numeric(as.factor(fbi_index)),
             color = case_when(
               !!sym(group_var) == group_labels[1] ~ colors[1],
               !!sym(group_var) == group_labels[2] ~ colors[2],
               !!sym(group_var) == group_labels[3] ~ colors[3] # NA for the third if only 2 groups
             ))

    # Generate accessibility text based on the data
    accessibility_text <- paste0("The chart below the ", accessibility_measure, " for different ",
                                 ifelse(group_var == "sex", "gender", "race and ethnicity"),
                                 " groups in ", state_name, ". ",
                                 group_labels[1], " spent on average ", df1$avg_time[df1$fbi_index_num == 1],
                                 " years, followed by ", group_labels[2], " with on average ", df1$avg_time[df1$fbi_index_num == 2],
                                 " years, and ", group_labels[3], " with ", df1$avg_time[df1$fbi_index_num == 3], " years.")

    max_los <- max(df1$avg_time, na.rm = TRUE)

    # Create a named list for y-axis labels
    y_labels <- as.list(setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index)))))

    # Initialize the highchart object
    highcharts <- highchart()

    # Add scatter series for each group with appropriate marker symbols
    for (i in seq_along(group_labels)) {
      highcharts <- highcharts |>
        hc_add_series(
          df1 %>% filter(!!sym(group_var) == group_labels[i]),
          type = 'scatter',
          color = colors[i],
          hcaes(x = avg_time, y = fbi_index_num, group = !!sym(group_var), name = fbi_index),
          marker = list(
            radius = 5,
            symbol = ifelse(i == 1, "square", ifelse(i == 2, "circle", "diamond"))
          )
        )
    }

    # Add y-axis and x-axis customizations
    highcharts <- highcharts |>
      hc_add_theme(base_hc_theme) |>
      hc_yAxis(
        title = list(text = ""),
        labels = fnc_xaxis_labels_right,
        majorGridLineColor = "transparent",
        gridLineColor = "transparent",
        lineColor = "transparent",
        majorGridLineColor = "transparent",
        minorGridLineColor = "transparent",
        tickColor = "white",
        categories = y_labels
      ) |>
      hc_xAxis(
        lineColor = "black",
        tickColor = "white",
        title = list(text = x_axis_title,
                     style = list(color = "black")),
        labels = list(style = list(color = "black")),
        gridLineDashStyle = "Dash",  # Add dashed grid lines
        gridLineWidth = 1,           # Ensure grid lines are visible
        gridLineColor = lightgray       # Set grid line color
      ) |>
      hc_title(text = chart_title) |>
      hc_exporting(enabled = TRUE) |>
      # hc_tooltip(
      #   pointFormat = paste0(
      #     '<span style="color:{point.color}">\u25CF</span> {series.name}:<br/>',
      #     'Offense: {point.name}<br/>',
      #     tooltip_format,
      #     'People in Prison: {point.people}<br/>'
      #   )
      # ) |>
      hc_tooltip(
        borderWidth = 1,
        borderRadius = 0,
        backgroundColor = '#FFFFFF', # Fully opaque white background
        outside = TRUE, # Ensure tooltip is rendered outside
        useHTML = TRUE,
        formatter = JS("function() {
      return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
      '<div style=\"text-align:left;\">' +
      '<span style=\"font-weight:bold;\">' + this.series.name + '</span><br/>' +
      '<span>Offense: ' + this.point.name + '</span><br/>' +
      '<span>Average Years Past Parole Eligibility: ' + this.point.x.toFixed(1) + '</span><br/>' +
      '<span>People in Prison: ' + this.point.people + '</span>' +
      '</div></div>';
    }")
      ) |>
      hc_legend(verticalAlign = "top",
                layout = "horizontal") |>
      fnc_add_hc_accessibility(accessibility_text) |>
      hc_caption(text = source)

    return(highcharts)
  })

  # Assign state names to the charts list
  all_charts <- setNames(all_charts, states)

  return(all_charts)
}





#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

#' @title Blank Out Plot Theme
#' @description This function sets up a theme for blanking out plot elements like axes, scales, and legends.
#' @return A list of ggplot2 theme and scale elements for use in plots.
#' @export
fnc_blankitout <- function(){
  list(
    theme_void(),  # Removes background and gridlines for a clean appearance.
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
    theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
  )
}

#' @title Icon Options for Plotting
#' @description Generates a list of plot options based on a partial fill value, colors, and a background for creating icon-based infographics.
#' @param partialval A numeric value between 0 and 1 representing the percentage of the partial icon fill.
#' @param empty Color for the empty part of the icon.
#' @param fill Color for the full part of the icon.
#' @param partial Color for the partially filled icon.
#' @param bg Background color for the icons.
#' @param fillHoriz A logical flag indicating whether to fill icons horizontally (TRUE) or vertically (FALSE).
#' @return A list of ggplot objects representing empty, full, and partial icon states.
#' @export
fnc_icon_options <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
  # Ensure partialval is within valid range
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Define color sets for different states of the icon (empty, full, partial)
  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )

  # Define percentage fills for each icon state
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )

  # Initialize the plot list to store generated plots for each state
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  # Determine the boundaries for filling either horizontally or vertically
  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
    max <- max(pos1)
  }
  h <- dim(img)[1]  # Icon height
  w <- dim(img)[2]  # Icon width
  min <- min(pos1)

  # Loop through each icon state and generate corresponding plot
  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
    pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
    finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
    bkgr <- (finalimg == 1)  # Background mask
    colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix

    # Apply the fill either horizontally or vertically
    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    # Assign partially filled cells in the image
    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting

    # Remove partial fill for the 'full' state
    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    # Create the ggplot for each icon state
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
      fnc_blankitout()  # Apply the blank theme

    plot_lst[[j]] <- plot  # Store the plot in the list
  }

  return(plot_lst)  # Return the list of generated plots
}

#' @title Create Icon Infographic
#' @description Generates an infographic representing a Relative Rate Index (RRI) using icons to indicate full, partial, and empty states.
#' @param rri_raw The raw Relative Rate Index value.
#' @param rri_digits Number of decimal places to round the RRI value.
#' @param fillcolor Color to fill the full icons.
#' @param partialcolor Color to fill partially filled icons.
#' @param emptyhumans Logical flag to indicate whether empty icons should be included.
#' @param emptycolor Color for the empty icons.
#' @param infogs Total number of icons to display.
#' @param infogs_ncol Number of columns for the grid layout of icons.
#' @param fillHoriz Logical flag to determine if icons should be filled horizontally or vertically.
#' @return A ggplot2 object representing the generated icon infographic.
#' @export
fnc_create_icons <- function(rri_raw, rri_digits = 1, fillcolor = dark_color, partialcolor = light_color,
                             emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                             infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value and compute full and partial icons
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partial icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()  # Initialize list for storing plots

  # Create full and partial icons based on RRI value
  if (RRI > 1 & numremain != 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
    plot_list[[numfull + 1]] <- plot_opts$partial
  } else if (RRI > 1 & numremain == 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
  } else if (RRI == 1) {
    plot_list[[1]] <- plot_opts$full
  } else if (RRI < 1) {
    plot_list[[1]] <- plot_opts$partial
  }

  # Add empty icons if needed
  if (emptyhumans == TRUE & length(plot_list) != infogs) {
    st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
    for (i in st_empty:infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(rri_raw / infogs_ncol), 1)

  # Return the grid of icon plots
  plot_grid(plotlist = plot_list, nrow = rows)
}

#' @title Create Infographic with Icons and RRI Label
#' @description Combines a label of the RRI value with a grid of icons representing the RRI.
#' @param rri_raw The raw Relative Rate Index value.
#' @param infographic_color The color for the infographic elements.
#' @return A ggplot object combining the RRI label and the icon grid.
#' @export
fnc_create_infographic <- function(rri_raw, infographic_color) {

  # Round the RRI value and format as a text label
  rri_text <- paste0(round(rri_raw, digits = 1), "x")

  # Generate the icons for the infographic
  ggtemp_justpeople <- fnc_create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = infographic_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Create the plot for displaying the RRI text label
  rri_label_plot <- ggplot() +
    annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
             fontface = "bold",
             color = infographic_color,
             family = "Graphik") +
    theme_void()

  # Combine the RRI label plot with the icon grid
  final_plot <- plot_grid(
    rri_label_plot, ggtemp_justpeople,
    nrow = 1, rel_widths = c(1, 6)  # Adjust widths to balance the label and icons
  )

  print(final_plot)  # Display the final infographic plot
}




