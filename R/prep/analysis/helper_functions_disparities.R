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
#'
fnc_disparities_sentences <- function(state_var, df, type, compare_var, los_col) {

  # Check for the comparison variable ("sex" or "race")
  if (compare_var == "sex") {

    # Filter data for the specified state
    df1 <- df |>
      ungroup() |>
      filter(state == state_var)

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var))
    }

    # Focus on comparisons with males
    df_male <- df1 |> filter(sex == "Male")

    # Initialize an empty sentence variable
    sentence <- ""

    # Generate sentence for female vs male comparison
    df_female <- df1 |> filter(sex == "Female")
    if (nrow(df_female) > 0 && nrow(df_male) > 0) {
      los_diff_female <- df_female[[los_col]] - df_male[[los_col]]
      if (!is.na(los_diff_female)) {
        abs_los_diff_female <- round(abs(los_diff_female), 1)
        if (los_diff_female > 0) {
          sentence <- paste0("In ", select_year, ", females spent an average of ",
                             abs_los_diff_female,
                             if (abs_los_diff_female == 1) " year more" else " years more",
                             " ", type," compared to males in ", state_var, ".")
        } else if (los_diff_female < 0) {
          sentence <- paste0("In ", select_year, ", females spent an average of ",
                             abs_los_diff_female,
                             if (abs_los_diff_female == 1) " year fewer" else " years fewer",
                             " ", type," compared to males in ", state_var, ".")
        }
      }
    }

    # Return the generated sentence, or indicate no disparity found
    if (sentence != "") {
      return(sentence)
    } else {
      sentence <- paste0("Females and males spent the same average number of years ", type, ".") # No significant differences
    }

  } else if (compare_var == "race") {

    # Filter and categorize races within the data
    df1 <- df |>
      ungroup() |>
      mutate(race = case_when(
        race == "White, non-Hispanic" ~ "White",
        race == "Black, non-Hispanic" ~ "Black",
        race == "Hispanic, any race" ~ "Hispanic"
      )) |>
      filter(state == state_var)

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var, "."))
    }

    # Focus on comparisons with White individuals
    df_white <- df1 |> filter(race == "White")

    # Initialize variables to hold sentences for each race comparison
    black_sentence <- ""
    hispanic_sentence <- ""

    # Generate sentence for Black vs White comparison
    df_black <- df1 |> filter(race == "Black")
    if (nrow(df_black) > 0 && nrow(df_white) > 0) {
      los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
      if (!is.na(los_diff_black) && los_diff_black > 0) {
        black_sentence <- paste0("In ", select_year, ", Black people spent an average of ",
                                 round(los_diff_black, 1), " more years ", type)
      }
    }

    # Generate sentence for Hispanic vs White comparison
    df_hispanic <- df1 |> filter(race == "Hispanic")
    if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
      los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
      if (!is.na(los_diff_hispanic) && los_diff_hispanic > 0) {
        hispanic_sentence <- paste0("Hispanic people spent an average of ",
                                    round(los_diff_hispanic, 1), " more years ", type)
      }
    }

    # Combine both sentences, or indicate no significant differences
    if (black_sentence != "" && hispanic_sentence != "") {
      sentence <- paste0(black_sentence, ", and ", hispanic_sentence, " compared to White people.")
    } else if (black_sentence != "") {
      sentence <- paste0(black_sentence, " compared to White people.")
    } else if (hispanic_sentence != "") {
      sentence <- paste0(hispanic_sentence, " compared to White people.")
    } else {
      sentence <- "" # No significant differences
    }

    return(sentence)
  } else {
    return("Invalid comparison variable.")
  }
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
create_lollipop_chart <- function(df, group_var, group_labels, colors, state_name, height = 150) {

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
  accessibility_text <- paste0("This chart shows the average length of stay for different ",
                               group_var, " groups in ", state_name, ". ",
                               group_labels[1], " spent an average of ", df1$average_los[df1$group_num == 1],
                               " years, followed by ", group_labels[2], " with an average of ", df1$average_los[df1$group_num == 2],
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
    fnc_add_hc_accessibility(accessibility_text)
    # hc_caption(text = source)
  return(highcharts)
}
















