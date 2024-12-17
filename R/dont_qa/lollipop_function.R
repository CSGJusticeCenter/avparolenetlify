#' Create a Lollipop Chart
#'
#' This function generates a lollipop chart for visualizing average values
#' (e.g., time served) by a specified group variable (e.g., sex, race) for a given state.
#'
#' @param df A data frame containing the data to visualize.
#' @param group_var A string indicating the grouping variable (`"sex"` or `"race"`).
#' @param state_name A string specifying the state for which the chart is generated.
#' @param height An integer defining the chart height in pixels. Default is 200.
#' @param source A string specifying the data source for the chart caption.
#' @return A `highchart` object representing the lollipop chart.
#' @examples
#' chart <- fnc_create_lollipop_chart(data, "race", "Georgia", source = "NCRP")
#' @export
fnc_create_lollipop_chart1 <- function(df, group_var, state_name, height = 200, source) {

  # Define consistent group labels, colors, and shapes
  if (group_var == "sex") {
    group_labels <- c("Male", "Female")
    colors <- c(teal, purple)  # Colors for male and female
    shapes <- c("circle", "triangle")  # Shapes for male and female
  } else {
    group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
    colors <- c(teal, blue, purple, red)  # Colors for race groups
    shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
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
             !!sym(group_var) == group_labels[3] ~ colors[3],
             !!sym(group_var) == group_labels[4] ~ colors[4]
           ))

  year <- unique(df1$rptyear)

  # Determine the title based on the group_var
  chart_title <- if (group_var == "sex") {
    # paste("Average Time Served by Sex,", year)
    paste("Average Time Served by Sex")
  } else if (group_var == "race") {
    # paste("Average Time Served by Race and Ethnicity,", year)
    paste("Average Time Served by Race and Ethnicity")
  } else {
    # paste("Average Time Served by", group_var, ",", year)
    paste("Average Time Served by", group_var)
  }

  # Generate accessibility text based on the data
  accessibility_text <- paste0("The chart below shows the average time served for different ",
                               group_var, " groups in ", state_name, ". ",
                               group_labels[1], " spent on average ", df1$average_los[df1$group_num == 1],
                               " years, followed by ", group_labels[2], " with ", df1$average_los[df1$group_num == 2],
                               " years, ", group_labels[3], " with ", df1$average_los[df1$group_num == 3],
                               " years, and ", group_labels[4], " with ", df1$average_los[df1$group_num == 4],
                               " years.")

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named list for y-axis labels
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
          symbol = shapes[i]  # Use unique shape for each group
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
      categories = y_labels
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
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(chart_title)), "_",
                                   year)) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = height) |>
    fnc_add_hc_accessibility(accessibility_text) |>
    hc_caption(text = paste0(source, ", ", year))

  return(highcharts)
}

#' Generate Lollipop Charts for All States
#'
#' This function generates lollipop charts for all states in the provided data
#' by iterating over the unique states.
#'
#' @param df A data frame containing the data to visualize.
#' @param compare_var A string indicating the grouping variable (`"sex"` or `"race"`).
#' @param height An integer defining the chart height in pixels. Default is 200.
#' @return A named list of `highchart` objects, where each element corresponds
#'   to a state.
#' @examples
#' charts <- fnc_generate_lollipop_charts(data, "race")
#' charts$Georgia  # View the chart for Georgia
#' @export
fnc_generate_lollipop_charts1 <- function(df, compare_var, height = 200) {

  # Extract unique states to iterate over
  states <- unique(df$state)

  # Generate lollipop chart for each state
  all_charts <- purrr::map(states, function(state_var) {
    fnc_create_lollipop_chart1(
      df = df,
      group_var = compare_var,
      state_name = state_var,
      source = ncrp_source,
      height = height
    )
  })

  # Assign state names as the list names for easy access
  all_charts <- setNames(all_charts, states)

  return(all_charts)
}

# Generate lollipop charts of time served by race and ethnicity
all_lollipop_los_race <- fnc_generate_lollipop_charts(
  df = los_race,
  compare_var = "race"
)

# Example states:
all_lollipop_los_race$Georgia
