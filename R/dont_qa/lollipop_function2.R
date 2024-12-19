
fnc_create_lollipop_chart <- function(df, group_var, value_var, state_name, height = 200, source) {

  # Define consistent group labels, colors, and shapes
  if (group_var == "sex") {
    group_labels <- c("Male", "Female")
    colors <- c(teal, purple)
    shapes <- c("circle", "triangle")
  } else {
    group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
    colors <- c(teal, blue, purple, red)
    shapes <- c("square", "circle", "diamond", "triangle")
  }

  # Filter and prepare data for the specified state
  df1 <- df |>
    ungroup() |>
    filter(state == state_name) |>
    arrange(desc(!!sym(value_var))) |>
    mutate(group_num = row_number(),
           color = case_when(
             !!sym(group_var) == group_labels[1] ~ colors[1],
             !!sym(group_var) == group_labels[2] ~ colors[2],
             !!sym(group_var) == group_labels[3] ~ colors[3],
             !!sym(group_var) == group_labels[4] ~ colors[4]
           )) |>
    rowwise() |>  # Ensure row-wise operation
    mutate(time_label = fnc_time_format(!!sym(value_var))) |>  # Apply `fnc_time_format` to each row
    ungroup()  # Remove rowwise grouping after mutate

  year <- unique(df1$rptyear)
  max_value <- max(df1[[value_var]], na.rm = TRUE)

  # Create a named list for y-axis labels
  y_labels <- as.list(setNames(as.character(df1[[group_var]]), df1$group_num))

  # Accessibility text
  accessibility_text <- paste0("The chart below shows the average ", value_var, " for different ",
                               group_var, " groups in ", state_name, ".")

  # Prepare line segments for the lollipop chart
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = !!sym(value_var)) |>
    select(group_num, start_x, end_x, !!sym(group_var)) |>
    gather(key = "point", value = "value", start_x, end_x)

  # Set chart title dynamically based on the measure
  chart_title <- ifelse(
    value_var == "average_los",
    paste("Average Time Served by", ifelse(group_var == "sex", "Sex", "Race and Ethnicity")),
    paste("Average Years Past Parole Eligibility by", ifelse(group_var == "sex", "Sex", "Race and Ethnicity"))
  )

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_title(text = chart_title) |>

    # Line series for the lollipop stems
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = group_num, group = !!sym(group_var)),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Add scatter points for each group with markers and labels
  for (i in seq_along(group_labels)) {
    highcharts <- highcharts |>
      hc_add_series(
        df1 %>% filter(!!sym(group_var) == group_labels[i]),
        type = 'scatter',
        color = colors[i],
        hcaes(x = !!sym(value_var), y = group_num, group = !!sym(group_var), name = !!sym(group_var)),
        marker = list(
          radius = 5,
          symbol = shapes[i]  # Use unique shape for each group
        ),
        dataLabels = list(
          enabled = TRUE,
          format = '{point.time_label}',  # Use the formatted time_label
          align = "left",
          y = 9,
          x = 8,
          style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
        )
      )
  }

  # Add axes, themes, and captions
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
      max = max_value * 1.5
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

fnc_generate_lollipop_charts <- function(df, group_var, value_var, height = 200, source) {

  states <- unique(df$state)

  all_charts <- purrr::map(states, function(state_var) {
    fnc_create_lollipop_chart(
      df = df,
      group_var = group_var,
      value_var = value_var,
      state_name = state_var,
      source = ncrp_source,
      height = height
    )
  })

  all_charts <- setNames(all_charts, states)

  return(all_charts)
}

# Generate lollipop charts of time served by sex
all_lollipop_los_sex <- fnc_generate_lollipop_charts(
  df = los_sex,
  group_var = "sex",
  value_var = "average_los"
)

# Example states:
all_lollipop_los_sex$Mississippi
