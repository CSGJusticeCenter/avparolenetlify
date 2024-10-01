fnc_hc_lollipop_race <- function(df, x_var){

  df1 <- df |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(!!sym(x_var))) |>
    mutate(race_num = row_number(),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2,
             race == "Other race(s), non-Hispanic" ~ color5
           ))

  max_los <- max(df1[[x_var]], na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$race), df1$race_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = !!sym(x_var)) |>
    select(race_num, start_x, end_x, race)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  highcharts <- highchart() |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = !!sym(x_var), y = race_num, group = race),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Other race(s), non-Hispanic"),
      type = 'scatter',
      color = color5,
      hcaes(x = !!sym(x_var), y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "triangle"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "White, non-Hispanic"),
      type = 'scatter',
      color = color1,
      hcaes(x = !!sym(x_var), y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "square"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Black, non-Hispanic"),
      type = 'scatter',
      color = color4,
      hcaes(x = !!sym(x_var), y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "circle"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Hispanic, any race"),
      type = 'scatter',
      color = color2,
      hcaes(x = !!sym(x_var), y = race_num, group = race, name = race),
      marker = list(
        radius = 5,
        symbol = "diamond"
      ),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    ) |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
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
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      minorGridLineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = 150)

  return(highcharts)

}





fnc_hc_scatter_race <- function(df, x_var){
  df1 <- df |>
    ungroup() |>
    filter(state == x)|>
    mutate(fbi_index_num = as.numeric(as.factor(fbi_index)),
           color = case_when(
             race == "White, non-Hispanic" ~ color1,
             race == "Black, non-Hispanic" ~ color4,
             race == "Hispanic, any race" ~ color2,
             race == "Other race(s), non-Hispanic" ~ color5
           ))

  # Create a named vector for y-axis labels
  y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = !!sym(x_var)) |>
    select(fbi_index_num, start_x, end_x, race, fbi_index)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_add_series(
      df1 %>% filter(race == "Black, non-Hispanic"),
      type = 'scatter',
      color = color4,
      hcaes(x = !!sym(x_var), y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "circle"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Hispanic, any race"),
      type = 'scatter',
      color = color2,
      hcaes(x = !!sym(x_var), y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "diamond"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "White, non-Hispanic"),
      type = 'scatter',
      color = color1,
      hcaes(x = !!sym(x_var), y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "square"
      )
    ) |>
    hc_add_series(
      df1 %>% filter(race == "Other race(s), non-Hispanic"),
      type = 'scatter',
      color = color5,
      hcaes(x = !!sym(x_var), y = fbi_index_num, group = race, name = fbi_index),
      marker = list(
        radius = 5,
        symbol = "triangle"
      )
    ) |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "black",
      title = list(text = "Average Time Served (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = "Average Time Served by Offense and Race and Ethnicity") |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(
      headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span> {series.name}:<br/>',
        'Offense: {point.name}<br/>',
        'Average LOS: {point.x: .1f} years<br/>',
        'People Released: {point.people_released}<br/>'
      )
    ) |>
    hc_legend(verticalAlign = "top",
              layout = "horizontal") |>
    hc_colors(c(color1, color4, color2, color5))

  return(highcharts)
}
