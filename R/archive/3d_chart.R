# Load the highcharter package
library(highcharter)

# Create a sample data frame
data <- data.frame(
  category = c("In Prison Past Their<br>Parole Eligibility Year",
               "Eligible in the Future",
               "Missing Data or<br>Not Parole-Eligible"),
  value = c(46, 29, 25)
)

# Colors for each bar
colors <- c(red, darkgray, darkgray)

# Create a 3D bar chart with adjustments
highchart() |>
  hc_chart(type = "column", options3d = list(
    enabled = TRUE,
    alpha = 15,
    beta = 15,
    viewDistance = 25
  )) |>
  hc_xAxis(categories = data$category,
           gridLineWidth = 0) |>
  hc_yAxis(
    title = list(text = ""),
    gridLineWidth = 0,
    labels = list(enabled = FALSE),
    minorGridLineWidth = 0,
    lineWidth = 0
  ) |>
  hc_zAxis(
    gridLineWidth = 0
  ) |>
  hc_plotOptions(column = list(
    depth = 250,
    groupPadding = 0,
    pointPadding = 0.05,
    colorByPoint = TRUE,
    dataLabels = list(
      enabled = TRUE,
      format = '{point.y}%',
      align = 'center',
      verticalAlign = 'top',
      style = list(
        fontSize = '1.5em',
        fontWeight = 'bold',
        color = 'white',
        textOutline = 'none'
      )
    )
  )) |>
  hc_add_series(data = data$value, name = "Values", showInLegend = FALSE, colors = colors) |> # Remove legend
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme)


