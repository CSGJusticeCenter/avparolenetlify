# Sample data
years <- 2000:2024
values <- rnorm(25)

data <- data.frame(
  x = years,
  y = rep(1, 25),  # Only one row
  value = values
)

# Create heatmap
highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = "Time-based Heatmap") %>%
  hc_xAxis(categories = as.character(data$x)) %>%
  hc_yAxis(categories = c("Time")) %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_series(
    list(
      data = list_parse2(data %>% mutate(value = round(value, 2))),
      name = "Values"
    )
  ) %>%
  hc_tooltip(
    pointFormat = 'Year: {point.x}<br>Value: {point.value}'
  )
