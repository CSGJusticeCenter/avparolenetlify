library(highcharter)
library(dplyr)

# Example data
data <- data.frame(
  category = c("A", "B", "C", "D", "E", "F", "G"),
  value = c(10, 20, 30, 40, NA, 60, 70)
)

# Create a highchart object
hc <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_add_series(data = data %>% filter(!is.na(value)), type = "column", hcaes(x = category, y = value), name = "Value") %>%
  hc_add_series(data = data %>% filter(is.na(value)), type = "column", hcaes(x = category, y = 0), name = "No Data", color = "gray") %>%
  hc_colorAxis(
    min = min(data$value, na.rm = TRUE),
    max = max(data$value, na.rm = TRUE),
    stops = color_stops(5, colors = c("blue", "green", "yellow", "orange", "red")),
    type = "linear"
  ) %>%
  hc_legend(
    layout = "vertical",
    align = "right",
    verticalAlign = "middle",
    title = list(text = "Legend")
  ) %>%
  hc_add_series(name = "No Data", color = "gray", data = list(), showInLegend = TRUE, enableMouseTracking = FALSE)

# Display the chart
hc
