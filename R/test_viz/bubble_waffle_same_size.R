# Common circle size in pixels
circle_radius <- 6  # Adjust this value to make circles larger or smaller
num_columns <- 50   # Fixed number of columns for layout

# Function to calculate the height of the chart based on the number of items and columns
calc_height <- function(num_items, columns, circle_radius) {
  num_rows <- ceiling(num_items / columns)
  return(num_rows * (circle_radius * 4))  # Adjust 4 as needed to increase spacing
}

# Black, non-Hispanic
df1 <- df2 |> filter(race == "Black, non-Hispanic")
rate_black <- df1$incarceration_rate[1]  # Adjust if there's more than one value
height_black <- calc_height(rate_black, num_columns, circle_radius)

hc_waffle_rri_black <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 Black, non-Hispanic people in the community, {rate_black} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:round(rate_black), function(i) {
        list(
          y = 1,
          marker = list(symbol = "circle", radius = circle_radius)
        )
      }),
      type = "item",
      marker = list(radius = circle_radius),
      layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_black / num_columns), columns = num_columns)
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_size(height = height_black) |>
  hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
  hc_colors(c(color1))
hc_waffle_rri_black

# Hispanic, any race
df1 <- df2 |> filter(race == "Hispanic, any race")
rate_hispanic <- df1$incarceration_rate[1]  # Adjust if there's more than one value
height_hispanic <- calc_height(rate_hispanic, num_columns, circle_radius)

hc_waffle_rri_hispanic <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 Hispanic people in the community, {rate_hispanic} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:round(rate_hispanic), function(i) {
        list(
          y = 1,
          marker = list(symbol = "circle", radius = circle_radius)
        )
      }),
      type = "item",
      marker = list(radius = circle_radius),
      layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_hispanic / num_columns), columns = num_columns)
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_size(height = height_hispanic) |>
  hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
  hc_colors(c(color4))
hc_waffle_rri_hispanic
