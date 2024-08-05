
# Black, non-Hispanic
df1 <- df2 |> filter(race == "Black, non-Hispanic")
rate_black <- df1$incarceration_rate[1]  # Adjust if there's more than one value

highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 10,000 Black, non-Hispanic people in the community, {rate_black} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "square")
        )
      }),
      type = "item",
      size = '100%'
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(color1))

# Hispanic, any race
df1 <- df2 |> filter(race == "Hispanic, any race")
rate_hispanic <- df1$incarceration_rate[1]  # Adjust if there's more than one value

highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 10,000 Hispanic people in the community, {rate_hispanic} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "square")
        )
      }),
      type = "item",
      size = '100%'
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(yellow))

# Other race(s), non-Hispanic
df1 <- df2 |> filter(race == "Other race(s), non-Hispanic")
rate_other <- df1$incarceration_rate[1]  # Adjust if there's more than one value

highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 10,000 non-Hispanic people of American Indian, Alaskan Native, Asian, Native Hawaiian, Pacific Islander, or other race and ethnicity in the community, {rate_other} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "square")
        )
      }),
      type = "item",
      size = '100%'
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(color4))

# Other race(s), non-Hispanic
df1 <- df2 |> filter(race == "White, non-Hispanic")
rate_white <- df1$incarceration_rate[1]  # Adjust if there's more than one value

highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 10,000 non-Hispanic White people in the community, {rate_white} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "square")
        )
      }),
      type = "item",
      size = '100%'
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(color2))

