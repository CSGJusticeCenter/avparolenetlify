# Black, non-Hispanic
df1 <- df2 |> filter(race == "Black, non-Hispanic")
rate_black <- df1$incarceration_rate[1]  # Adjust if there's more than one value

hc_waffle_rri_black <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 Black, non-Hispanic people in the community, {rate_black} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "circle")
        )
      }),
      type = "item",
      size = '10%'
    )
  ) |>
  hc_legend(enabled = FALSE) |>
  hc_add_theme(base_hc_theme) |>
  hc_exporting(enabled = TRUE) |>
  hc_colors(c(color1))
hc_waffle_rri_black

# Hispanic, any race
df1 <- df2 |> filter(race == "Hispanic, any race")
rate_hispanic <- df1$incarceration_rate[1]  # Adjust if there's more than one value

hc_waffle_rri_hispanic <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 Hispanic people in the community, {rate_hispanic} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "circle")
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
hc_waffle_rri_hispanic

# Other race(s), non-Hispanic
df1 <- df2 |> filter(race == "Other race(s), non-Hispanic")
rate_other <- df1$incarceration_rate[1]  # Adjust if there's more than one value

hc_waffle_rri_other <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 non-Hispanic people of American Indian, Alaskan Native, Asian, Native Hawaiian, Pacific Islander, or other race and ethnicity in the community, {rate_other} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "circle")
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
hc_waffle_rri_other

# White, non-Hispanic
df1 <- df2 |> filter(race == "White, non-Hispanic")
rate_white <- df1$incarceration_rate[1]  # Adjust if there's more than one value

hc_waffle_rri_white <- highchart() |>
  hc_chart(type = "item") |>
  hc_title(text = glue("For every 100,000 non-Hispanic White people in the community, {rate_white} are in prison.")) |>
  hc_xAxis(categories = df1$race) |>
  hc_yAxis(title = list(text = "")) |>
  hc_series(
    list(
      name = "",
      data = lapply(1:nrow(df1), function(i) {
        list(
          y = df1$incarceration_rate[i],
          marker = list(symbol = "circle")
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
hc_waffle_rri_white

# # Create highcharts showing breakdown of parole-eligible prison population by sentlgth
# states <- unique(rri_data$state)
# all_hc_rri_chart <- map(.x = states,  .f = function(x) {
#   # Filter and prepare the sample data for Georgia
#   df1 <- rri_data %>%
#     ungroup() |>
#     filter(state == x) %>%
#     select(-state, -rri) %>%
#     mutate(incarceration_rate = round(incarceration_rate, 1)) %>%
#     mutate(color = case_when(
#       race == "Black, non-Hispanic" ~ color1,
#       race == "Hispanic, any race" ~ color2,
#       race == "Other race(s), non-Hispanic" ~ color3,
#       race == "White, non-Hispanic" ~ color4
#     ))
#
#   # Split the data by race/ethnicity
#   white_data <- df1 %>% filter(race == "White, non-Hispanic")
#   black_data <- df1 %>% filter(race == "Black, non-Hispanic")
#   hispanic_data <- df1 %>% filter(race == "Hispanic, any race")
#   other_data <- df1 %>% filter(race == "Other race(s), non-Hispanic")
#
#   # Define SVG icon for person representation
#   svg_person <- "M8 12s1.5-2 4-2 4 2 4 2-1.5 2-4 2-4-2-4-2zm6 3s2 1.5 2 4-2 2-2 2h-4s-2 0-2-2 2-4 2-4h4zM8 6s1.5 2 4 2 4-2 4-2-1.5-2-4-2-4 2-4 2z"
#
#   # Function to create highchart with SVG icon and fixed size
#   create_item_chart <- function(data, color) {
#     highchart() %>%
#       hc_chart(type = "item", marginTop = 80) %>%
#       hc_plotOptions(item = list(
#         marker = list(
#           symbol = svg_person,
#           lineWidth = 2,
#           lineColor = color,
#           states = list(
#             hover = list(
#               enabled = TRUE
#             )
#           )
#         )
#       )) %>%
#       hc_add_series(
#         name = "Incarceration Rate",
#         data = data$incarceration_rate_10
#       ) %>%
#       hc_add_theme(base_hc_theme) %>%
#       hc_legend(enabled = FALSE) %>%
#       hc_title(text = paste0("For every 100,000 ", data$race[1], " people in the community,<br>",
#                              data$incarceration_rate[1], " are in prison."))
#   }
#
#   # Create the charts for each racial/ethnic group
#   chart_white <- create_item_chart(white_data, color4) %>%
#     #hc_chart(marginLeft = 140, marginRight = 140) %>%
#     hc_colors(c(color4))
#
#   chart_black <- create_item_chart(black_data, color1) %>%
#     hc_colors(c(color1))
#
#   chart_hispanic <- create_item_chart(hispanic_data, color2) %>%
#     #hc_chart(marginLeft = 160, marginRight = 160) %>%
#     hc_colors(c(color2))
#
#   chart_other <- create_item_chart(other_data, color3) %>%
#     #hc_chart(marginLeft = 195, marginRight = 195) %>%
#     hc_colors(c(color3))
#
#   # Display the charts in a grid layout
#   hc_rri_chart <- hw_grid(chart_black, chart_hispanic, chart_white, chart_other, ncol = 2)
#   return(hc_rri_chart)
# })
# all_hc_rri_chart <- setNames(all_hc_rri_chart, states)
# all_hc_rri_chart$Georgia
