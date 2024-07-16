library(highcharter)
library(dplyr)

gradient_colors <- c(colors$green1, colors$green2, colors$green3, colors$green4)

mydata <- map_data_breaks |>
  select(state, state_abb,current_perc, change_label, abolished_discretionary_parole) |>
  mutate(color = case_when(abolished_discretionary_parole == "Yes" ~ colors$yellow))

map_percent <- highchart() |>

  hc_add_series_map(
    map = hex_gj,
    df = mydata,
    joinBy = "state_abb",
    value = "current_perc",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.change_label + '</span>' + '</div>';}")),
    nullColor = colors$lightgray,
    borderColor = "#FFFFFF",  # Set the outline color to white
    borderWidth = 2,  # Set the outline width
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}")),
    point = list(events = list(
      click = JS("function() { window.location.assign(this.url); }")
    )
    )
  ) |>

  hc_add_theme(hc_theme_map) |>

  hc_colorAxis(min = 0, max = max(mydata$current_perc, na.rm = TRUE) * 1.2,
               stops = color_stops(n = 5, colors = gradient_colors),
               labels = list(
                 formatter = JS("function() { return this.value + '%'; }")
               )) |>

  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "horizontal",
    symbolWidth = 250,
    x = -7,
    title = list(text = "Pct. of People in Prison Past Their Parole Eligibility Date",
                 style = list(fontWeight = "regular",
                              fontSize = "12px"))
  )

map_percent

