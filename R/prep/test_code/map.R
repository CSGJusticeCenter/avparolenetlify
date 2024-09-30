map_data <- map_data_breaks |>
  select(state, state_abb, current_perc, change_label, abolished_discretionary_parole)

# Define the gradient colors for categories
gradient_colors <- c(green1, green2, green3, green4, blue)

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) |>
  group_by(gradient_color) |>
  mutate(
    data_category = case_when(
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & abolished_discretionary_parole == "No" ~ "Missing Data",
      is.na(data_category) & abolished_discretionary_parole == "Yes" ~ "Abolished Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ lightgray,
      is.na(gradient_color) & data_category == "Abolished Parole" ~ yellow,
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "Abolished Parole" ~ 5,
      TRUE ~ data_category_num
    )
  )

# create hex map
map_percent <- highchart(height = 600) |>

  hc_chart(marginTop = 60,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px; text-align:center;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px; text-align:center;\">' + this.point.change_label + '</span>' + '</div>';}"),
                      textOutline = "none",
                      y = 0),
    nullColor = lightgray,
    borderColor = "#FFFFFF",
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}"))) |>

  hc_colorAxis(dataClassColor="category",
               dataClasses = list(
                 list(from = 1, to = 1, color = green1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = green2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = green3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = green4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = yellow, name = "Abolished Parole"),
                 list(from = 6, to = 6, color = lightgray, name = "Missing Data")
               )) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 10,
            y = -40) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = 'rgba(255, 255, 255, 1)',
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          this.point.tooltip +
          '</div>';}"
    ),
    useHTML = TRUE
  ) |>

  hc_add_theme(hc_theme_map) |>

  hc_plotOptions(series = list(
    animation = FALSE,
    cursor = "pointer",
    borderWidth = 3,
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      pointDescriptionFormatter = JS("function(point) {
        return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
      }")
    )
  ),
  accessibility = list(
    enabled = TRUE,
    keyboardNavigation = list(enabled = TRUE),
    linkedDescription =
      paste0("This map shows the proportion of people in prison who are past their parole eligibility year."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description = paste0("TEXT")))
  ) |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

  hc_title(text = "People in Prison Past Their Parole Eligibility: 2023 Projections",
           align = "left") |>

  hc_exporting(
    enabled = TRUE,
    scale = 1,  # Ensure the exported image matches screen size exactly
    sourceWidth = 800,  # Set the width same as screen
    sourceHeight = 600,  # Set the height same as screen
    chartOptions = list(
      plotOptions = list(
        series = list(
          dataLabels = list(
            align = "center",
            verticalAlign = "middle",
            style = list(
              fontWeight = "bold",
              fontSize = "14px",
              textOutline = "none",
              align = "center"
            )
          )
        )
      )
    )
  )
map_percent

