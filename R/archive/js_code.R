# JavaScript code for tooltip formatter with a close button
tooltip_formatter_js <- JS("function() {
  var point = this.point,
      chart = this.series.chart,
      tooltip = '<div id=\"custom-tooltip\" style=\"position: relative; padding: 10px;\">' +
                '<span style=\"position: absolute; top: 0; right: 0; cursor: pointer;\" onclick=\"closeTooltip(chart)\">X</span>' +
                point.tooltip +
                '</div>';
  return tooltip;
}")

# JavaScript function to close the tooltip
close_tooltip_js <- JS("function closeTooltip(chart) {
  chart.tooltip.hide();
}")

# Custom click function to toggle tooltip display
click_event_js <- JS("function() {
  var point = this;
  if (point.series.chart.tooltip.isHidden) {
    point.series.chart.tooltip.refresh(point);
  } else {
    point.series.chart.tooltip.hide();
  }
}")


# JavaScript code to hide the tooltip on creation to prevent it from showing on hover
no_hover_js <- JS("function () { this.update({ tooltip: { enabled: false }}); }")

# JavaScript code for a custom click function that shows the tooltip
click_function_js <- JS("function () {
  // Enable the tooltip
  this.series.chart.update({ tooltip: { enabled: true }});
  // Show the tooltip
  this.series.chart.tooltip.refresh(this);
}")

# JavaScript code to add a close button to the tooltip
tooltip_formatter_js <- JS("function () {
  return '<div style=\"text-align: center; padding: 10px;\">' +
         '<span style=\"font-size: 10px; float: right; margin-left: 5px; cursor: pointer;\" ' +
         'onclick=\"var chart = Highcharts.charts[0]; chart.tooltip.hide(); return false;\">' +
         'Close' +
         '</span>' +
         this.point.tooltip +
         '</div>';
}")



####################
# Map (Percent)
####################

# parole_info_by_state (which states abolished parole release) was imported in import.R
map_data <- parole_eligibility_table_select_year %>%

  # add missing states
  complete(state = all_states) %>%

  # add info about whether state abolished parole release
  left_join(parole_info_by_state_clean, by = "state") %>%

  # format data and create tooltip
  mutate(current_perc           = current_perc*100,
         future_1_5_years_perc  = future_1_5_years_perc*100,
         missing_perc           = missing_perc*100,

         state_abb = state.abb[match(state, state.name)],

         # determine if the state has all missing data
         all_na = rowSums(is.na(select(.,
                                       current_count,
                                       future_1_5_years_count,
                                       missing_count))) ==
           length(select(.,
                         current_count,
                         future_1_5_years_count,
                         missing_count)),

         # create tooltip depending on state info
         tooltip = case_when(all_na == TRUE & abolished_discretionary_parole == "No" ~
                               paste0("<b>", state, "</b><br><br>",
                                      "Parole eligibility data is not available.<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(yearendpop, digits = 0), "</b>"),

                             all_na == TRUE & abolished_discretionary_parole == "Yes" ~
                               paste0("<b>", state, "</b><br><br>",
                                      state, " abolished discretionary parole.<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(yearendpop, digits = 0), "</b>"),

                             all_na == FALSE & abolished_discretionary_parole == "Yes" ~
                               paste0("<b>", state, "</b><br><br>",
                                      state, " abolished discretionary parole.<br><br>",
                                      "Number of People in Prison<br> Currently Eligible for Release:<br><b>",
                                      paste(formattable::comma(current_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Number of People in Prison<br> Eligible for Release in the Next 1-5 Years:<br><b>",
                                      paste(formattable::comma(future_1_5_years_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Number of People in Prison<br> with Missing Release Eligibility Data:<br><b>",
                                      paste(formattable::comma(missing_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(yearendpop, digits = 0), "</b>"),

                             all_na == FALSE & abolished_discretionary_parole == "No" ~
                               paste0("<b>", state, "</b><br><br>",
                                      "Number of People in Prison<br> Currently Eligible for Parole Release:<br><b>",
                                      paste(formattable::comma(current_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Number of People in Prison<br> Eligible for Parole Release in the Next 1-5 Years:<br><b>",
                                      paste(formattable::comma(future_1_5_years_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Number of People in Prison<br> with Missing Parole Eligibility Data:<br><b>",
                                      paste(formattable::comma(missing_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(yearendpop, digits = 0), "</b>")),

         tooltip = str_replace_all(tooltip, "NA%", "No Data"),
         tooltip = str_replace_all(tooltip, "NA", "No Data")) %>%

  # create data labels
  mutate(change_label = paste0(round(current_perc, 0), "%"),
         change_label = str_replace_all(change_label, "NA%", "-"),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))

# Define the gradient colors for categories
gradient_colors <- c("#D5F5F3", "#6AD0C9", "#00ABA0", "#006F8A", "#003474")

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data %>%
  mutate(
    all_na = rowSums(is.na(select(., current_count, future_1_5_years_count, future_6_years_count))) ==
      length(select(., current_count, future_1_5_years_count, future_6_years_count)),
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) %>%
  group_by(gradient_color) %>%
  mutate(
    data_category = case_when(
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & all_na & abolished_discretionary_parole == "No" ~ "Missing Data",
      is.na(data_category) & abolished_discretionary_parole == "Yes" ~ "Abolished Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ "#e8e8e8",
      is.na(gradient_color) & data_category == "Abolished Discretionary Parole" ~ "#ffaf00",
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "Abolished Discretionary Parole" ~ 5,
      abolished_discretionary_parole == "Yes" ~ 5,
      TRUE ~ data_category_num
    )
  )

# create hex map
map_percent <- highchart(height = 600) %>%

  hc_chart(marginTop = 60,
           marginBottom = 50,
           marginRight = 50) %>%

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.change_label + '</span>' + '</div>';}")),
    nullColor = "#e8e8e8",
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}"))) %>%

  hc_colorAxis(dataClassColor="category",
               dataClasses = list(list(from = 1, to = 1, color="#D5F5F3", name = "0 - 3%"),
                                  list(from = 2, to = 2, color="#6AD0C9", name = "4 - 12%"),
                                  list(from = 3, to = 3, color="#00ABA0", name = "13 - 20%"),
                                  list(from = 4, to = 4, color="#006F8A", name = "21 - 39%"),
                                  list(from = 5, to = 5, color="#ffaf00", name = "Abolished Discretionary Parole"),
                                  list(from = 6, to = 6, color="#e8e8e8", name = "Missing Data")
               )) %>%

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 10,
            y = -40) %>%

  hc_xAxis(title = "") %>%
  hc_yAxis(title = "") %>%

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
  ) %>%

  hc_add_theme(hc_theme_map) %>%

  hc_plotOptions(
    series = list(
      animation = FALSE,
      cursor = "pointer",
      borderWidth = 3,
      point = list(
        events = list(
          click = JS("function() { this.series.chart.tooltip.refresh(this); }")
        )
      ),
      states = list(
        hover = list(
          stickyTracking = FALSE
        )
      )
    ),
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(
        enabled = TRUE
      ),
      linkedDescription = paste0("TEXT"),
      landmarkVerbosity = "one"
    ),
    area = list(
      accessibility = list(
        description = paste0("TEXT")
      )
    )
  )

map_percent
