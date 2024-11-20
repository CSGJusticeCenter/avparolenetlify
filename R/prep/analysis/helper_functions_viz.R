
# ---------------------------------------------------------------------------- #
# Clean
# ---------------------------------------------------------------------------- #

# Function to create Highcharts pie charts for each state
fnc_hc_pie_chart <- function(df, variable, source = ncrp_csg_source) {
  # Get unique states from the data
  states <- unique(df$state)

  # Iterate over each state to generate charts
  all_pie_charts <- map(states, function(state_name) {
    # Filter data for the current state
    df1 <- df |>
      ungroup() |>
      filter(state == state_name) |>
      mutate(color = case_when(
        parelig_status == "Future" ~ color2,
        parelig_status == "Missing" ~ darkgray,
        parelig_status == "Current" ~ color4
      ))

    # Extract `rptyear` for the current state (assuming it's consistent)
    select_year <- unique(df1$rptyear)

    # Generate descriptive accessibility text
    category_counts <- df1 |>
      group_by(!!sym(variable)) |>
      summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |>
      arrange(desc(percentage))

    accessibility_text <- paste(
      "This pie chart shows the distribution of the prison population by", variable, "in", select_year, ".",
      paste(
        category_counts |>
          transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |>
          pull(text),
        collapse = ", "
      )
    )

    # Create the Highchart pie chart
    highchart() |>
      hc_chart(type = "pie") |>
      hc_plotOptions(pie = list(
        dataLabels = list(
          enabled = TRUE,
          format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
        ),
        colorByPoint = FALSE
      )) |>
      hc_series(list(
        data = list_parse(df1 |> mutate(y = n) |> transmute(
          name = !!sym(variable), y, color, tooltip
        ))
      )) |>
      hc_add_theme(base_hc_theme) |>
      hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |>
      hc_title(text = paste0("Prison Population by Parole Eligibility Status, ", select_year)) |>
      hc_exporting(enabled = TRUE, filename = paste0("prison_population_", state_name, "_", select_year)) |>
      hc_caption(text = source) |>
      fnc_add_hc_accessibility(accessibility_text)
  })

  # Assign state names to the charts list
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
}

#' Add Accessibility to Highcharts Object
#'
#' Adds accessibility options to a Highcharts object, including keyboard navigation
#' and a descriptive label for screen readers.
#'
#' @param hc_object A Highcharts object to which accessibility features will be added.
#' @param accessibility_text A string of text used for accessibility descriptions.
#'
#' @return A Highcharts object with accessibility options enabled.
#' @export
fnc_add_hc_accessibility <- function(hc_object, accessibility_text) {
  hc_object |>
    hc_chart(accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      description = accessibility_text,
      landmarkVerbosity = "one"
    )) |>
    hc_plotOptions(series = list(
      animation = FALSE,
      cursor = "pointer",
      borderWidth = 3,
      minPointLength = 4,
      accessibility = list(
        description = accessibility_text
      )
    ))
}

#' Common Style Elements
#'
#' This list defines the common style elements used across different themes,
#' including font family, color, font size, and font weight.
#'
#' @return A list of common style elements to maintain consistent appearance across visualizations.
#' @export
common_style <- list(
  fontFamily = "Graphik",
  color = "black",
  fontSize = "14px",
  fontWeight = "regular"
)

#' Common Chart Style
#'
#' This list defines the common chart style elements used across different themes,
#' specifically for chart text formatting.
#'
#' @return A list of common chart style elements for Highcharts.
#' @export
common_chart_style <- list(
  fontFamily = "Graphik",
  fontSize = "14px",
  color = "black"
)

#' Common Title Style
#'
#' This list defines the common title style elements, including the font family,
#' weight, and color, ensuring consistency across chart titles.
#'
#' @return A list of common title style elements for charts.
#' @export
common_title_style <- list(
  fontFamily = "Graphik",
  fontWeight = "bold",
  color = "black"
)

#' Base Highcharts Theme
#'
#' This theme serves as the base for other themes in Highcharts.
#' It sets common styling elements like colors, chart layout, axis labels,
#' legend positioning, and data label styling.
#' @export
base_hc_theme <- hc_theme(
  colors = c(color1, color2, color3, color4, color5),
  chart = list(style = common_chart_style),
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = common_style
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style
    ),
    gridLineColor = "transparent",
    lineColor = "black",
    minorGridLineColor = "transparent",
    tickColor = "black"
  ),
  yAxis = list(
    labels = list(enabled = FALSE,
                  style = common_style
    ),
    gridLineColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  plotOptions = list(
    series = list(
      events = list(
        legendItemClick = JS("function() { return false; }")  # Disables clicking on legend items
      )
    ),
    column = list(
      dataLabels = list(
        style = common_style
      )
    )
  ),
  caption = list(
    align = "left",
    style = list(
      fontSize = "10px",
      color = "#555555"
    )
  ),
  exporting = list(
    buttons = list(
      contextButton = list(
        menuItems = list(
          "downloadPNG"
        )
      )
    )
  )
)

#' Highcharts Theme with Line Chart Support
#'
#' Custom Highcharts theme that builds on the base theme, adding specific support for line charts.
#'
#' @return A Highcharts theme configuration object.
#' @export
hc_theme_with_line <- hc_theme(
  colors = c(color1, color2, color3, color4, color5),
  chart = list(style = common_chart_style),
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(align = "center", verticalAlign = "top", itemStyle = common_style),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    tickmarkPlacement = 'on',
    tickLength = 5,
    tickWidth = 1,
    tickColor = "white",
    lineColor = "black"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = common_style)
  ),
  caption = list(
    align = "left",
    style = list(
      fontSize = "10px",
      color = "#555555"
    )
  ),
  plotOptions = list(
    column = list(
      dataLabels = list(
        style = list(color = "black")
      )
    )
  )
)

#' Highchart Labels JS Code
#'
#' JavaScript code that splits long labels (over 23 characters) into multiple rows
#' to prevent labels from being cut off.
#'
#' @return A JavaScript function to split long labels into multiple rows in Highcharts.
js_code <- "function() {
                    var label = this.value;
                    var maxLength = 23;
                    if (label.length > maxLength) {
                      var words = label.split(' ');
                      var result = [];
                      var line = [];
                      var lineLength = 0;

                      words.forEach(function(word) {
                        if (lineLength + word.length > maxLength) {
                          result.push(line.join(' '));
                          line = [];
                          lineLength = 0;
                        }
                        line.push(word);
                        lineLength += word.length + 1;
                      });
                      if (line.length > 0) {
                        result.push(line.join(' '));
                      }
                      return result.join('<br>');
                    } else {
                      return label;
                    }
                  }"


fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
                               source = ncrp_csg_source, orientation = "vertical") {

  df1 <- df |>
    filter(state == state_var) |>
    fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var))

  year <- unique(df1$rptyear)

  # Conditionally arrange by prop if x_var is "race", "fbi_index", or "sex"
  # Don't arrange if sentence length or age since these need to be in order
  if (x_var %in% c("race", "fbi_index", "sex")) {
    df1 <- df1 |> arrange(desc(prop))
  }

  title <- paste0(title_type, " by ", metric)

  accessibility_text <- paste0("This graph shows the percentage of ", type,
                               " by ", tolower(metric), " in ",
                               year, " in the state of ", state_var, ".")

  xaxis_order <- df1[[x_var]]

  # Determine the chart type based on the orientation parameter
  chart_type <- ifelse(orientation == "horizontal", "bar", "column")

  # Adjust label alignment based on orientation
  label_alignment <- ifelse(orientation == "horizontal", "right", "center")

  highcharts <- highchart() |>
    hc_add_series(df1,
                  type = chart_type,  # Use the determined chart type here
                  hcaes(x = !!sym(x_var),
                        y = !!sym(y_var)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "14px",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) |>
    hc_xAxis(categories = xaxis_order,
             labels = list(
               useHTML = TRUE,
               enabled = TRUE,
               formatter = JS(js_code),
               style = list(fontSize = "14px", fontFamily = "Graphik",
                            textAlign = label_alignment)  # Adjust alignment here
             )) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() { return this.value + '%'; }")
             )) |>

    hc_add_theme(base_hc_theme) |>

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>

    hc_legend(enabled = FALSE) |>

    hc_title(text = paste0(title, ", ", year)) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", year)) |>

    fnc_add_hc_accessibility(accessibility_text) |>

    hc_caption(text = source)

  return(highcharts)
}

































# ---------------------------------------------------------------------------- #
# Visualization Helper Functions
# ---------------------------------------------------------------------------- #



