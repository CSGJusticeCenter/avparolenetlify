# ---------------------------------------------------------------------------- #
# Analysis Helper Functions
# ---------------------------------------------------------------------------- #

#' Filter Prison Population Based on Parole Eligibility Criteria
#'
#' This function filters the prison population data to include only individuals
#' who meet specific criteria related to admission type and sentence length.
#' When analyzing people in prison past parole eligibility, we are interested in
#' people in prison for new crimes (not parole revocations) and with sentence lengths
#' of more than 1 year but not life. This function also excludes states with
#' high missingness or abolished parole systems and skips filtering for states
#' that don't require these criteria.
#'
#' @param data A data frame containing prison population data to be filtered.
#' @param exclude A data frame or vector containing states to exclude due to high missingness or abolished parole systems.
#' @param dont_filter A data frame or vector containing states that don't require filtering for admission type or sentence length.
#' @return A filtered data frame that includes only individuals and states meeting the specified criteria.
#' @details
#' - Excludes states in the `exclude` list.
#' - Filters individuals based on `admtype` ("New court commitment") and `sentlgth` (1+ years, except life),
#'   unless the state is in the `dont_filter` list.
#' @examples
#' filtered_data <- fnc_filter_pe_population_criteria(data, states_to_exclude, states_nofilter)
#' @export
fnc_filter_pe_population_criteria <- function(data, exclude, dont_filter) {
  # Extract the list of states to exclude (e.g., due to missing data or abolished parole)
  exclude <- exclude |> pull(state)

  # Extract the list of states that don't need filtering for admission type or sentence length
  dont_filter <- dont_filter |> pull(state)

  # Apply filtering criteria to the data
  # 1. Exclude states in the `exclude` list
  # 2. For other states:
  #    - Filter individuals with "New court commitment" as `admtype`
  #    - Include only those with sentence lengths of 1+ years, excluding life sentences
  #    - Skip filtering entirely for states in the `dont_filter` list
  filtered_data <- data |>
    filter(!(state %in% exclude)) |> # Exclude states with missing data or no parole system
    filter(
      (state %in% dont_filter) | # Include states in dont_filter without further filtering
        # (admtype == "New court commitment" & # Filter for "New court commitment" admission type
        #    sentlgth %in% c("1-1.9 years", "2-4.9 years", # Include specific sentence length categories
        #                    "5-9.9 years", "10-24.9 years", ">=25 years"))
        !(admtype %in% c("Other", "Parole return/revocation") | is.na(admtype) | admtype == "Unknown") &
        !(sentlgth %in% c("< 1 year", "Life, LWOP, Life plus additional years, Death") | is.na(sentlgth) | sentlgth == "Unknown")
    )

  # Return the filtered dataset
  return(filtered_data)
}

#' Filter Data by Year Based on State-Specific Year Selection
#'
#' Filters a data frame to include only rows where the reporting year (`rptyear`) matches
#' the year determined to be most reliable (`year_to_use`) for each state.
#'
#' @param df A data frame containing data to be filtered.
#' @param which_state_year A data frame containing state-year mapping with columns:
#'   - `state`: State name or code.
#'   - `year_to_use`: The year to use for filtering data for each state.
#' @return A filtered data frame containing only rows where `rptyear` matches `year_to_use`.
#' @details
#' - Joins the input data with the `which_state_year` data frame to add the `year_to_use` column.
#' - Filters the input data to include only rows where `rptyear` equals `year_to_use`.
#' @examples
#' filtered_data <- fnc_filter_by_year(data, state_year_mapping)
#' @export
fnc_filter_by_year <- function(df, which_state_year) {
  df |>
    # Join the input data with `which_state_year` to add the `year_to_use` column
    left_join(which_state_year, by = "state") |>
    # Filter rows where the reporting year (`rptyear`) matches the selected year (`year_to_use`)
    filter(rptyear == year_to_use)
}

#' Create Tooltip for Highcharts Visualization
#'
#' Generates a tooltip column for a data frame, containing formatted text for use
#' in Highcharts visualizations. The tooltip includes variable labels, the variable value,
#' the number of people, and their percentage representation.
#'
#' @param df A data frame containing the data to which tooltips will be added.
#' @param variable_label A string representing the label to display in the tooltip for the variable.
#' @param variable The variable whose values will appear in the tooltip.
#' @return A modified data frame with a new `tooltip` column containing the formatted tooltip text.
#' @details
#' - The tooltip text includes the variable label, variable value, number of people (formatted with commas),
#'   and the percentage of people (rounded to the nearest whole number).
#' - Tooltips are designed for use with Highcharts data visualizations.
#' @examples
#' tooltip_df <- fnc_create_tooltip(data, "Parole Eligibility Status", parelig_status)
#' @export
fnc_create_tooltip <- function(df, variable_label, variable) {
  df |>
    dplyr::mutate(
      tooltip = paste0(
        "<b>", variable_label, ":</b> ", {{ variable }}, "<br>", # Add variable label and value
        "<b>People:</b> ", formattable::comma(n, 0), "<br>", # Add number of people with comma formatting
        "<b>Percentage of People:</b> ", round(prop, 0), "%" # Add percentage of people rounded to whole number
      )
    )
}








# ---------------------------------------------------------------------------- #
# Visualization Styles and Helper Functions
# ---------------------------------------------------------------------------- #

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
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "lightgray", # Color of the gridlines
    gridLineWidth = 0.5          # Width of the gridlines
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

# ---------------------------------------------------------------------------- #
# Highcharter Helper Functions
# ---------------------------------------------------------------------------- #

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

#' Add Logo and Export Options to a Highcharter Chart
#'
#' This function enhances a Highcharter chart by adding export options (e.g., PNG download) and a custom logo.
#' It configures chart exporting dependencies, style settings, and bottom margin adjustments.
#'
#' @param hc A Highcharter object. The chart to which export options and a logo will be added.
#' @param title A string. The filename to use when exporting the chart.
#' @param bottom_margin_value A numeric value. The bottom margin size for the chart in pixels.
#'
#' @return A modified Highcharter object with export and styling options applied.
#'
#' @examples
#' library(highcharter)
#' hc <- highchart() |>
#'   hc_add_series(name = "Sample", data = c(1, 2, 3))
#' hc <- fnc_add_logo_and_export(hc, title = "my_chart", bottom_margin_value = 50)
#'
#' @export
fnc_add_logo_and_export <- function(hc, title, bottom_margin_value) {###############################################################################################################
  hc |>
    hc_add_dependency(name = "modules/exporting.js") |>
    hc_add_dependency(name = "modules/offline-exporting.js") |>
    hc_exporting(
      filename = title,
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("downloadPNG"))),
      chartOptions = list(
        chart = list(
          style = list(
            style = list(fontFamily = "Helvetica, sans-serif")
          ),
          events = list(
            load = render_image
          )
        )
      )
    ) |>
    hc_chart(
      marginBottom = bottom_margin_value
    )
}

#' Create Highcharts Pie Chart
#'
#' Generates Highcharts pie charts for each state in the input data frame, visualizing
#' the distribution of a given variable such as parole eligibility status.
#'
#' @param df A data frame containing data for multiple states.
#' @param variable The variable to visualize in the pie chart (e.g., "parelig_status").
#' @param source A string providing the source information for the chart caption (default: `ncrp_csg_source`).
#' @return A named list of Highcharts objects, one for each state in the data frame.
#' @details
#' - Iterates over states in the data frame and creates a pie chart for each.
#' - Adds accessibility text to describe the chart for screen readers.
#' - Outputs charts with exporting options enabled for saving.
#' @export
fnc_hc_pie_chart <- function(df, variable, source1 = ncrp_source, source2 = csg_source) {
  # Get unique states from the data
  states <- unique(df$state)

  # Iterate over each state to generate pie charts
  all_pie_charts <- map(states, function(state_name) {
    # Filter the data for the current state
    df1 <- df |>
      ungroup() |> # Remove grouping to ensure accurate filtering
      filter(state == state_name) |> # Select data for the current state
      mutate(color = case_when( # Assign colors based on parole eligibility status
        parelig_status_new == "Will Be Eligible In 1+ Year" ~ color2,
        parelig_status_new == "Will Be Eligible Next Year" ~ color3,
        parelig_status_new == "Missing Data or Possibly Never Eligible" ~ darkgray,
        parelig_status_new == "Past Parole Eligibility at End of Year" ~ color4
      ))

    # Extract the reporting year for the current state (assumes it's consistent within the state)
    year <- unique(df1$rptyear)

    # Generate descriptive accessibility text for the pie chart
    category_counts <- df1 |>
      group_by(!!sym(variable)) |> # Group by the specified variable
      # Calculate percentage for each category
      summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |>
      arrange(desc(percentage)) # Sort categories by descending percentage

    # Build a textual description of the chart for accessibility
    accessibility_text <- paste(
      "This pie chart shows the distribution of the prison population by", variable, "in", year, ".",
      paste(
        category_counts |>
          # Combine category and percentage
          transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |>
          pull(text), # Extract the formatted text
        collapse = ", " # Join all categories into a single string
      )
    )

    # Generate title of chart
    download_title <- paste0("prison_pop_by_parelig_status_", state_name, "_", year)
    bottom_margin_value <- 120

    # Create the Highcharts pie chart
    highchart() |>
      hc_chart(type = "pie") |>
      hc_plotOptions(pie = list(
        dataLabels = list( # Define label formatting for the chart
          enabled = TRUE,
          format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span>
          <br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
        ),
        # Use custom colors defined in the data
        colorByPoint = FALSE
      )) |>
      hc_series(list(
        # Add data to the chart
        data = list_parse(df1 |> mutate(y = n) |> transmute(
          name = !!sym(variable), y, color, tooltip
        ))
      )) |>
      hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |>
      hc_title(text = "Prison Population by Parole Eligibility Status") |>
      hc_caption(text = paste0(source1, ", ", year, " and ", source2),
                 y = -40) |>
      fnc_add_hc_accessibility(accessibility_text) |>
      fnc_add_logo_and_export(download_title, bottom_margin_value) |>  # Add logo and export options
      hc_add_theme(base_hc_theme)
  })

  # Assign state names to the charts list for clarity
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
}
