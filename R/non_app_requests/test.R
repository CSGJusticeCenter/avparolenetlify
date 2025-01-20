render_image <- JS("
  function(){
    this.renderer.image('https://avparoleproject.netlify.app/img/csgjc-logo.png', 10, this.chartHeight - 0, 140.1, 30)
    .add();
  }")

# ---------------------------------------------------------------------------- #
# Visualization Styles and Helper Functions
# ---------------------------------------------------------------------------- #


#' Base Highcharts Theme
#'
#' This theme serves as the base for other themes in Highcharts.
#' It sets common styling elements like colors, chart layout, axis labels,
#' legend positioning, and data label styling.
#' @export
base_hc_theme <- hc_theme(
  colors = c(color1, color2, color3, color4, color5),
  chart = list(
    style = list(
      fontFamily = "Graphik",
      fontSize = "14px",
      color = "black"
    )
  ),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      fontSize = "16px",
      color = "black"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      fontSize = "14px",
      color = "black"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(
      fontFamily = "Graphik",
      fontSize = "14px",
      fontWeight = "regular",
      color = "black"
    )
  ),
  xAxis = list(
    labels = list(
      enabled = TRUE,
      style = list(
        fontFamily = "Graphik",
        fontSize = "14px",
        fontWeight = "regular",
        color = "black"
      )
    ),
    gridLineColor = "transparent",
    lineColor = "black",
    minorGridLineColor = "transparent",
    tickColor = "black"
  ),
  yAxis = list(
    labels = list(
      enabled = FALSE,
      style = list(
        fontFamily = "Graphik",
        fontSize = "14px",
        fontWeight = "regular",
        color = "black"
      )
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
        style = list(
          fontFamily = "Graphik",
          fontSize = "14px",
          fontWeight = "regular",
          color = "black"
        )
      )
    )
  ),
  caption = list(
    align = "left",
    style = list(
      fontSize = "10px",
      fontFamily = "Graphik",
      color = "#555555"
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
fnc_add_hc_accessibility1 <- function(hc_object, accessibility_text) {
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
#' hc <- fnc_add_logo_and_export1(hc, title = "my_chart", bottom_margin_value = 50)
#'
#' @export
fnc_add_logo_and_export1 <- function(hc, title, bottom_margin_value) {
  hc |>
    hc_add_dependency(name = "modules/exporting.js") |>
    hc_add_dependency(name = "modules/offline-exporting.js") |>
    hc_exporting(
      filename = title,
      enabled = TRUE,
      buttons = list(contextButton = list(menuItems = list("downloadPNG"))),
      chartOptions = list(
        chart = list(
          style = list(fontFamily = "Helvetica"),
          # marginBottom = bottom_margin_value, # Margin applied to the expor
          events = list(load = render_image)
        ),
        title = list(
          style = list(fontFamily = "Helvetica")
        ),
        subtitle = list(
          style = list(fontFamily = "Helvetica")
        ),
        caption = list(
          style = list(fontFamily = "Helvetica")
        )
      )
    ) |>
    hc_chart(
      marginBottom = bottom_margin_value,
      style = list(fontFamily = "Graphik")
    )
}

# ---------------------------------------------------------------------------- #
# Sentences and Visualization Helper Functions
# ---------------------------------------------------------------------------- #

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
fnc_hc_pie_chart1 <- function(df, variable, source1 = ncrp_source, source2 = csg_source, missing_data_df) {
  # Get unique states from the data
  states <- unique(df$state)
  states <- "Georgia"
  # Iterate over each state to generate pie charts
  all_pie_charts <- map(states, function(state_name) {
    # Filter the data for the current state
    df1 <- df |>
      ungroup() |> # Remove grouping to ensure accurate filtering
      filter(state == state_name) |> # Select data for the current state
      mutate(color = case_when( # Assign colors based on parole eligibility status
        parelig_status_new == "Will Be Eligible In 1+ Year" ~ color2,
        parelig_status_new == "Will Be Eligible Next Year" ~ color3,
        parelig_status_new == "Missing Data" ~ darkgray, # Adjusted for "Missing Data"
        parelig_status_new == "Missing, Possibly Due to Eligibility Rules" ~ darkgray, # Adjusted for "Missing, Possibly Due to Eligibility Rules"
        parelig_status_new == "Past Parole Eligibility at End of Year" ~ color4
      ))

    # Missing data text depending on state
    missing_data <- missing_data_df |>
      filter(state == state_name)

    missing_data_text <- missing_data |>
      mutate(missing_data_text = ifelse(
        missing_due_to_rules == 1,
        "Missing, Possibly Due to Eligibility Rules: This includes individuals for whom parole eligibility information is unavailable and could not be estimated. This could be because, due to the state's eligibility rules, they may have never been eligible, or because other data was also missing, such as admission year or maximum sentence length.",
        "Missing Data: This includes individuals for whom parole eligibility information is unavailable and could not be estimated due to other missing data, such as admission year or maximum sentence length."
      )) |>
      pull(missing_data_text)

    # Change missing data category depending on state
    df1 <- df1 |>
      mutate(parelig_status_new = if_else(missing_data$missing_due_to_rules == 0,
                                          "Missing Data", "Missing Data Due to Eligibility Rules"
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
      hc_caption(text = paste0("Source: ", source1, ", ", year, " and ", source2, ".<br>",
                               missing_data_text),
                 y = -30
                 ) |>
      fnc_add_hc_accessibility1(accessibility_text) |>
      hc_add_theme(base_hc_theme) |>
      fnc_add_logo_and_export1(download_title, bottom_margin_value)  # Add logo and export options
  })

  # Assign state names to the charts list for clarity
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
}

# Generate pie charts visualizing parole eligibility status proportions for each state
# `fnc_hc_pie_chart1` creates individual charts with data and accessibility text for each state
all_pie_pe_type <- fnc_hc_pie_chart1(
  df = pe_status_pop,
  variable = "parelig_status_new",
  missing_data_df = states_missing_data
)

all_pie_pe_type$Georgia

# Define the data objects and their corresponding file names
data_files <- list(
  all_pie_pe_type             = "all_pie_pe_type.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
