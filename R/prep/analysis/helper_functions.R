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
  #    - Skip filtering entirely for states in the `dont_filter` list --- removed for now but keep just in case
  filtered_data <- data |>
    filter(!(state %in% exclude)) |>
    filter(
        !(admtype %in% c("Other", "Parole return/revocation")) &
        !(sentlgth %in% c("< 1 year", "Life, LWOP, Life plus additional years, Death"))
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

#' Filter Out States with High Missing Race Data
#'
#' Excludes rows in the input data frame where the `state` column matches states
#' listed in `states_with_high_missing_race`. This ensures the data only includes states
#' with sufficient race data for analysis.
#'
#' @param data A data frame containing the data to be filtered, with a `state` column.
#' @param states_with_high_missing_race A character vector or list of state names
#'   (or codes) to exclude due to high missingness in race data.
#' @return A filtered data frame that excludes rows for states in `states_with_high_missing_race`.
#' @details
#' - Converts `states_with_high_missing_race` to a character vector if it's provided as a list.
#' - Prints the list of excluded states for debugging purposes.
#' @examples
#' filtered_data <- fnc_filter_exclude_high_missing_race(data, c("Georgia", "Alabama"))
#' @export
fnc_filter_exclude_high_missing_race <- function(data, states_with_high_missing_race) {
  # Convert `states_with_high_missing_race` to a character vector if it's provided as a list
  if (is.list(states_with_high_missing_race)) {
    states_with_high_missing_race <- unlist(states_with_high_missing_race)
  }

  # Debugging step: Print the list of states to be excluded for verification
  print("States with high missing race data:")
  print(states_with_high_missing_race)

  # Ensure that both the `state` column in `data` and `states_with_high_missing_race` are in the same format
  # Filter out rows where `state` matches any of the states in `states_with_high_missing_race`
  filtered_data <- data |>
    filter(!(state %in% states_with_high_missing_race))

  # Return the filtered data frame
  return(filtered_data)
}

#' Summarize Data with Counts and Proportions
#'
#' Summarizes the input data frame by grouping it by `state` and `rptyear`, and
#' calculates counts, proportions, and labels for a specified column. Optionally excludes
#' "Unknown" values for columns other than `race`.
#'
#' @param df A data frame containing the data to be summarized.
#' @param count_column A string representing the column name to group by and count.
#' @return A summarized data frame with the following columns:
#' - `state`: The state name or identifier.
#' - `rptyear`: The reporting year.
#' - `<count_column>`: The grouped column values.
#' - `n`: Count of observations for each group.
#' - `prop`: Proportion of observations for each group (in percentages).
#' - `n_total`: Total count for each state and reporting year.
#' - `prop_label`: Proportion formatted as a percentage label.
#' - `n_label`: Count formatted with commas for readability.
#' @details
#' - Filters out missing values (`NA`) from the specified column.
#' - Conditionally excludes "Unknown" values unless the column is `race`.
#' @examples
#' summarized_data <- fnc_summarize_data(data, "race")
#' @export
fnc_summarize_data <- function(df, count_column) {
  # Convert the string column name to a symbol for use in dplyr operations
  count_column <- sym(count_column)

  # Summarize the data, grouping by state and reporting year
  df1 <- df |>
    group_by(state, rptyear) |>

    # Filter out missing values and optionally exclude "Unknown" values
    # - Always exclude `NA`.
    # - Exclude "Unknown" unless the column is `race`.
    filter(
      !is.na(!!count_column) &                 # Exclude missing values
        (!(quo_name(count_column) != "race" &  # Check column name (string comparison)
             !!count_column == "Unknown"))     # Remove "Unknown" for non-"race" columns
    ) |>

    # Count occurrences of each value in the specified column
    count(!!count_column) |>

    # Calculate proportions and add formatted labels for visualization
    mutate(
      prop = (n / sum(n)) * 100,                # Calculate the proportion of each group
      n_total = sum(n),                         # Calculate the total count for the group
      prop_label = paste0(round(prop, 0), "%"), # Format proportion as a percentage string
      n_label = formattable::comma(n, 0)        # Format counts with commas
    ) |>
    ungroup() # Remove grouping for a flat data frame structure

  # Return the summarized data frame
  return(df1)
}

#' Group Offense Types into Broad Categories
#'
#' Categorizes offenses into broad groups such as "Violent," "Nonviolent," and "Other or Unknown"
#' based on the `fbi_index` column in the input data frame.
#'
#' @param data A data frame containing an `fbi_index` column with offense types.
#' @return A data frame with an additional column `offense_group`, categorizing the offenses.
#' @details
#' - The "Violent" group includes serious offenses such as murder, rape, and assault.
#' - The "Nonviolent" group includes drug, public order, and property offenses.
#' - Offenses not matching these groups are categorized as "Other or Unknown."
#' @examples
#' grouped_data <- fnc_group_offense_type(data)
#' @export
fnc_group_offense_type <- function(data) {
  data %>%
    # Add a new column `offense_group` based on the `fbi_index` offense type
    mutate(offense_group = case_when(
      # Categorize serious offenses as "Violent"
      fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                       "Negligent Manslaughter",
                       "Rape or Sexual Assault",
                       "Robbery",
                       "Aggravated or Simple Assault",
                       "Other Violent Offenses") ~ "Violent",

      # Categorize nonviolent offenses as "Nonviolent"
      fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",

      # Default category for unknown or uncategorized offenses
      TRUE ~ "Other or Unknown"
    ))
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
fnc_add_logo_and_export <- function(hc, title, bottom_margin_value, marginleft = NULL) {
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
          marginLeft = marginleft,
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
        ),
        xAxis = list(
          labels = list(style = list(fontFamily = "Helvetica")),
          title = list(style = list(fontFamily = "Helvetica"))
        ),
        yAxis = list(
          labels = list(style = list(fontFamily = "Helvetica")),
          title = list(style = list(fontFamily = "Helvetica"))
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
fnc_hc_pie_chart <- function(df, variable, source1 = ncrp_source, source2 = csg_source, missing_data_df) {
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
        parelig_status_new == "Missing Data" ~ darkgray, # Adjusted for "Missing Data"
        parelig_status_new == "Past Parole Eligibility at End of Year" ~ color4
      ))

    # Check if "Missing Data" is present in the current state's data
    include_missing_text <- "Missing Data" %in% df1$parelig_status_new

    # Generate missing data text only if "Missing Data" is present
    missing_data_text <- if (include_missing_text) {
      missing_data_df |>
        filter(state == state_name) |>
        mutate(missing_data_text = ifelse(
          missing_due_to_rules == 1,
          "Missing, Possibly Due to Eligibility Rules: This includes individuals for whom parole eligibility information is unavailable and could not be estimated. This could be because, due to the state's eligibility rules, they may have never been eligible, or because other data was also missing, such as admission year or maximum sentence length.",
          "Missing Data: This includes individuals for whom parole eligibility information is unavailable and could not be estimated due to other missing data, such as admission year or maximum sentence length."
        )) |>
        pull(missing_data_text)
    } else {
      NULL
    }

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
    download_title <- paste0("prison_pop_by_parelig_status_", year)
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
      hc_caption(
        text = paste0("Source: ", source1, ", ", year, " and ", source2, ".",
                      if (!is.null(missing_data_text)) paste0("<br>", missing_data_text)),
        y = -30
      ) |>
      fnc_add_hc_accessibility(accessibility_text) |>
      hc_add_theme(base_hc_theme) |>
      fnc_add_logo_and_export(download_title, bottom_margin_value)  # Add logo and export options
  })

  # Assign state names to the charts list for clarity
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
}
# fnc_hc_pie_chart <- function(df, variable, source1 = ncrp_source, source2 = csg_source, missing_data_df) {
#   # Get unique states from the data
#   states <- unique(df$state)
#   # Iterate over each state to generate pie charts
#   all_pie_charts <- map(states, function(state_name) {
#     # Filter the data for the current state
#     df1 <- df |>
#       ungroup() |> # Remove grouping to ensure accurate filtering
#       filter(state == state_name) |> # Select data for the current state
#       mutate(color = case_when( # Assign colors based on parole eligibility status
#         parelig_status_new == "Will Be Eligible In 1+ Year" ~ color2,
#         parelig_status_new == "Will Be Eligible Next Year" ~ color3,
#         parelig_status_new == "Missing Data" ~ darkgray, # Adjusted for "Missing Data"
#         parelig_status_new == "Past Parole Eligibility at End of Year" ~ color4
#       ))
#
#     # Missing data text depending on state
#     missing_data <- missing_data_df |>
#       filter(state == state_name)
#
#     missing_data_text <- missing_data |>
#       mutate(missing_data_text = ifelse(
#         missing_due_to_rules == 1,
#         "Missing, Possibly Due to Eligibility Rules: This includes individuals for whom parole eligibility information is unavailable and could not be estimated. This could be because, due to the state's eligibility rules, they may have never been eligible, or because other data was also missing, such as admission year or maximum sentence length.",
#         "Missing Data: This includes individuals for whom parole eligibility information is unavailable and could not be estimated due to other missing data, such as admission year or maximum sentence length."
#       )) |>
#       pull(missing_data_text)
#
#     # Extract the reporting year for the current state (assumes it's consistent within the state)
#     year <- unique(df1$rptyear)
#
#     # Generate descriptive accessibility text for the pie chart
#     category_counts <- df1 |>
#       group_by(!!sym(variable)) |> # Group by the specified variable
#       # Calculate percentage for each category
#       summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |>
#       arrange(desc(percentage)) # Sort categories by descending percentage
#
#     # Build a textual description of the chart for accessibility
#     accessibility_text <- paste(
#       "This pie chart shows the distribution of the prison population by", variable, "in", year, ".",
#       paste(
#         category_counts |>
#           # Combine category and percentage
#           transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |>
#           pull(text), # Extract the formatted text
#         collapse = ", " # Join all categories into a single string
#       )
#     )
#
#     # Generate title of chart
#     download_title <- paste0("prison_pop_by_parelig_status_", state_name, "_", year)
#     bottom_margin_value <- 120
#
#     # Create the Highcharts pie chart
#     highchart() |>
#       hc_chart(type = "pie") |>
#       hc_plotOptions(pie = list(
#         dataLabels = list( # Define label formatting for the chart
#           enabled = TRUE,
#           format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span>
#           <br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
#         ),
#         # Use custom colors defined in the data
#         colorByPoint = FALSE
#       )) |>
#       hc_series(list(
#         # Add data to the chart
#         data = list_parse(df1 |> mutate(y = n) |> transmute(
#           name = !!sym(variable), y, color, tooltip
#         ))
#       )) |>
#       hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |>
#       hc_title(text = "Prison Population by Parole Eligibility Status") |>
#       hc_caption(text = paste0("Source: ", source1, ", ", year, " and ", source2, ".<br>",
#                                missing_data_text),
#                  y = -30
#                  ) |>
#       fnc_add_hc_accessibility(accessibility_text) |>
#       hc_add_theme(base_hc_theme) |>
#       fnc_add_logo_and_export(download_title, bottom_margin_value)  # Add logo and export options
#   })
#
#   # Assign state names to the charts list for clarity
#   all_pie_charts <- setNames(all_pie_charts, states)
#
#   return(all_pie_charts)
# }

#' Generate Projection Sentence for Past Parole Eligibility Trends
#'
#' Creates a summary sentence describing trends in the percentage of people in prison
#' past parole eligibility for a given state, based on historical and projected data.
#'
#' @param state_name A string representing the state name.
#' @param data A data frame containing past and projected data for parole eligibility percentages,
#'   with columns such as `state`, `year`, `pct_past_pe`, `proj_pct_past_pe`, and `used_projected_flag`.
#' @return A string summarizing trends in past and projected parole eligibility percentages for the state.
#' @details
#' - Calculates the percentage change for both past and projected data.
#' - Includes a note if projections were used for 2019 or 2020.
#' - Handles missing or insufficient data gracefully, providing alternative text where necessary.
#' @examples
#' sentence <- fnc_generate_projection_sentence("Georgia", pe_proj_pop)
#' @export
fnc_generate_projection_sentence <- function(state_name, data) {
  # Filter data for the specified state
  state_data <- data |> filter(state == state_name)

  # Extract years with valid percent of people past parole eligibility and projected data
  valid_past_years <- state_data |> filter(!is.na(pct_past_pe)) |> pull(year)
  valid_proj_years <- state_data |> filter(!is.na(proj_pct_past_pe)) |> pull(year)

  # Determine earliest and latest years for original and projected data
  earliest_year_past <- min(valid_past_years, na.rm = TRUE) # First year with valid past data
  latest_year_past <- max(valid_past_years, na.rm = TRUE) # Last year with valid past data
  earliest_year_proj <- if (length(valid_proj_years) > 0) min(valid_proj_years, na.rm = TRUE) else NA # First projection year
  latest_year_proj <- if (length(valid_proj_years) > 0) max(valid_proj_years, na.rm = TRUE) else NA # Last projection year

  # Extract percentage values for the earliest and latest past years
  pct_earliest <- state_data |> filter(year == earliest_year_past) |> pull(pct_past_pe)
  pct_latest <- state_data |> filter(year == latest_year_past) |> pull(pct_past_pe)

  # Calculate the percentage change for past data (if available)
  change_past <- if (!is.na(pct_earliest) && !is.na(pct_latest)) {
    round(((pct_latest - pct_earliest) / pct_earliest) * 100, 0)
  } else NA

  # Extract percentage values for the earliest and latest projected years
  proj_earliest <- if (!is.na(earliest_year_proj)) state_data |> filter(year == earliest_year_proj) |> pull(proj_pct_past_pe) else NA
  proj_latest <- if (!is.na(latest_year_proj)) state_data |> filter(year == latest_year_proj) |> pull(proj_pct_past_pe) else NA

  # Calculate the percentage change for projected data (if available)
  change_proj <- if (!is.na(pct_latest) && !is.na(proj_latest)) {
    round(((proj_latest - pct_latest) / pct_latest) * 100, 0)
  } else NA

  # Generate a note if projections were used for specific years
  note <- case_when(
    state_data |> filter(year == 2019) |> pull(used_projected_flag) ~ " Note: 2019 data uses projections.",
    state_data |> filter(year == 2020) |> pull(used_projected_flag) ~ " Note: 2020 data uses projections.",
    TRUE ~ ""
  )

  # Construct the summary sentence
  sentence <- paste0(
    "From ", earliest_year_past, " to ", latest_year_past,
    ", the percent of people in prison past parole eligibility ",
    if (!is.na(change_past)) {
      if (change_past > 0) paste0("increased by ", change_past, " percent. ")
      else if (change_past < 0) paste0("decreased by ", abs(change_past), " percent. ")
      else "remained the same. "
    } else "has insufficient data to determine a change. ",
    if (!is.na(earliest_year_proj) && !is.na(latest_year_proj)) {
      paste0(
        "Our projection model estimated that the percent of people past their initial parole eligibility ",
        if (!is.na(change_proj)) {
          if (change_proj > 0) paste0("increased by ", change_proj, " percent from ", latest_year_past, " to ", latest_year_proj)
          else if (change_proj < 0) paste0("decreased by ", abs(change_proj), " percent from ", latest_year_past, " to ", latest_year_proj)
          else paste0("remained around ", round(proj_latest, 0), " percent from ", latest_year_past, " to ", latest_year_proj)
        } else "has insufficient data to project a change",
        "."
      )
    } else "Projected data is insufficient to provide a future change.",
    note
  )

  return(sentence)
}

#' Create Highcharts Column or Bar Chart
#'
#' Generates a Highcharts column or bar chart for a specific state, visualizing
#' metrics like percentages or proportions by a given variable.
#'
#' @param state_var The state for which the chart is being created.
#' @param df A data frame containing the data for multiple states.
#' @param x_var The variable to use on the x-axis.
#' @param y_var The variable to use on the y-axis (e.g., percentages or proportions).
#' @param metric A label for the variable being visualized (e.g., "Race").
#' @param type The type of data being visualized (e.g., "Releases").
#' @param title_type A title prefix for the chart (e.g., "Prison Population").
#' @param source A string providing the source information for the chart caption (default: `ncrp_csg_source`).
#' @param orientation The orientation of the chart ("vertical" for column, "horizontal" for bar).
#' @return A Highcharts object visualizing the data for the specified state.
#' @details
#' - Adjusts orientation and label alignment based on the `orientation` parameter.
#' - Includes accessibility text and exporting functionality.
#' @export
# fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
#                                source1, source2 = NULL,
#                                orientation = "vertical") {
#
#   # Filter the data for the specified state
#   df1 <- df |>
#     filter(state == state_var) |> # Filter by state
#     fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var)) # Add tooltips for better interactivity
#
#   # Extract the reporting year for the state
#   year <- unique(df1$rptyear)
#
#   # Conditionally arrange data by proportions for certain variables
#   if (x_var %in% c("race", "fbi_index", "sex")) {
#     df1 <- df1 |> arrange(desc(prop)) # Arrange by descending proportions
#   }
#
#   # Construct the chart title
#   title <- paste0(title_type, " by ", metric)
#
#   # Generate accessibility text describing the chart
#   accessibility_text <- paste0("This graph shows the percentage of ", type,
#                                " by ", tolower(metric), " in ",
#                                year, " in the state of ", state_var, ".")
#
#   # Download file title
#   download_title <- paste0(gsub(" ", "_", tolower(title)), "_", year)
#
#   # Define the x-axis order based on the data
#   xaxis_order <- df1[[x_var]]
#
#   # Determine chart type based on orientation
#   chart_type <- ifelse(orientation == "horizontal", "bar", "column")
#
#   # Adjust label alignment for horizontal orientation
#   label_alignment <- ifelse(orientation == "horizontal", "right", "center")
#
#   # Check if "Other race(s), non-Hispanic" exists in the x-axis variable
#   other_race_note <- if (x_var == "race" && any(df1[[x_var]] == "Other race(s), non-Hispanic")) {
#     "<br><br>According to the NCRP, the “Other race(s)” category may include American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, and individuals identifying as more than one race."
#   } else {
#     ""
#   }
#
#   # Determine caption_y based on x_var
#   caption_y <- if (x_var == "race") {
#     -30
#   } else if (x_var %in% c("fbi_index")) {
#     -30
#   } else if (x_var %in% c("sentlgth") & type == "the prison population") {
#     -20
#   } else {
#     -30 # Default space for other variables
#   }
#
#   # Space below chart to accompany logo
#   bottom_margin_value <- if (x_var == "race") {
#     160
#   } else if (x_var %in% c("fbi_index")) {
#     100
#   } else if (x_var %in% c("sentlgth") & type == "the prison population") {
#     130
#   } else {
#     100
#   }
#
#   # Create the Highcharts chart
#   highcharts <- highchart() |>
#     hc_add_series(df1, # Add the data series
#                   type = chart_type, # Use bar or column based on orientation
#                   hcaes(x = !!sym(x_var), y = !!sym(y_var)), # Map x and y variables
#                   dataLabels = list(enabled = TRUE, # Enable data labels
#                                     format = "{point.prop_label}",
#                                     style = list(fontWeight = "regular",
#                                                  fontSize = "14px",
#                                                  fontFamily = "Graphik",
#                                                  textOutline = 0))) |>
#     hc_xAxis(categories = xaxis_order, # Set x-axis categories
#              labels = list(
#                useHTML = TRUE,
#                enabled = TRUE,
#                # formatter = JS(js_code), # Format labels with JavaScript
#                style = list(
#                  fontSize = "14px",
#                  fontFamily = "Graphik",
#                  textAlign = label_alignment,
#                  overflow = "justify" # Prevent clipping of labels
#                ),
#                x = ifelse(orientation == "horizontal", -10, 0) # Add padding only for horizontal orientation
#              )
#     ) |>
#     hc_yAxis(max = 100, # Set y-axis maximum to 100% for proportions
#              labels = list(
#                formatter = JS("function() { return this.value + '%'; }") # Append % to y-axis labels
#              )) |>
#     hc_add_theme(base_hc_theme) |> # Apply the base theme
#     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> # Add custom tooltip formatter
#     hc_legend(enabled = FALSE) |> # Disable the legend
#     hc_title(text = title) |> # Add the chart title
#     fnc_add_hc_accessibility(accessibility_text) |>  # Add accessibility text
#     hc_caption(
#       text = paste0("Source: ",
#                     source1, ", ", year,
#                     if (!is.null(source2)) paste0(" and ", source2) else "", ".",
#                     other_race_note # Add the note dynamically
#       ),
#       y = caption_y
#     ) |>
#     fnc_add_logo_and_export(download_title, bottom_margin_value)
#
#   return(highcharts) # Return the generated Highchart
# }
fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
                               source1, source2 = NULL,
                               orientation = "vertical") {

  # Filter the data for the specified state
  df1 <- df |>
    filter(state == state_var) |> # Filter by state
    fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var)) # Add tooltips for better interactivity

  # Extract the reporting year for the state
  year <- unique(df1$rptyear)

  # Conditionally arrange data by proportions for certain variables
  if (x_var %in% c("race", "fbi_index", "sex")) {
    df1 <- df1 |> arrange(desc(prop)) # Arrange by descending proportions
  }

  # Adjust labels for fbi_index if needed
  if (x_var == "fbi_index") {
    label_map <- c(
      "Murder or Nonnegligent Manslaughter" = "Murder or Nonnegligent<br>Manslaughter",
      "Aggravated or Simple Assault" = "Aggravated or<br>Simple Assault"
    )
    df1[[x_var]] <- factor(
      df1[[x_var]],
      levels = levels(df1[[x_var]]), # Preserve factor ordering
      labels = sapply(levels(df1[[x_var]]), function(x) ifelse(x %in% names(label_map), label_map[x], x))
    )
  }

  # Define x-axis order based on the data
  xaxis_order <- df1[[x_var]]

  # Construct the chart title
  title <- paste0(title_type, " by ", metric)

  # Generate accessibility text describing the chart
  accessibility_text <- paste0("This graph shows the percentage of ", type,
                               " by ", tolower(metric), " in ",
                               year, " in the state of ", state_var, ".")

  # Download file title
  download_title <- paste0(gsub(" ", "_", tolower(title)), "_", year)

  # Determine chart type based on orientation
  chart_type <- ifelse(orientation == "horizontal", "bar", "column")

  # Adjust label alignment for horizontal orientation
  label_alignment <- ifelse(orientation == "horizontal", "right", "center")

  # Check if "Other race(s), non-Hispanic" exists in the x-axis variable
  other_race_note <- if (x_var == "race" && any(df1[[x_var]] == "Other race(s), non-Hispanic")) {
    "<br><br>According to the NCRP, the “Other race(s)” category may include American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, and individuals identifying as more than one race."
  } else {
    ""
  }

  # Determine caption_y based on x_var
  caption_y <- if (x_var == "race") {
    -30
  } else if (x_var %in% c("fbi_index")) {
    -30
  } else if (x_var %in% c("sentlgth") & type == "the prison population") {
    -20
  } else {
    -30 # Default space for other variables
  }

  # Space below chart to accompany logo
  bottom_margin_value <- if (x_var == "race") {
    160
  } else if (x_var %in% c("fbi_index")) {
    100
  } else if (x_var %in% c("sentlgth") & type == "the prison population") {
    140
  } else {
    100
  }

  # Create the Highcharts chart
  highcharts <- highchart() |>
    hc_add_series(df1, # Add the data series
                  type = chart_type, # Use bar or column based on orientation
                  hcaes(x = !!sym(x_var), y = !!sym(y_var)), # Map x and y variables
                  dataLabels = list(enabled = TRUE, # Enable data labels
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "14px",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) |>
    hc_xAxis(categories = xaxis_order, # Set x-axis categories
             labels = list(
               useHTML = TRUE,
               enabled = TRUE,
               style = list(
                 fontSize = "14px",
                 fontFamily = "Graphik",
                 textAlign = label_alignment,
                 overflow = "justify" # Prevent clipping of labels
               ),
               x = ifelse(orientation == "horizontal", -10, 0) # Add padding only for horizontal orientation
             )
    ) |>
    hc_yAxis(max = 100, # Set y-axis maximum to 100% for proportions
             labels = list(
               formatter = JS("function() { return this.value + '%'; }") # Append % to y-axis labels
             )) |>
    hc_add_theme(base_hc_theme) |> # Apply the base theme
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> # Add custom tooltip formatter
    hc_legend(enabled = FALSE) |> # Disable the legend
    hc_title(text = title) |> # Add the chart title
    fnc_add_hc_accessibility(accessibility_text) |> # Add accessibility text
    hc_caption(
      text = paste0("Source: ",
                    source1, ", ", year,
                    if (!is.null(source2)) paste0(" and ", source2) else "", ".",
                    other_race_note # Add the note dynamically
      ),
      y = caption_y
    ) |>
    fnc_add_logo_and_export(download_title, bottom_margin_value)

  return(highcharts) # Return the generated Highchart
}

#' Generate Bar Charts for Multiple States
#'
#' Creates a collection of bar charts for each state based on the input data,
#' visualizing a specified metric grouped by a given variable.
#'
#' @param data A data frame containing the data to visualize, with a `state` column.
#' @param x_var A string representing the variable to use on the x-axis (e.g., "fbi_index").
#' @param metric A string representing the label for the metric being visualized.
#' @param type A string describing the type of data (e.g., "Releases" or "Admissions").
#' @param title_type A string representing the title prefix for the chart.
#' @param y_var A string representing the variable to use on the y-axis (default: "prop").
#' @param source A string providing the source information for the chart caption.
#' @return A named list of Highcharts bar or column charts, one for each state.
#' @details
#' - Automatically determines the orientation (horizontal or vertical) based on the `x_var`.
#' - Passes the source and orientation dynamically to the chart creation function.
#' @examples
#' charts <- fnc_generate_bar_charts(data, "fbi_index", "Crime Type", "Releases", "Release Trends", "prop", "CSG Data Source")
#' @export
fnc_generate_bar_charts <- function(data, x_var, metric, type, title_type, y_var = "prop", source1, source2 = NULL) {
  # Extract unique states from the data
  states <- unique(data$state)

  # Generate charts for each state
  charts <- map(states, function(state_name) {
    # Determine chart orientation dynamically
    # Offense type, use horizontal bars
    orientation <- if (x_var == "fbi_index") "horizontal" else "vertical"

    # Call the column chart creation function for each state
    fnc_hc_columnchart(
      state_var   = state_name,   # Current state
      df          = data,         # Filtered data
      x_var       = x_var,        # X-axis variable
      y_var       = y_var,        # Y-axis variable (default: "prop")
      metric      = metric,       # Metric label
      type        = type,         # Type description (e.g., "Releases")
      title_type  = title_type,   # Title prefix
      orientation = orientation,  # Determine horizontal or vertical orientation
      source1     = source1,      # Source 1
      source2     = source2       # Source 2
    )
  })

  # Assign state names to the generated charts
  setNames(charts, states)
}

#' Generate Summary Sentences for Multiple States
#'
#' Creates a collection of summary sentences for each state based on the input data,
#' describing trends or distributions for a specified variable.
#'
#' @param data A data frame containing the data to summarize, with a `state` column.
#' @param x_var A string representing the variable to summarize (e.g., "fbi_index").
#' @param type A string describing the type of data (e.g., "Releases" or "Admissions").
#' @return A named list of sentences, one for each state.
#' @details
#' - Uses `fnc_generate_columnchart_sentence` to create state-specific summaries.
#' @examples
#' sentences <- fnc_generate_sentences(data, "fbi_index", "Releases")
#' @export
fnc_generate_sentences <- function(data, x_var, type) {
  # Extract unique states from the data
  states <- unique(data$state)

  # Generate sentences for each state
  sentences <- map(states, function(state_name) {
    # Call the sentence generation function for each state
    fnc_generate_columnchart_sentence(
      state_var = state_name, # Current state
      df        = data,       # Filtered data
      x_var     = x_var,      # X-axis variable for grouping
      type      = type        # Type description (e.g., "Releases")
    )
  })

  # Assign state names to the generated sentences
  setNames(sentences, states)
}

#' Generate a Column Chart Summary Sentence
#'
#' Creates a summary sentence describing trends or distributions based on the input data
#' for a specific state and a given variable.
#'
#' @param state_var A string representing the state name or code.
#' @param df A data frame containing data to summarize, with a `state` column.
#' @param x_var A string representing the variable to summarize (e.g., "fbi_index").
#' @param type A string describing the type of data (e.g., "Releases" or "Admissions").
#' @return A string summarizing trends or distributions for the specified state and variable.
#' @details
#' - Handles special cases for `fbi_index`, `sex`, age-related variables, and sentence length.
#' - Dynamically adjusts wording and formatting based on the input variable.
#' - Ensures robust handling of missing or incomplete data.
#' @examples
#' sentence <- fnc_generate_columnchart_sentence("Georgia", data, "fbi_index", "Releases")
#' @export
fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type) {
  # Filter the data for the specified state and arrange by proportion in descending order
  df1 <- df |>
    filter(state == state_var) |>
    arrange(-prop)

  # Extract the unique reporting year for the state
  year <- unique(df1$rptyear)

  # Handle cases where data is missing or insufficient
  if (nrow(df1) < 1 || is.na(df1$prop[1])) {
    return(paste0("Data for ", state_var, " is missing or incomplete."))
  }

  # Convert values to lowercase for "sex" variable
  if (x_var == "sex") {
    df1[[x_var]] <- tolower(df1[[x_var]])
  }

  # Adjust specific race text
  if (x_var == "race") {
    df1[[x_var]] <- gsub("^Other race\\(s\\), non-Hispanic$", "other race(s), non-Hispanic", df1[[x_var]])
  }

  # Special handling for "fbi_index" variable
  if (x_var == "fbi_index") {
    # Summarize violent and nonviolent offense proportions
    current_ped_offense_group <- df |>
      select(state, fbi_index, offense_group, n) |>
      filter(offense_group %in% c("Violent", "Nonviolent")) |>
      group_by(state, offense_group) |>
      summarise(total_offenses = sum(n), .groups = 'drop') |>
      group_by(state) |>
      mutate(prop = total_offenses / sum(total_offenses))

    violent_prop <- current_ped_offense_group |>
      filter(state == state_var, offense_group == "Violent") |>
      pull(prop) |>
      round(2) * 100
    nonviolent_prop <- current_ped_offense_group |>
      filter(state == state_var, offense_group == "Nonviolent") |>
      pull(prop) |>
      round(2) * 100

    # Identify all categories with the highest proportion
    max_prop <- max(round(df1$prop, 0))
    top_categories <- df1 |>
      filter(round(prop, 0) == max_prop) |>
      arrange(desc(prop))

    # Create sentences for top categories, appending "offenses" to each
    top_sentences <- top_categories |>
      mutate(sentence = paste0(tolower(fbi_index), " offenses (", round(prop, 0), " percent)")) |>
      pull(sentence)

    # Determine whether to use "type" or "types"
    type_word <- if (length(top_sentences) > 1) "types" else "type"

    # Format the final sentence based on the number of items
    if (length(top_sentences) > 2) {
      top_sentence_final <- paste(
        paste(top_sentences[-length(top_sentences)], collapse = ", "),
        ", and ", top_sentences[length(top_sentences)], sep = ""
      )
    } else if (length(top_sentences) == 2) {
      top_sentence_final <- paste(top_sentences[1], "and", top_sentences[2])
    } else {
      top_sentence_final <- top_sentences
    }

    # Construct the final sentence for "fbi_index"
    sentences <- paste0(
      violent_prop, " percent of people ", type,
      " were in prison for violent offenses and ",
      nonviolent_prop, " percent for nonviolent offenses. ",
      "The most common offense ", type_word, " among people ", type,
      " were ", top_sentence_final, ".")
  }
  # Special handling for age-related variables
  else if (x_var == "ageyrend" | x_var == "agerlse") {
    age_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0(
      round(df1$prop[1], 0),
      " percent of people ", type, " were between the ages of ",
      age_range[1], " and ", age_range[2], " old.")
  }
  # Special handling for sentence length variables
  else if (x_var == "sentlgth") {
    sent_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0(
      round(df1$prop[1], 0),
      " percent of people ", type, " had sentence lengths between ",
      sent_range[1], " and ", sent_range[2], ".")
  }
  # General case for other variables
  else {
    sentences <- paste0(
      round(df1$prop[1], 0),
      " percent of people ", type, " were ",
      df1[[x_var]][1], ".")
  }

  # Replace "offenses offenses" with "offenses"
  sentences <- gsub("offenses offenses", "offenses", sentences)

  return(sentences)
}





# ---------------------------------------------------------------------------- #
# Disparities Helper Functions
# ---------------------------------------------------------------------------- #

#' Filter Data by State and Year
#'
#' This function filters a dataset to include only rows corresponding to the
#' specified state and the most recent reporting year (`rptyear`) for that state.
#'
#' @param df A data frame containing at least `state` and `rptyear` columns.
#' @param state_var A string specifying the state to filter.
#' @return A list containing:
#'   - `data`: A filtered data frame for the specified state and year.
#'   - `year`: The most recent reporting year (`rptyear`) for the specified state.
#' @examples
#' filtered <- fnc_filter_data_by_state_year(df, "Georgia")
#' head(filtered$data)  # View filtered data
#' filtered$year        # View the most recent year
#' @export
fnc_filter_data_by_state_year <- function(df, state_var) {

  # Extract the most recent year for the specified state
  year <- df |>
    filter(state == state_var) |>
    pull(rptyear) |>
    max(na.rm = TRUE)

  # Filter the data frame to include only rows for the specified state and year
  df_filtered <- df |>
    ungroup() |>  # Ensure no grouping to avoid filtering issues
    filter(state == state_var) |>
    filter(rptyear == year)

  # Return the filtered data and the year as a list
  list(data = df_filtered, year = year)
}

#' Generate Scatter Charts by State
#'
#' This function generates scatter charts visualizing disparities in measures such as
#' average time served or years past parole eligibility by offense type for each state.
#' The visualizations highlight group differences (e.g., by race or sex) and are customized
#' with dynamic labels, colors, and accessibility features.
#'
#' @param df A data frame containing the data to be visualized, including offense type,
#'   grouping variables (e.g., race or sex), and the measure (e.g., `average_los`).
#' @param group_var A string specifying the grouping variable (`"sex"` or `"race"`).
#' @param measure A string specifying the measure variable (e.g., `"average_los"`).
#' @param source A string for the chart's source caption (default is `ncrp_csg_source`).
#' @return A named list of Highcharts objects, each corresponding to a state.
#' @export
fnc_create_scatter_charts_by_state <- function(df, group_var, measure, source1, source2 = NULL) {

  # Extract unique states to iterate over
  states <- unique(df$state)

  # Iterate through each state to generate scatter charts
  all_charts <- purrr::map(.x = states, .f = function(state_name) {

    # Define group-specific labels, colors, and shapes
    if (group_var == "sex") {
      group_labels <- c("Male", "Female")
      colors <- c(teal, purple)  # Colors for male and female
      shapes <- c("circle", "triangle")  # Shapes for male and female
    } else {
      group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
      colors <- c(teal, blue, purple, red)  # Colors for race groups
      shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
    }

    # Filter data for the specific state and prepare for visualization
    df1 <- df |>
      ungroup() |>
      filter(state == state_name) |>
      arrange(desc(!!sym(measure))) |>
      mutate(group_num = row_number())

    # Define the desired order of offense types
    desired_order <- c(
      "Drug",
      "Public Order",
      "Property",
      "Aggravated or Simple Assault",
      "Robbery",
      "Rape or Sexual Assault",
      "Negligent Manslaughter",
      "Murder or Nonnegligent Manslaughter",
      "Other Violent Offenses"
    )

    # Map offense types to their positions
    y_labels <- setNames(as.list(desired_order), seq_along(desired_order))

    # Extract the year of the data for labeling
    year <- unique(df1$rptyear)

    # Define dynamic titles and labels for the chart
    x_axis_title <- ifelse(measure == "average_los", "Average Time Served (Years)", "Average Years Past Parole Eligibility")
    chart_title <- paste0("Average ", ifelse(measure == "average_los", "Time Served", "Years Past Parole Eligibility"),
                          " by Offense and ", ifelse(group_var == "sex", "Sex", "Race and Ethnicity"))

    # Generate accessibility text for the chart
    accessibility_measure <- ifelse(measure == "average_los", "average length of stay", "average years past parole eligibility")
    accessibility_text <- paste0("The chart shows the ", accessibility_measure, " for different ",
                                 group_var, " groups in ", state_name, ".")

    # Set maximum value for scaling
    max_los <- max(df1[[measure]], na.rm = TRUE)

    # Download file title
    download_title <- paste0(gsub(" ", "_", tolower(chart_title)), "_",
                                     year)

    # Space below chart to accompany logo
    bottom_margin_value <- 120

    # Initialize Highcharts object
    highcharts <- highchart() |>
      hc_title(text = chart_title) |>
      hc_yAxis(
        title = list(text = ""),  # Y-axis title
        labels = list(enabled = TRUE, style = list(color = "black")),  # Style Y-axis labels
        categories = y_labels,  # Map dynamically filtered categories to offense types
        gridLineColor = "transparent",  # Remove grid lines
        reversed = TRUE  # Reverse order for better readability
      ) |>
      hc_xAxis(
        title = list(text = x_axis_title, style = list(color = "black")),  # X-axis title
        labels = list(style = list(color = "black")),  # Style X-axis labels
        gridLineDashStyle = "Dash",  # Dashed grid lines
        gridLineWidth = 1,  # Set grid line width
        gridLineColor = "lightgray",  # Set grid line color
        tickLength = 0  # Remove tick marks
      ) |>
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0("
    function() {
      function fnc_time_format(time_in_years) {
        if (time_in_years < 1) {
          var months = Math.round(time_in_years * 12);
          return months + ' ' + (months === 1 ? 'month' : 'months');
        } else {
          var years = Math.floor(time_in_years);
          var months = Math.round((time_in_years - years) * 12);
          if (months === 12) {
            years += 1; // Roll over to the next year
            months = 0;
          }
          var year_part = years + ' ' + (years === 1 ? 'year' : 'years');
          if (months > 0) {
            var month_part = months + ' ' + (months === 1 ? 'month' : 'months');
            return year_part + ' ' + month_part;
          } else {
            return year_part;
          }
        }
      }
      return '<b>' + this.series.name + '</b><br/>' +
             'Offense: ' + (this.point.fbi_index || 'Unknown') + '<br/>' +
             '", ifelse(measure == "average_los", "Average Time Served", "Average Time Past Parole Eligibility"),
                              ": ' + fnc_time_format(this.point.x) + '<br/>' +
             'People: ' + (this.point.people ? this.point.people.toLocaleString() : 'N/A');
    }
  "))
      ) |>
      hc_legend(layout = "horizontal", verticalAlign = "top") |>
      hc_add_theme(base_hc_theme) |>
      hc_caption(
        text = paste0("Source: ",
          source1, ", ", year,
          if (!is.null(source2)) paste0(" and ", source2) else "", "."
        ),
        y = -30
      ) |>
      fnc_add_logo_and_export(download_title, bottom_margin_value, 200) |>
      fnc_add_hc_accessibility(accessibility_text)

    # Add scatter series for each group dynamically
    for (i in seq_along(group_labels)) {

      highcharts <- highcharts |>
        hc_add_series(
          df1 |> filter(!!sym(group_var) == group_labels[i]),
          type = 'scatter',  # Scatter plot
          color = colors[i],  # Assign color
          hcaes(x = !!sym(measure), y = as.numeric(factor(fbi_index, levels = desired_order)), group = !!sym(group_var)),
          marker = list(symbol = shapes[i], radius = 5)  # Assign marker shape and size
        )
    }

    return(highcharts)
  })

  # Assign state names to the resulting charts list
  all_charts <- setNames(all_charts, states)

  return(all_charts)
}


fnc_create_lollipop_chart <- function(df, group_var, value_var, state_name, height = 200, source, second_source = NULL) {

  # Define consistent group labels, colors, and shapes
  if (group_var == "sex") {
    group_labels <- c("Male", "Female")
    colors <- c(teal, purple)
    shapes <- c("circle", "triangle")
    bottom_margin_value <- 80
    caption_y <- -30
  } else {
    group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
    colors <- c(teal, blue, purple, red)
    shapes <- c("square", "circle", "diamond", "triangle")
    bottom_margin_value <- 80
    caption_y <- -30
  }

  # Filter and prepare data for the specified state
  df1 <- df |>
    ungroup() |>
    filter(state == state_name) |>
    arrange(desc(!!sym(value_var))) |>
    mutate(group_num = row_number(),
           color = case_when(
             !!sym(group_var) == group_labels[1] ~ colors[1],
             !!sym(group_var) == group_labels[2] ~ colors[2],
             !!sym(group_var) == group_labels[3] ~ colors[3],
             !!sym(group_var) == group_labels[4] ~ colors[4]
           )) |>
    rowwise() |>  # Ensure row-wise operation
    mutate(time_label = fnc_time_format(!!sym(value_var))) |>  # Apply `fnc_time_format` to each row
    ungroup()  # Remove rowwise grouping after mutate

  year <- unique(df1$rptyear)
  max_value <- max(df1[[value_var]], na.rm = TRUE)

  # Create a named list for y-axis labels
  y_labels <- as.list(setNames(as.character(df1[[group_var]]), df1$group_num))

  # Accessibility text
  accessibility_text <- paste0("The chart below shows the average ", value_var, " for different ",
                               group_var, " groups in ", state_name, ".")

  # Prepare line segments for the lollipop chart
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = !!sym(value_var)) |>
    select(group_num, start_x, end_x, !!sym(group_var)) |>
    gather(key = "point", value = "value", start_x, end_x)

  # Set chart title dynamically based on the measure
  chart_title <- ifelse(
    value_var == "average_los",
    paste("Average Time Served by", ifelse(group_var == "sex", "Sex", "Race and Ethnicity")),
    paste("Average Years Past Parole Eligibility by", ifelse(group_var == "sex", "Sex", "Race and Ethnicity"))
  )

  # Initialize the highchart object
  highcharts <- highchart() |>
    hc_title(text = chart_title) |>

    # Line series for the lollipop stems
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = group_num, group = !!sym(group_var)),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    )

  # Add scatter points for each group with markers and labels
  for (i in seq_along(group_labels)) {
    highcharts <- highcharts |>
      hc_add_series(
        df1 %>% filter(!!sym(group_var) == group_labels[i]),
        type = 'scatter',
        color = colors[i],
        hcaes(x = !!sym(value_var), y = group_num, group = !!sym(group_var), name = !!sym(group_var)),
        marker = list(
          radius = 5,
          symbol = shapes[i]  # Use unique shape for each group
        ),
        dataLabels = list(
          enabled = TRUE,
          format = '{point.time_label}',  # Use the formatted time_label
          align = "left",
          y = 9,
          x = 8,
          style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
        )
      )
  }

  # Add axes, themes, and captions
  caption_text <- if (!is.null(second_source)) {
    paste0("Source: ", source, ", ", year, " and ", second_source, ".")
  } else {
    paste0("Source: ", source, ", ", year, ".")
  }

  # Download file title
  download_title <- paste0(gsub(" ", "_", tolower(chart_title)), "_",
                           year)

  # Add axes, themes, and captions
  highcharts <- highcharts |>
    hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
        enabled = TRUE,
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      tickColor = "white",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_value * 1.5
    ) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = height) |>
    hc_caption(text = caption_text,
               y = caption_y) |>
    fnc_add_logo_and_export(download_title, bottom_margin_value) |>
    fnc_add_hc_accessibility(accessibility_text)

  return(highcharts)
}


fnc_generate_lollipop_charts <- function(df, group_var, value_var, height = 200, source, second_source = NULL) {

  states <- unique(df$state)

  all_charts <- purrr::map(states, function(state_var) {
    fnc_create_lollipop_chart(
      df = df,
      group_var = group_var,
      value_var = value_var,
      state_name = state_var,
      source = source,
      second_source = second_source,
      height = height
    )
  })

  all_charts <- setNames(all_charts, states)

  return(all_charts)
}




# ---------------------------------------------------------------------------- #
# Disparities Sentences
# ---------------------------------------------------------------------------- #

fnc_time_format <- function(time_in_years) {
  if (time_in_years < 1) {
    # Convert to months for times under 1 year
    months <- round(time_in_years * 12)
    paste0(months, " ", ifelse(months == 1, "month", "months"))
  } else {
    # Calculate years and remaining months for times over 1 year
    years <- floor(time_in_years)
    months <- round((time_in_years - years) * 12)
    if (months == 12) {
      # Handle edge case where rounding results in 12 months
      years <- years + 1
      paste0(years, " ", ifelse(years == 1, "year", "years"))
    } else {
      year_part <- paste0(years, " ", ifelse(years == 1, "year", "years"))
      if (months > 0) {
        month_part <- paste0(months, " ", ifelse(months == 1, "month", "months"))
        paste0(year_part, " and ", month_part)
      } else {
        year_part
      }
    }
  }
}

fnc_time_format_months <- function(time_in_months) {
  time_in_months <- round(time_in_months)  # Round the months
  if (time_in_months < 12) {
    # For times less than 12 months, return only months
    paste0(time_in_months, " ", ifelse(time_in_months == 1, "month", "months"))
  } else {
    # Convert to years and remaining months for times 12 months or more
    years <- floor(time_in_months / 12)
    months <- time_in_months %% 12
    if (months == 12) {
      # Handle edge case where months round to 12
      years <- years + 1
      months <- 0
    }
    year_part <- paste0(years, " ", ifelse(years == 1, "year", "years"))
    if (months > 0) {
      month_part <- paste0(months, " ", ifelse(months == 1, "month", "months"))
      paste0(year_part, " and ", month_part)
    } else {
      year_part
    }
  }
}



fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
  # Extract unique states for iteration
  states <- unique(df$state)

  # Determine the introductory sentence based on compare_var and type
  intro_sentence <- if (compare_var == "sex") {
    if (type == "in prison") {
      "The chart below shows the average time served in prison by sex among released individuals."
    } else if (type == "past parole eligibility") {
      "The chart below shows the average time spent in prison past parole eligibility by sex for individuals still incarcerated."
    } else {
      ""
    }
  } else if (compare_var == "race") {
    if (type == "in prison") {
      "The chart below shows the average time served in prison by race and ethnicity for individuals released from prison."
    } else if (type == "past parole eligibility") {
      "The chart below shows the average time spent in prison past parole eligibility by race and ethnicity for individuals still incarcerated."
    } else {
      ""
    }
  } else {
    ""
  }

  # Generate sentences for each state
  all_sentences <- purrr::map(.x = states, .f = function(state_var) {
    # Use helper function to filter data by state and year
    filtered_data <- fnc_filter_data_by_state_year(df, state_var)
    df1 <- filtered_data$data
    year <- filtered_data$year

    # Handle missing data for the state
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", state_var))
    }

    if (compare_var == "sex") {
      return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
    } else if (compare_var == "race") {
      df1 <- df1 |>
        dplyr::mutate(race = dplyr::case_when(
          race == "White, non-Hispanic" ~ "White",
          race == "Black, non-Hispanic" ~ "Black",
          race == "Hispanic, any race" ~ "Hispanic",
          race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
        ))

      df_white <- df1 |> dplyr::filter(race == "White")
      black_sentence <- ""
      hispanic_sentence <- ""
      other_sentence <- ""
      overall_summary <- ""
      groups_more <- c()
      groups_less <- c()

      if (type == "in prison") {
        summary_phrase <- "spent more time in prison than White people"
        less_phrase <- "spent less time in prison than White people"
        detail_suffix <- "in prison"
      } else if (type == "past parole eligibility") {
        summary_phrase <- "spent more time in prison after becoming eligible for parole than White people"
        less_phrase <- "spent less time in prison after becoming eligible for parole than White people"
        detail_suffix <- "past parole eligibility"
      }

      # Black vs White
      df_black <- df1 |> dplyr::filter(race == "Black")
      converted_black <- round((df_black[[los_col]])*12)
      converted_white <- round((df_white[[los_col]])*12)

      if (nrow(df_black) > 0 && nrow(df_white) > 0) {
        los_diff_black <- converted_black - converted_white
        if (!is.na(los_diff_black)) {
          formatted_time <- fnc_time_format_months(abs(los_diff_black))
          black_sentence <- if (los_diff_black > 0) {
            groups_more <- c(groups_more, "Black people")
            paste0("Black people spent on average ", formatted_time, " more ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "Black people")
            paste0("Black people spent on average ", formatted_time, " less ", detail_suffix)
          }
        }
      }

      # Hispanic vs White
      df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
      converted_hispanic <- round((df_hispanic[[los_col]])*12)

      if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
        los_diff_hispanic <- converted_hispanic - converted_white
        if (!is.na(los_diff_hispanic)) {
          formatted_time <- fnc_time_format_months(abs(los_diff_hispanic))
          hispanic_sentence <- if (los_diff_hispanic > 0) {
            groups_more <- c(groups_more, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " more ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "Hispanic people")
            paste0("Hispanic people spent on average ", formatted_time, " less ", detail_suffix)
          }
        }
      }

      # Other vs White
      df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
      converted_other <- round((df_other[[los_col]]) * 12)

      if (nrow(df_other) > 0 && nrow(df_white) > 0) {
        los_diff_other <- converted_other - converted_white
        if (!is.na(los_diff_other) && los_diff_other != 0) { # Exclude if no difference
          formatted_time <- fnc_time_format_months(abs(los_diff_other))
          other_sentence <- if (los_diff_other > 0) {
            groups_more <- c(groups_more, "non-Hispanic people of other races")
            paste0("non-Hispanic people of other races spent on average ", formatted_time, " more ", detail_suffix)
          } else {
            groups_less <- c(groups_less, "non-Hispanic people of other races")
            paste0("non-Hispanic people of other races spent on average ", formatted_time, " less ", detail_suffix)
          }
        }
      }

      # Construct overall summary
      if (length(groups_more) > 0) {
        overall_summary <- paste0(paste(groups_more, collapse = " and "), " ", summary_phrase, ".")
      }
      if (length(groups_less) > 0) {
        if (overall_summary != "") {
          overall_summary <- paste0(overall_summary, " ")
        }
        overall_summary <- paste0(overall_summary, paste(groups_less, collapse = " and "), " ", less_phrase, ".")
      }

      # Combine sentences
      sentences <- c(black_sentence, hispanic_sentence, other_sentence)
      sentences <- sentences[sentences != ""] # Remove empty sentences

      # Exclude sentences mentioning "0 months"
      sentences <- sentences[!grepl("\\b0 months\\b", sentences)]

      if (length(sentences) > 0) {
        # Construct the final sentence with proper formatting
        if (length(sentences) > 1) {
          # Use a comma for the first parts and 'and' before the last item
          sentence_body <- paste0(paste(sentences[-length(sentences)], collapse = ", "), ", and ", sentences[length(sentences)])
        } else {
          # Only one sentence, no need for a conjunction
          sentence_body <- sentences[1]
        }

        # Combine the summary and detailed sentences
        final_sentence <- paste0(overall_summary, " ", sentence_body, " compared to White people.")

        # Correct "Non-Hispanic" capitalization if needed
        final_sentence <- gsub("Non-Hispanic", "non-Hispanic", final_sentence)
        final_sentence <- gsub("\\. and", " and", final_sentence) # Fix edge cases with unnecessary ". and"
        final_sentence <- paste0(intro_sentence, " ", final_sentence)

        return(final_sentence)
      } else {
        return("")
      }

    } else {
      return("Invalid comparison variable.")
    }
  })

  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}

fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
  # Filter the data for males
  df_male <- df1 |> dplyr::filter(sex == "Male")

  if (type == "in prison") {
    intro_sentence <- "The chart below shows the average time served in prison by sex among released individuals."
  } else if (type == "past parole eligibility") {
    intro_sentence <- "The chart below shows the average time spent in prison past parole eligibility by sex for individuals still incarcerated."
  } else {
    intro_sentence <- ""
  }

  # Initialize an empty sentence variable
  sentence <- ""

  # Filter the data for females
  df_female <- df1 |> dplyr::filter(sex == "Female")
  converted_male <- df_male[[los_col]]
  converted_female <- df_female[[los_col]]

  # Check if both male and female data exist
  if (nrow(df_female) > 0 && nrow(df_male) > 0) {
    # Calculate the difference in length of stay (LOS) between females and males
    los_diff_female <- converted_female - converted_male

    # Ensure the LOS difference is not NA
    if (!is.na(los_diff_female)) {
      formatted_time <- fnc_time_format(abs(los_diff_female))  # Format the time difference

      if (los_diff_female > 0) {
        # Males spent more time on average
        sentence <- paste0(
          intro_sentence, " ",
          "Men ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", formatted_time, " more ",
          if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to women."
        )
      } else if (los_diff_female < 0) {
        # Females spent less time on average
        sentence <- paste0(
          intro_sentence, " ",
          "Women ",
          if (type == "in prison") "released" else "who were still incarcerated",
          " spent on average ", formatted_time, " less ",
          if (type == "in prison") "in prison" else "past parole eligibility",
          " compared to men."
        )
      }
    }
  }

  # Handle cases where no meaningful disparity exists or data is missing
  if (sentence != "") {
    return(sentence)  # Return the constructed sentence if disparity is found
  } else {
    return(paste0(
      ""
    ))
  }
}


fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los") {
  # Extract unique states to iterate over
  states <- unique(data$state)

  # Generate sentences for each state
  all_sentences <- purrr::map(.x = states, .f = function(x) {
    # Filter data for the specified state and exclude unspecified offense types
    df1 <- data |>
      dplyr::filter(state == x & fbi_index != "Other or Unspecified")

    # Handle missing data: If no data exists for the state, return a message
    if (nrow(df1) == 0) {
      return(paste0("No data available for ", x))
    }

    if (grouping_var == "race") {
      # Transforming data into wide format
      df_wide_largest <- df1 |>
        group_by(state, race, fbi_index) |>
        summarize(avg_time_months = mean(.data[[time_var]] * 12, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = race,
          values_from = avg_time_months,
          names_prefix = "",
          names_glue = "{race}_avg_time_months"
        )

      # Ensure all necessary columns exist by initializing missing columns with NA
      necessary_columns <- c(
        "White, non-Hispanic_avg_time_months",
        "Hispanic, any race_avg_time_months",
        "Black, non-Hispanic_avg_time_months",
        "Other race(s), non-Hispanic_avg_time_months"
      )
      for (col in necessary_columns) {
        if (!col %in% names(df_wide_largest)) {
          df_wide_largest[[col]] <- NA_real_
        }
      }

      # Rename and calculate differences
      df_wide_largest <- df_wide_largest |>
        ungroup() |>
        mutate(
          diff_Hispanic_White = round(`Hispanic, any race_avg_time_months`, 1) - round(`White, non-Hispanic_avg_time_months`, 1),
          diff_Black_White = round(`Black, non-Hispanic_avg_time_months`, 1) - round(`White, non-Hispanic_avg_time_months`, 1),
          diff_Other_White = round(`Other race(s), non-Hispanic_avg_time_months`, 1) - round(`White, non-Hispanic_avg_time_months`, 1)
        ) |>
        rowwise() |>
        mutate(
          largest_diff = ifelse(
            all(is.na(c_across(c(diff_Hispanic_White, diff_Black_White, diff_Other_White)))),
            NA_real_,
            max(c_across(c(diff_Hispanic_White, diff_Black_White, diff_Other_White)), na.rm = TRUE)
          ),
          chosen_column = case_when(
            largest_diff == diff_Hispanic_White ~ "diff_Hispanic_White",
            largest_diff == diff_Black_White ~ "diff_Black_White",
            largest_diff == diff_Other_White ~ "diff_Other_White",
            TRUE ~ NA_character_
          )
        ) |>
        ungroup() |>
        filter(!is.na(largest_diff)) |>
        slice_max(largest_diff, with_ties = TRUE)

    } else if (grouping_var == "sex") {
      df_wide_largest <- df1 |>
        group_by(state, sex, fbi_index) |>
        summarize(avg_time_months = mean(.data[[time_var]] * 12, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(
          names_from = sex,
          values_from = avg_time_months,
          names_prefix = "",
          names_glue = "{sex}_avg_time_months"
        ) |>
        ungroup() |>
        mutate(diff_Male_Female = round(Male_avg_time_months, 1) - round(Female_avg_time_months, 1)) |>
        rowwise() |>
        mutate(
          largest_diff = ifelse(
            all(is.na(diff_Male_Female)),
            NA_real_,
            max(diff_Male_Female, na.rm = TRUE)
          ),
          chosen_column = ifelse(!is.na(largest_diff), "diff_Male_Female", NA_character_)
        ) |>
        ungroup() |>
        filter(!is.na(largest_diff)) |>
        slice_max(largest_diff, with_ties = TRUE)
    }

    # Handle no significant differences
    if (nrow(df_wide_largest) == 0 || all(is.na(df_wide_largest$largest_diff))) {
      time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0(
        "The chart below shows the average ", time_description, " by offense type and ",
        ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."
      ))
    }

    # Extract details for the largest disparity
    df_row <- df_wide_largest |> slice(1) # Select the first row
    offense_type <- df_row$fbi_index
    chosen_column <- df_row$chosen_column
    group_longest <- case_when(
      chosen_column == "diff_Hispanic_White" ~ "Hispanic",
      chosen_column == "diff_Black_White" ~ "Black",
      chosen_column == "diff_Other_White" ~ "non-Hispanic people of other races",
      chosen_column == "diff_Male_Female" ~ "men",
      TRUE ~ NA_character_
    )
    group_shortest <- ifelse(
      chosen_column %in% c("diff_Hispanic_White", "diff_Black_White", "diff_Other_White"),
      "White",
      "women"
    )
    disparity_diff_months <- df_row$largest_diff

    if (is.na(group_longest)) {
      time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
      return(paste0(
        "The chart below shows the average ", time_description, " by offense type and ",
        ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."
      ))
    }

    # Format time in months and years
    formatted_time <- fnc_time_format_months(abs(disparity_diff_months))

    # Construct the descriptive sentence
    time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
    sentence <- paste0(
      "The chart below shows the average ", time_description, " by offense type and ",
      ifelse(grouping_var == "race", "race and ethnicity", grouping_var),
      ifelse(time_var == "average_los", " for individuals released from prison", " for individuals still incarcerated"), ". ",
      "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
      group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
      " spent on average ", formatted_time, " more in prison compared to ",
      group_shortest, if (grouping_var == "race") " people" else "", "."
    )
    # Replace "offenses offenses" with "offenses"
    sentence <- gsub("offenses offenses", "offenses", sentence)
    return(sentence)
  })

  # Assign state names to the resulting list
  all_sentences <- setNames(all_sentences, states)

  return(all_sentences)
}





# ---------------------------------------------------------------------------- #
# RRI Helper Functions
# ---------------------------------------------------------------------------- #

#' Calculate Relative Rate Index (RRI) for Groups
#'
#' This function calculates the Relative Rate Index (RRI) for a specified
#' category (e.g., race or sex) compared to a reference group.
#'
#' @param data A data frame containing the data, including `state`, `past_pe_rate`,
#'   and the category of interest (e.g., race or sex).
#' @param comparison_group A string specifying the reference group for comparison
#'   (e.g., "White people" or "females").
#' @param category A string indicating the column name for the category of interest
#'   (e.g., "race" or "sex").
#' @return A data frame containing the state, category, and calculated RRI.
#' @export
fnc_calculate_rri <- function(data, comparison_group, category) {
  # Calculate reference rate for the comparison group
  reference_rate_data <- data |>
    filter(!!sym(category) == comparison_group) |>
    select(state, rptyear, past_pe_rate) |>  # Include rptyear in the selection
    rename(reference_past_pe_rate = past_pe_rate)  # Rename rate for clarity

  # Calculate RRI for all groups
  rri_data <- data |>
    inner_join(reference_rate_data, by = c("state", "rptyear")) |>  # Join by state and rptyear
    mutate(rri = round(past_pe_rate / reference_past_pe_rate, 1)) |>  # Calculate RRI
    select(state, rptyear, !!sym(category), rri)  # Keep rptyear in the output

  return(rri_data)
}

#' Generate RRI Sentences for Disparities
#'
#' This function generates HTML-formatted sentences describing disparities in
#' incarceration rates past parole eligibility for a given category (e.g., race or sex)
#' compared to a reference group.
#'
#' @param data A data frame containing the calculated RRI values for each group,
#'   including `state`, `category`, and `rri`.
#' @param category A string indicating the column name for the category of interest
#'   (e.g., "race" or "sex").
#' @param label A string specifying the label for the group of interest (e.g., "Black people").
#' @param color A string indicating the HTML color code for styling the group label in the sentence.
#' @return A named list of HTML-formatted sentences for each state.
#' @export
fnc_generate_rri_sentences <- function(data, category, label, color) {
  # Define comparison group and color based on category
  comparison_group <- if (category == "race") "White people" else "women"
  comparison_color <- if (category == "race") red else purple

  # Iterate over each state to generate sentences
  sentences <- map(unique(data$state), function(state_name) {
    # Filter data for the specific state and category label
    df1 <- data |> filter(state == state_name, !!sym(category) == label)

    # Handle missing data
    if (nrow(df1) == 0 || is.na(df1$rri)) return("")

    # Extract RRI value
    rri <- df1$rri

    # Ensure "label" is lowercase if it matches "Male"
    # label <- if (label == "Male") "males" else label
    # Adjust the label for specific cases
    label <- case_when(
      label == "Hispanic, any race" ~ "Hispanic people",
      label == "Black, non-Hispanic" ~ "Black, non-Hispanic people",
      label == "Male" ~ "Men",
      TRUE ~ label
    )

    # Generate sentence for RRI > 1 (higher disparity)
    if (rri > 1) {
      paste0(
        # "In ", df1$rptyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
        "<span style='color:", color, "; font-weight:bold;'>", label,
        "</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:",
        color, "; font-weight:bold;'>", rri, " times higher</span> than <span style='color:",
        comparison_color, "; font-weight:bold;'>", comparison_group,
        "</span>, when accounting for prison population sizes in ", state_name, "."
      )
    } else {  # Generate sentence for RRI <= 1 (lower disparity)
      percent_less <- round((1 - rri) * 100, 0)
      paste0(
        # "In ", df1$rptyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
        "<span style='color:", color, "; font-weight:bold;'>", label,
        "</span> were <span style='color:", color, "; font-weight:bold;'>", percent_less,
        " percent less likely</span> to be incarcerated in state prison past parole eligibility compared to <span style='color:",
        comparison_color, "; font-weight:bold;'>", comparison_group,
        "</span>, when accounting for population sizes in ", state_name, "."
      )
    }
  })

  # Assign state names to the list of sentences
  sentences <- setNames(sentences, unique(data$state))

  return(sentences)
}


# ---------------------------------------------------------------------------- #
# People Infographic Helper Functions
# ---------------------------------------------------------------------------- #

#' Create and Save State-Specific Infographic
#'
#' This function generates and saves state-specific infographics based on the provided
#' Relative Rate Index (RRI) data. For each state, it creates an infographic using
#' the `fnc_create_infographic` function, saves the plot as a PNG file, and crops
#' the saved image for better presentation.
#'
#' @param data A data frame containing the RRI data with columns `state` and `rri`.
#' @param color A string representing the color to use for the infographic elements.
#' @param prefix A string to prefix the saved infographic filenames, typically indicating
#'        the type of data or infographic.
#'
#' @return This function does not return a value but saves PNG files to the specified
#'         folder (`png_folder`) for each state.
#' @examples
#' # Example usage:
#' fnc_create_and_save_infographic(data = rri_data, color = "blue", prefix = "rri_")
#'
fnc_create_and_save_infographic <- function(data, color, prefix) {
  # Get a unique list of states to iterate over
  states <- unique(data$state)

  # Iterate over each state and create its infographic
  purrr::map(.x = states, .f = function(x) {
    # Filter the data for the specific state
    df_state <- data |> filter(state == x)

    # Create the infographic using the state's RRI and specified color
    fnc_create_infographic(df_state$rri, color)

    # Format the state name for filename consistency
    # Convert the state name to lowercase and replace spaces with underscores
    formatted_state <- stringr::str_to_lower(stringr::str_replace_all(x, " ", "_"))

    # Construct the file path for saving the infographic
    file_path <- file.path(png_folder, paste0(prefix, formatted_state, ".png"))

    # Save the infographic to the specified file path
    # Use `ggsave` with standard dimensions and resolution
    ggsave(file_path, plot = ggplot2::last_plot(), width = 8, height = 6, dpi = 300)

    # Read the saved PNG image for cropping
    img <- magick::image_read(file_path)

    # Crop the image to remove excess whitespace
    img_cropped <- magick::image_trim(img)

    # Save the cropped image back to the same file path
    magick::image_write(img_cropped, file_path)
  })
}

#' @title Blank Out Plot Theme
#' @description This function sets up a theme for blanking out plot elements like axes, scales, and legends.
#' @return A list of ggplot2 theme and scale elements for use in plots.
#' @export
fnc_blankitout <- function(){
  list(
    theme_void(),  # Removes background and gridlines for a clean appearance.
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
    theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
  )
}

#' Generate Icon Options with Partial and Full Fill States
#'
#' This function generates a set of icon plots based on different fill states
#' (empty, full, partial) using a specified image matrix. The icons can be filled
#' horizontally or vertically and are styled with customizable colors.
#'
#' @param partialval A numeric value between 0 and 1 indicating the proportion of
#'   the icon to be filled for the "partial" state.
#' @param empty A string specifying the color for the empty part of the icon (default: white).
#' @param fill A string specifying the color for the fully filled part of the icon (default: dark color).
#' @param partial A string specifying the color for the partially filled part of the icon (default: light color).
#' @param bg A string specifying the background color of the icon (default: white).
#' @param fillHoriz A logical value indicating whether the fill should be applied
#'   horizontally (TRUE) or vertically (FALSE). Defaults to FALSE (vertical fill).
#'
#' @return A list of ggplot objects representing the empty, full, and partially filled states of the icon.
fnc_icon_options <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
  # Ensure partialval is within valid range
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Define color sets for different states of the icon (empty, full, partial)
  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )

  # Define percentage fills for each icon state
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )

  # Initialize the plot list to store generated plots for each state
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  # Determine the boundaries for filling either horizontally or vertically
  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
    max <- max(pos1)
  }
  h <- dim(img)[1]  # Icon height
  w <- dim(img)[2]  # Icon width
  min <- min(pos1)

  # Loop through each icon state and generate corresponding plot
  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
    pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
    finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
    bkgr <- (finalimg == 1)  # Background mask
    colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix

    # Apply the fill either horizontally or vertically
    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    # Assign partially filled cells in the image
    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting

    # Remove partial fill for the 'full' state
    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    # Create the ggplot for each icon state
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
      fnc_blankitout()  # Apply the blank theme

    plot_lst[[j]] <- plot  # Store the plot in the list
  }

  return(plot_lst)  # Return the list of generated plots
}

#' Create Icons for Representing RRI (Relative Rate Index)
#'
#' This function generates a grid of icons to visually represent the Relative Rate Index (RRI).
#' Icons can be fully filled, partially filled, or empty, with customizable colors and arrangements.
#'
#' @param rri_raw Numeric value of the RRI to represent.
#' @param rri_digits Integer specifying the number of decimal places to round the RRI (default: 1).
#' @param fillcolor Character specifying the color for fully filled icons (default: `dark_color`).
#' @param partialcolor Character specifying the color for partially filled icons (default: `light_color`).
#' @param emptyhumans Logical indicating whether to include empty icons in the grid (default: `TRUE`).
#' @param emptycolor Character specifying the color for empty icons (default: white).
#' @param infogs Integer specifying the total number of icons in the grid (default: `default_ncols`).
#' @param infogs_ncol Integer specifying the number of columns in the grid (default: `default_ncols`).
#' @param fillHoriz Logical indicating whether the fill should be applied horizontally (default: `FALSE`).
#'
#' @return A grid of icons as a ggplot object.
fnc_create_icons <- function(rri_raw, rri_digits = 1, fillcolor = dark_color, partialcolor = light_color,
                             emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                             infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value and compute full and partial icons
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partial icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()  # Initialize list for storing plots

  # Create full and partial icons based on RRI value
  if (RRI > 1 & numremain != 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
    plot_list[[numfull + 1]] <- plot_opts$partial
  } else if (RRI > 1 & numremain == 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
  } else if (RRI == 1) {
    plot_list[[1]] <- plot_opts$full
  } else if (RRI < 1) {
    plot_list[[1]] <- plot_opts$partial
  }

  # Add empty icons if needed
  if (emptyhumans == TRUE & length(plot_list) != infogs) {
    st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
    for (i in st_empty:infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(rri_raw / infogs_ncol), 1)

  # Return the grid of icon plots
  plot_grid(plotlist = plot_list, nrow = rows)
}

#' Create an Infographic Representing the RRI (Relative Rate Index)
#'
#' This function generates an infographic that visually represents the Relative Rate Index (RRI)
#' using an icon grid and a bold text label displaying the RRI value.
#'
#' @param rri_raw Numeric value of the RRI to represent.
#' @param infographic_color Character specifying the color for the icons and text in the infographic.
#'
#' @return A ggplot object combining the RRI text label and the icon grid.
fnc_create_infographic <- function(rri_raw, infographic_color) {

  # Round the RRI value and format as a text label
  rri_text <- paste0(round(rri_raw, digits = 1), "x")

  # Generate the icons for the infographic
  ggtemp_justpeople <- fnc_create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = infographic_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Create the plot for displaying the RRI text label
  rri_label_plot <- ggplot() +
    annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
             fontface = "bold",
             color = infographic_color,
             family = "Graphik") +
    theme_void()

  # Combine the RRI label plot with the icon grid
  final_plot <- plot_grid(
    rri_label_plot, ggtemp_justpeople,
    nrow = 1, rel_widths = c(1, 6)  # Adjust widths to balance the label and icons
  )

  print(final_plot)  # Display the final infographic plot
}

#' Round Numbers to Significant Figures or Nearest Tens
#'
#' This function rounds numbers based on their magnitude.
#' - For numbers with 3 or more digits, it rounds to the nearest power of 10 below.
#' - For smaller numbers, it rounds to the nearest tens place.
#'
#' @param x A numeric vector to be rounded.
#' @return A numeric vector with rounded values. If the input contains `NA`, the corresponding output will also be `NA`.
#' @examples
#' fnc_round_to_power(c(12345, 678, 45, 9, NA))
#' # Returns: 12300, 680, 50, 10, NA
#'
fnc_round_to_power <- function(x) {
  sapply(x, function(val) {
    # Check if the value is NA, and return NA if true
    if (is.na(val)) {
      return(NA)
    }

    # Determine the number of digits in the number
    digits <- nchar(floor(val))

    # Define the rounding level: if digits >= 3, round to the nearest power of 10 down, else round to 10
    if (digits >= 3) {
      power <- 10^(digits - 2) # This determines the rounding level to the nearest power of 10 below
      round(val / power) * power  # Use round to round to the nearest significant value
    } else {
      round(val, -1)
    }
  })
}


#' Create an Icon Grid for the Homepage
#'
#' This function generates a grid of icons representing the Relative Rate Index (RRI),
#' with the first icon displayed in a distinct color (e.g., green), followed by a combination
#' of full, partial, and empty icons to represent the RRI value.
#'
#' @param rri_raw Numeric value of the RRI to represent.
#' @param rri_digits Integer specifying the number of decimal places to round the RRI.
#' @param fillcolor Character specifying the color for fully filled icons (default: "darkgray").
#' @param partialcolor Character specifying the color for partially filled icons (default: "white").
#' @param emptyhumans Logical indicating whether to include empty icons (default: TRUE).
#' @param emptycolor Character specifying the color for empty icons (default: "white").
#' @param infogs Integer specifying the total number of icons in the grid (default: `default_ncols`).
#' @param infogs_ncol Integer specifying the number of columns in the icon grid (default: `default_ncols`).
#' @param fillHoriz Logical indicating whether the icons should fill horizontally (default: FALSE).
#'
#' @return A ggplot object representing the grid of icons for the homepage.
#' @examples
#' # Generate a homepage icon grid for an RRI of 3.5
#' fnc_create_icons_homepage(rri_raw = 3.5, fillcolor = "blue")
fnc_create_icons_homepage <- function(rri_raw, rri_digits = 1, fillcolor = "darkgray", partialcolor = "white",
                                      emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                                      infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value to the specified number of digits
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partially filled icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options(
    partialval = numremain,  # Partial fill value for partially filled icons
    empty = emptycolor,      # Color for empty icons
    fill = fillcolor,        # Color for fully filled icons
    partial = partialcolor,  # Color for partially filled icons
    fillHoriz = fillHoriz    # Direction of the fill (horizontal or vertical)
  )

  # Initialize a list to store the plots
  plot_list <- list()

  # Set the first icon to a distinct color (e.g., green)
  first_icon_color <- color4  # Customize this color as needed
  first_icon_opts <- fnc_icon_options(
    partialval = 0,          # No partial fill for the first icon
    empty = emptycolor,      # Color for empty background
    fill = first_icon_color, # Color for the first icon
    partial = first_icon_color, # Use the same color for partial fill
    fillHoriz = fillHoriz    # Direction of the fill
  )
  plot_list[[1]] <- first_icon_opts$full  # Add the first icon to the list

  # Create fully filled icons in gray for the remaining RRI value
  for (i in 2:numfull) {
    plot_list[[i]] <- plot_opts$full
  }

  # Add a partially filled icon if the RRI has a fractional part
  if (numremain > 0) {
    plot_list[[numfull + 1]] <- plot_opts$partial
  }

  # Add empty icons to complete the grid if needed
  if (emptyhumans && length(plot_list) < infogs) {
    for (i in (numfull + 2):infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(length(plot_list) / infogs_ncol), 1)

  # Return the grid of icons as a ggplot object
  plot_grid(plotlist = plot_list, nrow = rows)
}

