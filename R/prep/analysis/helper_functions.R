
# ---------------------------------------------------------------------------- #
# Analysis Helper Functions
# ---------------------------------------------------------------------------- #

#' Filter Prison Population Based on Parole Eligibility Criteria
#'
#' This function filters the prison population data to include only individuals
#' who meet specific criteria related to admission type and sentence length.
#' It also excludes states with high missingness or abolished parole systems
#' and skips filtering for states that don't require these criteria.
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
        (admtype == "New court commitment" & # Filter for "New court commitment" admission type
           sentlgth %in% c("1-1.9 years", "2-4.9 years", # Include specific sentence length categories
                           "5-9.9 years", "10-24.9 years", ">=25 years"))
    )

  # Return the filtered dataset
  return(filtered_data)
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
      !is.na(!!count_column) &                                   # Exclude missing values
        (!(deparse(substitute(count_column)) != "race" &           # For non-"race" columns:
             (!!count_column == "Unknown")))                         # Exclude "Unknown".
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
fnc_hc_pie_chart <- function(df, variable, source = ncrp_csg_source) {
  # Get unique states from the data
  states <- unique(df$state)

  # Iterate over each state to generate pie charts
  all_pie_charts <- map(states, function(state_name) {
    # Filter the data for the current state
    df1 <- df |>
      ungroup() |> # Remove grouping to ensure accurate filtering
      filter(state == state_name) |> # Select data for the current state
      mutate(color = case_when( # Assign colors based on parole eligibility status
        parelig_status == "Future" ~ color2,
        parelig_status == "Missing" ~ darkgray,
        parelig_status == "Current" ~ color4
      ))

    # Extract the reporting year for the current state (assumes it's consistent within the state)
    select_year <- unique(df1$rptyear)

    # Generate descriptive accessibility text for the pie chart
    category_counts <- df1 |>
      group_by(!!sym(variable)) |> # Group by the specified variable
      summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |> # Calculate percentage for each category
      arrange(desc(percentage)) # Sort categories by descending percentage

    # Build a textual description of the chart for accessibility
    accessibility_text <- paste(
      "This pie chart shows the distribution of the prison population by", variable, "in", select_year, ".",
      paste(
        category_counts |>
          transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |> # Combine category and percentage
          pull(text), # Extract the formatted text
        collapse = ", " # Join all categories into a single string
      )
    )

    # Create the Highcharts pie chart
    highchart() |>
      hc_chart(type = "pie") |>
      hc_plotOptions(pie = list(
        dataLabels = list( # Define label formatting for the chart
          enabled = TRUE,
          format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
        ),
        colorByPoint = FALSE # Use custom colors defined in the data
      )) |>
      hc_series(list( # Add data to the chart
        data = list_parse(df1 |> mutate(y = n) |> transmute(
          name = !!sym(variable), y, color, tooltip
        ))
      )) |>
      hc_add_theme(base_hc_theme) |> # Add a base theme for consistency
      hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |> # Custom tooltip formatting
      hc_title(text = paste0("Prison Population by Parole Eligibility Status, ", select_year)) |> # Chart title
      hc_exporting(enabled = TRUE, filename = paste0("prison_population_", state_name, "_", select_year)) |> # Enable export
      hc_caption(text = source) |> # Add chart caption with source information
      fnc_add_hc_accessibility(accessibility_text) # Add accessibility text
  })

  # Assign state names to the charts list for clarity
  all_pie_charts <- setNames(all_pie_charts, states)

  return(all_pie_charts)
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
fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
                               source = ncrp_csg_source, orientation = "vertical") {

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

  # Construct the chart title
  title <- paste0(title_type, " by ", metric)

  # Generate accessibility text describing the chart
  accessibility_text <- paste0("This graph shows the percentage of ", type,
                               " by ", tolower(metric), " in ",
                               year, " in the state of ", state_var, ".")

  # Define the x-axis order based on the data
  xaxis_order <- df1[[x_var]]

  # Determine chart type based on orientation
  chart_type <- ifelse(orientation == "horizontal", "bar", "column")

  # Adjust label alignment for horizontal orientation
  label_alignment <- ifelse(orientation == "horizontal", "right", "center")

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
               formatter = JS(js_code), # Format labels with JavaScript
               style = list(fontSize = "14px", fontFamily = "Graphik",
                            textAlign = label_alignment) # Align labels based on orientation
             )) |>
    hc_yAxis(max = 100, # Set y-axis maximum to 100% for proportions
             labels = list(
               formatter = JS("function() { return this.value + '%'; }") # Append % to y-axis labels
             )) |>
    hc_add_theme(base_hc_theme) |> # Apply the base theme
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> # Add custom tooltip formatter
    hc_legend(enabled = FALSE) |> # Disable the legend
    hc_title(text = paste0(title, ", ", year)) |> # Add the chart title
    hc_exporting(enabled = TRUE, # Enable exporting functionality
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", year)) |>
    fnc_add_hc_accessibility(accessibility_text) |> # Add accessibility text
    hc_caption(text = source) # Add source caption

  return(highcharts) # Return the generated Highchart
}



# ---------------------------------------------------------------------------- #
# Sentences and Visualization Helper Functions
# ---------------------------------------------------------------------------- #

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

  # Extract years with valid past and projected data
  valid_past_years <- state_data |> filter(!is.na(pct_past_pe)) |> pull(year)
  valid_proj_years <- state_data |> filter(!is.na(proj_pct_past_pe)) |> pull(year)

  # Determine earliest and latest years for past and projected data
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
  change_proj <- if (!is.na(proj_earliest) && !is.na(proj_latest)) {
    round(((proj_latest - proj_earliest) / proj_earliest) * 100, 0)
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
        "We've projected that from ", earliest_year_proj, " to ", latest_year_proj,
        ", the percent of people past parole eligibility ",
        if (!is.na(change_proj)) {
          if (change_proj > 0) paste0("will increase by ", change_proj, " percent")
          else if (change_proj < 0) paste0("will decrease by ", abs(change_proj), " percent")
          else "will not change (0 percent change)"
        } else "has insufficient data to project a change",
        "."
      )
    } else "Projected data is insufficient to provide a future change.",
    note
  )

  return(sentence)
}

#' Generate Bar Charts for Multiple States
#'
#' Creates a collection of bar charts for each state based on the input data,
#' visualizing a specified metric grouped by a given variable.
#'
#' @param data A data frame containing the data to visualize, with a `state` column.
#' @param x_var A string representing the variable to use on the x-axis (e.g., "fbi_index").
#' @param metric A string representing the label for the metric being visualized.
#' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
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
fnc_generate_bar_charts <- function(data, x_var, metric, type_desc, title_type, y_var = "prop", source) {
  # Extract unique states from the data
  states <- unique(data$state)

  # Generate charts for each state
  charts <- map(states, function(state_name) {
    # Determine chart orientation dynamically
    orientation <- if (x_var == "fbi_index") "horizontal" else "vertical"

    # Call the column chart creation function for each state
    fnc_hc_columnchart(
      state_var  = state_name,   # Current state
      df         = data,        # Filtered data
      x_var      = x_var,       # X-axis variable
      y_var      = y_var,       # Y-axis variable (default: "prop")
      metric     = metric,      # Metric label
      type       = type_desc,   # Type description (e.g., "Releases")
      title_type = title_type,  # Title prefix
      source     = source,      # Chart source
      orientation = orientation # Determine horizontal or vertical orientation
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
#' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
#' @return A named list of sentences, one for each state.
#' @details
#' - Uses `fnc_generate_columnchart_sentence` to create state-specific summaries.
#' @examples
#' sentences <- fnc_generate_sentences(data, "fbi_index", "Releases")
#' @export
fnc_generate_sentences <- function(data, x_var, type_desc) {
  # Extract unique states from the data
  states <- unique(data$state)

  # Generate sentences for each state
  sentences <- map(states, function(state_name) {
    # Call the sentence generation function for each state
    fnc_generate_columnchart_sentence(
      state_var = state_name, # Current state
      df        = data,      # Filtered data
      x_var     = x_var,     # X-axis variable for grouping
      type      = type_desc  # Type description (e.g., "Releases")
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
#' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
#' @return A string summarizing trends or distributions for the specified state and variable.
#' @details
#' - Handles special cases for `fbi_index`, `sex`, age-related variables, and sentence length.
#' - Dynamically adjusts wording and formatting based on the input variable.
#' - Ensures robust handling of missing or incomplete data.
#' @examples
#' sentence <- fnc_generate_columnchart_sentence("Georgia", data, "fbi_index", "Releases")
#' @export
fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type_desc) {
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

  # Special handling for "fbi_index" variable
  if (x_var == "fbi_index") {
    # Identify the top categories based on the highest proportion
    max_prop <- max(round(df1$prop, 0))
    top_categories <- df1 |>
      filter(round(prop, 0) == max_prop) |>
      arrange(desc(prop))

    # Create sentences for top offense categories
    fbi_sentences <- top_categories |>
      mutate(fbi_sentence = paste0(tolower(fbi_index), " (", round(prop, 0), " percent)")) |>
      pull(fbi_sentence)

    # Format the final sentence using commas and "and" for readability
    fbi_sentence_final <- if (length(fbi_sentences) > 1) {
      paste(paste(fbi_sentences[-length(fbi_sentences)], collapse = ", "),
            ", and ", fbi_sentences[length(fbi_sentences)], sep = "")
    } else {
      fbi_sentences
    }

    # Summarize violent and nonviolent offense proportions
    current_ped_offense_group <- df |>
      select(state, fbi_index, offense_group, n) |>
      filter(offense_group == "Violent" | offense_group == "Nonviolent") |>
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

    # Construct the final sentence for "fbi_index"
    sentences <- paste0("In ", year, ", ", violent_prop, " percent of people ", type_desc,
                        " were in prison for violent offenses and ",
                        nonviolent_prop, " percent for nonviolent offenses. ",
                        "Most people ", type_desc, " were incarcerated for ", fbi_sentence_final, " offenses.")
  }
  # Special handling for age-related variables
  else if (x_var == "ageyrend" | x_var == "agerlse") {
    age_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type_desc, " were between the ages of ",
                        age_range[1], " and ", age_range[2], " old.")
  }
  # Special handling for sentence length variables
  else if (x_var == "sentlgth") {
    sent_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type_desc, " had sentence lengths between ",
                        sent_range[1], " and ", sent_range[2], ".")
  }
  # General case for other variables
  else {
    sentences <- paste0("In ", year, ", ", round(df1$prop[1], 0),
                        " percent of people ", type_desc, " were ",
                        df1[[x_var]][1], ".")
  }

  return(sentences)
}






