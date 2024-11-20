
#-------------------------------------------------------------------------------
# CHECKED FUNCTIONS
#-------------------------------------------------------------------------------

#' Filter Parole Eligibility Population
#'
#' Filters the input data based on specific parole eligibility criteria such as admission type,
#' sentence length, and exclusion of states with missing data or abolished parole.
#'
#' @param data A data frame containing population data to filter.
#'
#' @return A filtered data frame based on parole eligibility criteria.
#' @export
fnc_filter_pe_population_criteria <- function(data, exclude, dont_filter) {
  # Get states to exclude - missing data and abolished parole
  exclude <- exclude |>
    pull(state)

  # Get states that don't need to filter admtype and sentlgth
  dont_filter <- dont_filter |>
    pull(state)

  # Filter data based on criteria, applying admtype and sentlgth filters only if state is not in dont_filter
  filtered_data <- data |>
    filter(!(state %in% exclude)) |>
    filter(
      (state %in% dont_filter) | # Skip filtering if in dont_filter
        (admtype == "New court commitment" & sentlgth %in% c("1-1.9 years",
                                                             "2-4.9 years",
                                                             "5-9.9 years",
                                                             "10-24.9 years",
                                                             ">=25 years"))
    )

  # Return the filtered data
  return(filtered_data)
}

fnc_filter_by_year <- function(df, which_state_year) {
  df |>
    # Join with `which_state_year` to add `year_to_use`
    left_join(which_state_year, by = "state") |>
    # Filter rows where `rptyear` matches `year_to_use`
    filter(rptyear == year_to_use)
}

#' Create Tooltip for Highcharts
#'
#' Creates a tooltip for a Highchart object by formatting the variable label,
#' number of people, and the percentage of people in the population.
#'
#' @param df A data frame containing the data to be used for tooltips.
#' @param variable_label A string representing the label of the variable to be displayed in the tooltip.
#' @param variable A column in the data frame representing the variable used in the tooltip.
#'
#' @return A data frame with an added tooltip column.
#' @export
fnc_create_tooltip <- function(df, variable_label, variable) {
  df |>
    dplyr::mutate(
      tooltip = paste0(
        "<b>", variable_label, ":</b> ", {{ variable }}, "<br>",
        "<b>People:</b> ", formattable::comma(n, 0), "<br>",
        "<b>Percentage of People:</b> ", round(prop, 0), "%"
      )
    )
}


























#-------------------------------------------------------------------------------
# Analysis Functions
#-------------------------------------------------------------------------------

# Function to determine select_year based on the state
fnc_determine_select_year <- function(state_name, which_overall_year) {
  # Filter for the specified state and pull the year_to_use
  select_year <- which_overall_year |>
    filter(state == state_name) |>
    pull(year_to_use)

  # Return the selected year
  if (length(select_year) == 0) {
    stop("State not found in which_overall_year data.")
  }

  return(select_year)
}

#' Filter Population Data
#'
#' Filters the input data by excluding states with missing data and states that have abolished parole.
#'
#' @param data A data frame containing population data to filter.
#'
#' @return A filtered data frame with states that did not abolish parole and have valid data.
#' @export
fnc_filter_population <- function(data, exclude) {
  # Get states to exclude - missing data and abolished parole
  exclude <- exclude |>
    pull(state)

  # Filter data based on the admission type, sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(!(state %in% exclude))  # Only keep states that did not abolish parole

  return(filtered_data)
}







#' Filter Data by Excluding States with High Missing Race Data
#'
#' Filters the input data by excluding states that have high levels of missing race data.
#'
#' @param data A data frame containing population data.
#' @param states_with_high_missing_race A list or vector of states to exclude due to high missing race data.
#'
#' @return A filtered data frame with states with high missing race data excluded.
#' @export
fnc_filter_exclude_high_missing_race <- function(data, states_with_high_missing_race) {
  # Convert to character vector if it's a list
  if (is.list(states_with_high_missing_race)) {
    states_with_high_missing_race <- unlist(states_with_high_missing_race)
  }

  # Debugging step: Print the list of states to be excluded
  print("States with high missing race data:")
  print(states_with_high_missing_race)

  # Ensure both 'state' in data and 'states_with_high_missing_race' are in the same format
  filtered_data <- data |>
    filter(!(state %in% states_with_high_missing_race))

  # Return the filtered data
  return(filtered_data)
}


#' Summarize Data by Count and Proportion
#'
#' Summarizes the input data by counting the occurrences of a specified column, calculating proportions,
#' and formatting the labels for use in visualizations.
#'
#' @param df A data frame containing the data to summarize.
#' @param count_column The column to count occurrences for summarization.
#' @param year The year to filter data by.
#'
#' @return A summarized data frame with counts, proportions, and formatted labels.
#' @export
# fnc_summarize_data <- function(df, count_column, year) {
#   count_column <- sym(count_column)  # Convert the string column name to a symbol
#
#   df1 <- df |>
#     filter(rptyear == year) |>
#     group_by(state) |>
#
#     # Conditionally exclude "Unknown" only if the count_column is not "race"
#     filter(!is.na(!!count_column) &
#              (!(deparse(substitute(count_column)) != "race" & (!!count_column == "Unknown")))) |>
#
#     count(!!count_column) |>
#
#     # Calculate proportions and create labels for visualization
#     mutate(
#       prop = (n / sum(n)) * 100,                # Calculate proportion
#       n_total = sum(n),                         # Calculate total population
#       prop_label = paste0(round(prop, 0), "%"), # Create proportion label as percentage
#       n_label = formattable::comma(n, 0)        # Format count labels with commas
#     ) |>
#     ungroup()
#
#   return(df1)
# }
fnc_summarize_data <- function(df, count_column, year_df) {
  count_column <- sym(count_column)  # Convert the string column name to a symbol

  # Join df with year_df to get the correct year for each state
  df <- df |>
    left_join(year_df, by = "state")

  df1 <- df |>
    filter(rptyear == year_to_use) |>
    group_by(state) |>

    # Conditionally exclude "Unknown" only if the count_column is not "race"
    filter(!is.na(!!count_column) &
             (!(deparse(substitute(count_column)) != "race" & (!!count_column == "Unknown")))) |>

    count(!!count_column) |>

    # Calculate proportions and create labels for visualization
    mutate(
      prop = (n / sum(n)) * 100,                # Calculate proportion
      n_total = sum(n),                         # Calculate total population
      prop_label = paste0(round(prop, 0), "%"), # Create proportion label as percentage
      n_label = formattable::comma(n, 0)        # Format count labels with commas
    ) |>
    ungroup()

  return(df1)
}


#' Round Numbers to Nearest Power of 10
#'
#' Rounds numbers to the nearest power of 10 based on the number of digits.
#' If a number has 3 or more digits, it rounds down to the nearest power of 10 below the number.
#' For numbers with fewer than 3 digits, it rounds to the nearest 10.
#'
#' @param x A numeric vector of values to round.
#'
#' @return A numeric vector with values rounded to the nearest power of 10.
#' @export
# fnc_round_to_power <- function(x) {
#   sapply(x, function(val) {
#     # Check if the value is NA, and return NA if true
#     if (is.na(val)) {
#       return(NA)
#     }
#
#     # Determine the number of digits in the number
#     digits <- nchar(floor(val))
#
#     # Define the rounding level: if digits >= 3, round to the next power of 10 down, else round to 10
#     if (digits >= 3) {
#       power <- 10^(digits - 2) # This rounds down to the next power of 10 below the number
#       floor(val / power) * power  # Use floor to always round down
#     } else {
#       round(val, -1)
#     }
#   })
# }
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









#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Viz Functions

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


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


#' Generate Highcharts Pie Chart
#'
#' Creates a Highcharts pie chart with customized options such as title, tooltips,
#' accessibility features, and export functionality.
#'
#' @param df A data frame containing the data for the pie chart.
#' @param variable The column in the data frame representing the pie chart categories.
#' @param title A string representing the chart title.
#' @param accessibility_text A string of text for accessibility descriptions.
#' @param year The year for which the chart is being generated.
#' @param source The source of the data displayed in the chart.
#'
#' @return A Highcharts pie chart object.
#' @export
fnc_hc_pie <- function(df, variable, title, accessibility_text, year, source = ncrp_csg_source) {
  highchart() |>
    hc_chart(type = "pie") |>
    hc_plotOptions(pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
      ),
      colorByPoint = FALSE  # Disable automatic coloring by Highcharts
    )) |>
    hc_series(list(
      data = df |>
        mutate(y = n) |>
        transmute(
          name = !!sym(variable),  # Dynamically use the column passed
          y = y,
          color = color,
          tooltip = tooltip
        ) |>
        list_parse()  # Manually specify data with colors and tooltip
    )) |>
    hc_add_theme(base_hc_theme) |>
    hc_tooltip(formatter = JS("function () {return this.point.tooltip;}")) |>
    hc_title(text = paste0(title, ", ", year)) |>
    hc_exporting(enabled = TRUE,
                 filename = paste0(gsub(" ", "_", tolower(title)), "_", year)) |>
    hc_caption(text = source) |>
    fnc_add_hc_accessibility(accessibility_text)
}


#' Generate Highcharts Column Chart
#'
#' Generates a Highcharts column or bar chart based on the given parameters.
#'
#' @param state_var The state variable to filter the data.
#' @param df A data frame containing the data to plot.
#' @param x_var The x-axis variable for the chart.
#' @param y_var The y-axis variable for the chart.
#' @param metric The label for the metric displayed on the chart.
#' @param type The type of people represented in the chart (e.g., "people past parole eligibility").
#' @param title_type The type of chart title.
#' @param year The year for which the chart is being generated.
#' @param source The source of the data displayed in the chart.
#' @param orientation The chart orientation, either "vertical" (default) or "horizontal".
#'
#' @return A Highcharts column or bar chart object.
#' @export
fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
                               year, source = ncrp_csg_source, orientation = "vertical") {

  df1 <- df |>
    filter(state == state_var) |>
    fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var))

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


#' Generate Sentence for Column Chart
#'
#' Generates a descriptive sentence based on the top category in the column chart.
#'
#' @param state_var The state variable to filter the data.
#' @param df A data frame containing the data to analyze.
#' @param x_var The variable representing the categories in the chart.
#' @param type The type of people represented in the sentence (e.g., "people past parole eligibility").
#' @param year The year for which the sentence is being generated.
#'
#' @return A descriptive sentence about the top category in the column chart.
#' @export
fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type, year) {

  df1 <- df |>
    filter(state == state_var) |>
    arrange(-prop) |>
    slice(1)

  # Modify df1[[x_var]] to lowercase if x_var is "sex"
  if (x_var == "sex") {
    df1[[x_var]] <- tolower(df1[[x_var]])
  }

  # Check if x_var is "ageyrend" to format the sentence differently
  if (x_var == "ageyrend" | x_var == "agerlse") {
    age_range <- strsplit(as.character(df1[[x_var]]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
                        " percent of people ", type, " were between the ages of ",
                        age_range[1], " and ", age_range[2], " old.")
  } else if (x_var == "fbi_index") {
    # Lowercase the crime for proper sentence structure
    crime <- tolower(df1[[x_var]])
    sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
                        " percent of people ", type, " were incarcerated for ",
                        crime, " offenses.")
  } else if (x_var == "sentlgth") {
    # Split the sentence length range
    sent_range <- strsplit(as.character(df1[[x_var]]), "-")[[1]]
    sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
                        " percent of people ", type, " had sentence lengths between ",
                        sent_range[1], " and ", sent_range[2], ".")
  } else {
    sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
                        " percent of people ", type, " were ",
                        df1[[x_var]], ".")
  }

  return(sentences)
}





#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Disparities Functions

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------













