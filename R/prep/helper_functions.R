#-------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#-------------------------------------------------------------------------------

#' Read Data and Add Year Column
#'
#' This function reads in a Stata file, extracts the year from the file name,
#' and adds a `rptyear` column with the extracted year. It also removes labels from the
#' `state_encoded` column, if it exists, to avoid conflicts during analysis.
#'
#' @param file_path The file path of the Stata file to read.
#' @return A data frame with the data from the Stata file, with an additional `rptyear` column,
#' and the `state_encoded` column converted to numeric, if present.
#' @examples
#' \dontrun{
#' fnc_read_and_add_year("path_to_file.dta")
#' }
fnc_read_and_add_year <- function(file_path) {
  # Read the data from Stata file
  data <- read_dta(file_path)

  # Extract year from file name using regular expression
  year <- sub(".*_(\\d{4})_.*", "\\1", file_path)

  # Add extracted year as rptyear column
  data <- data %>% mutate(rptyear = as.numeric(year))

  # Remove labels from state_encoded, if it exists
  if("state_encoded" %in% colnames(data)) {
    data$state_encoded <- as.numeric(data$state_encoded)
  }

  return(data)
}

# Test: Ensure that 'rptyear' is added correctly and 'state_encoded' is numeric
# test_df <- fnc_read_and_add_year("sample_2023_data.dta")
# stopifnot("rptyear" %in% colnames(test_df))
# stopifnot(is.numeric(test_df$state_encoded))

#' Apply Factor Levels to a Column
#'
#' This function applies specific factor levels to a specified column in a data frame.
#' It is useful for converting categorical variables into factors with specified levels.
#'
#' @param df A data frame containing the column to be factored.
#' @param col_name The name of the column to be factored.
#' @param levels The levels to apply to the factor.
#' @return A data frame with the specified column transformed into a factor with the given levels.
#' @examples
#' df <- data.frame(var = c("A", "B", "C"))
#' fnc_apply_factor_levels(df, var, c("A", "B", "C"))
fnc_apply_factor_levels <- function(df, col_name, levels) {
  df |>
    mutate({{col_name}} := factor({{col_name}}, levels = levels))
}

# Test: Ensure the column is factored correctly with the specified levels
# test_df <- data.frame(var = c("A", "B", "C"))
# test_df <- fnc_apply_factor_levels(test_df, var, c("A", "B", "C"))
# stopifnot(is.factor(test_df$var))
# stopifnot(all(levels(test_df$var) == c("A", "B", "C")))

#' Create FBI Index Category Column
#'
#' This function categorizes offenses into FBI index categories based on the `offdetail` column
#' and applies factor levels to the resulting `fbi_index` column. It standardizes offense types
#' into broader categories such as "Murder and Non-negligent Manslaughter", "Robbery", etc.
#'
#' @param df A data frame containing an `offdetail` column with specific offense details.
#' @return A data frame with a new `fbi_index` column, categorized and factored into FBI index categories.
#' @examples
#' \dontrun{
#' fnc_create_fbi_index(df)
#' }
fnc_create_fbi_index <- function(df) {
  df |>
    mutate(fbi_index = case_when(
      offdetail == "Aggravated or simple assault" ~ "Aggravated or Simple Assault",
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder and Non-negligent Manslaughter",
      offdetail == "Negligent manslaughter" ~ "Negligent Manslaughter",
      offdetail == "Other violent offenses" ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
      offdetail == "Robbery" ~ "Robbery",
      offdetail == "Other/unspecified" ~ "Other or Unknown",
      is.na(offdetail) ~ "Other or Unknown",
      TRUE ~ offgeneral
    )) |>
    fnc_apply_factor_levels(fbi_index, c("Murder and Non-negligent Manslaughter", "Negligent Manslaughter",
                                     "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
                                     "Other Violent Offenses", "Property", "Public order", "Drugs", "Other", "Unknown"))
}

# Test: Ensure that 'fbi_index' is correctly categorized and factored
# test_df <- data.frame(offdetail = c("Aggravated or simple assault", "Murder (including non-negligent manslaughter)", NA))
# test_df <- fnc_create_fbi_index(test_df)
# stopifnot(all(levels(test_df$fbi_index) == c("Murder and Non-negligent Manslaughter", "Negligent Manslaughter", "Rape or Sexual Assault",
#                                              "Robbery", "Aggravated or Simple Assault", "Other Violent Offenses", "Property",
#                                              "Public order", "Drugs", "Other", "Unknown")))

#' Create Admission Type Categories
#'
#' This function standardizes the `admtype` column by recategorizing various admission types into
#' simplified categories such as "New court commitment", "Parole return/revocation", and "Other or Unknown".
#' It also applies factor levels to the `admtype` column.
#'
#' @param df A data frame containing an `admtype` column.
#' @return A data frame with the recategorized and factored `admtype` column.
#' @examples
#' \dontrun{
#' fnc_create_admtype(df)
#' }
fnc_create_admtype <- function(df) {
  df |>
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other or Unknown",
      is.na(admtype) ~ "Other or Unknown",
      TRUE ~ admtype
    )) |>
    fnc_apply_factor_levels(admtype, c("New court commitment", "Parole return/revocation", "Other or Unknown"))
}

# Test: Ensure that 'admtype' is correctly categorized and factored
# test_df <- data.frame(admtype = c("Other admission (including unsentenced, transfer, AWOL/escapee return)", NA))
# test_df <- fnc_create_admtype(test_df)
# stopifnot(all(levels(test_df$admtype) == c("New court commitment", "Parole return/revocation", "Other or Unknown")))

#' Clean Bureau of Justice Statistics (BJS) Data
#'
#' This function cleans BJS data by removing or correcting invalid state names,
#' filtering out unwanted rows, and cleaning the `bjs_prison_population` column by removing
#' non-numeric characters and converting it to numeric.
#'
#' @param df A data frame containing the BJS data. It must have `state` and `bjs_prison_population` columns.
#' @return A cleaned data frame with corrected state names and numeric prison population values.
#' @examples
#' \dontrun{
#' df_cleaned <- fnc_clean_bjs_data(df)
#' }
fnc_clean_bjs_data <- function(df) {
  df <- df |>
    # Remove anything after the state name in the `state` column
    mutate(state = str_replace(state, "/.*", "")) |>
    # Correct specific misspelled state names
    mutate(state = str_replace(state, "Alaskab", "Alaska")) |>
    mutate(state = str_replace(state, "Utahc", "Utah")) |>
    # Filter out invalid state names and totals
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") |>
    # Remove non-numeric characters from `bjs_prison_population` and convert it to numeric
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) |>
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))

  return(df)
}

# Test: Ensure that the function correctly cleans state names and converts prison population to numeric
# test_df <- data.frame(state = c("Alaskab", "Utahc", "Federal", "U.S. Total", "California"),
#                      bjs_prison_population = c("1,000", "2,000", "3,000", "4,000", "5,000"))
# clean_df <- fnc_clean_bjs_data(test_df)
# stopifnot(all(clean_df$state == c("Alaska", "Utah", "California")))
# stopifnot(all(clean_df$bjs_prison_population == c(1000, 2000, 5000)))




#-------------------------------------------------------------------------------
# DATA ANALYSIS FUNCTIONS
#-------------------------------------------------------------------------------

#' Filter Population Based on Abolished Parole Status
#'
#' This function filters the input data to only include states that have not abolished parole.
#' It references an external dataset (`carl_state_notes`) to determine which states have abolished parole.
#'
#' @param data A data frame containing the population data, which must include a `state` column.
#'
#' @return A filtered data frame that only contains data for states where parole has not been abolished.
#' @export
#'
#' @examples
#' # Example usage:
#' filtered_data <- fnc_filter_population(population_data)
fnc_filter_population <- function(data) {
  # Get states that have not abolished parole
  abolished <- carl_state_notes |>
    filter(abolished_parole_16_total == "N") |>
    pull(state)

  # Filter data based on the admission type, sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(state %in% abolished)  # Only keep states that did not abolish parole

  return(filtered_data)
}

#' Retrieve and Process Census Data for a Given State
#'
#' This function retrieves decennial census data for a specific state using the `tidycensus` package.
#' It processes the data by cleaning column names and categorizing race variables into broader groups.
#'
#' @param state A string representing the state for which the census data is to be retrieved.
#'
#' @return A data frame containing census data for the specified state with processed race categories.
#' @export
#'
#' @examples
#' # Example usage:
#' census_data <- fnc_get_census_data("NY")
fnc_get_census_data <- function(state) {
  df <-
    tidycensus::get_decennial(
      geography = "state",
      state = state,
      variables = race_vars,
      summary_var = "P3_001N",
      year = select_year,
      geometry = FALSE) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race = case_when(
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    )
  return(df)
}


#' Filter Population Criteria for Analysis
#'
#' This function filters a dataset of prison admissions based on specific criteria,
#' including admission type, sentence lengths, and whether the state has abolished parole.
#'
#' @param data A data frame containing prison admissions data. It must have columns for `admtype`, `sentlgth`, and `state`.
#' @param admtype_filter The type of admission to filter by. Defaults to "New court commitment".
#' @param sentence_lengths A vector of sentence lengths to filter by. Defaults to c("1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years").
#' @return A filtered data frame based on the specified criteria.
#' @examples
#' \dontrun{
#' filtered_data <- filter_population_criteria(prison_data)
#' }
fnc_filter_pe_population_criteria <- function(data,
                                       admtype_filter = "New court commitment",
                                       sentence_lengths = c("1-1.9 years",
                                                            "2-4.9 years",
                                                            "5-9.9 years",
                                                            "10-24.9 years")) {
  # Get states that have not abolished parole
  abolished <- carl_state_notes |>
    filter(abolished_parole_16_total == "N") |>
    pull(state)

  # Filter data based on the admission type, sentence lengths, and states that did not abolish parole
  filtered_data <- data |>
    filter(admtype == admtype_filter) |>
    filter(sentlgth %in% sentence_lengths) |>
    filter(state %in% abolished)  # Only keep states that did not abolish parole

  return(filtered_data)
}

# Test: Ensure the filtering works correctly
# test_data <- data.frame(
#   admtype = c("New court commitment", "Parole return/revocation"),
#   sentlgth = c("1-1.9 years", "10-24.9 years"),
#   state = c("California", "Texas")
# )
# carl_state_notes <- data.frame(
#   state = c("California", "Texas"),
#   abolished_parole_16_total = c("N", "Y")
# )
# filtered_df <- filter_population_criteria(test_data)
# stopifnot(nrow(filtered_df) == 1)  # Only one row should remain after filtering


#' Prepare Parole Eligibility Data for Visualization
#'
#' This function filters, groups, and aggregates data for parole eligibility based on specific conditions
#' such as report year, admission type, and sentence length. It calculates the proportion of the
#' population that is parole-eligible, and adds labels for visualization.
#'
#' @param df A data frame containing the parole eligibility data.
#' It should include columns for `rptyear`, `parelig_status`, `admtype`, `sentlgth`, and `state`.
#' @param count_column The name of the column to use for counting and grouping the data (e.g., parole eligibility).
#' @return A data frame grouped by state with proportions and labeled columns for use in visualizations.
#' @examples
#' \dontrun{
#' fnc_prepare_pe_data(df, count_column = "parole_eligibility_status")
#' }
fnc_prepare_pe_data <- function(df, count_column) {
  df1 <- df |>
    # Filter for the selected year and 'Current' parole eligibility status
    filter(rptyear == select_year & parelig_status == "Current") |>
    # Further filter for the "New court commitment" admission type
    filter(admtype == "New court commitment") |>
    # Filter for specific sentence lengths
    filter(sentlgth == "1-1.9 years" |
             sentlgth == "2-4.9 years" |
             sentlgth == "5-9.9 years" |
             sentlgth == "10-24.9 years") |>
    # Group by state and count occurrences of the specified column
    group_by(state) |>
    filter(!is.na({{ count_column }})) |>
    count({{ count_column }}) |>
    # Calculate proportions and create labels for visualization
    mutate(
      prop = n/sum(n),                    # Calculate proportion
      yearendpop_ped = sum(n),            # Calculate total population
      prop_label = paste0(round(prop * 100, 0), "%"),  # Create proportion label as percentage
      n_label = formattable::comma(n, 0)  # Format count labels with commas
    ) |>
    ungroup()

  return(df1)
}








#-------------------------------------------------------------------------------
# DATA VISUALIZATION FUNCTIONS
#-------------------------------------------------------------------------------

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
  fontSize = "12px",
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
  fontSize = "12px",
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
  title = list(align = alignment, style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = alignment, style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(
    align = alignment,
    verticalAlign = "top",
    itemStyle = common_style
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "transparent",
    lineColor = "black",
    minorGridLineColor = "transparent",
    tickColor = "black"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE), dataLabels = list(style = common_style)),
    spline = list(marker = list(enabled = FALSE), dataLabels = list(style = common_style)),
    area = list(marker = list(enabled = FALSE), dataLabels = list(style = common_style)),
    areaspline = list(marker = list(enabled = FALSE), dataLabels = list(style = common_style)),
    arearange = list(marker = list(enabled = FALSE), dataLabels = list(style = common_style)),
    bubble = list(maxSize = "10%", dataLabels = list(style = common_style)),
    column = list(
      dataLabels = list(
        style = common_style
      )
    )
  )
)

#' Highcharts Theme with Line Marker
#'
#' This theme includes a Highcharts configuration with enabled markers for
#' line, spline, area, and bubble charts, with custom styling.
#' @export
hc_theme_with_line <- hc_theme(
  colors = c(color1, color2, color3, color4, color5),
  chart = list(style = common_chart_style),
  title = list(align = alignment, style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = alignment, style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(align = alignment, verticalAlign = "top", itemStyle = common_style),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    tickmarkPlacement = 'on',
    tickLength = 5,
    tickWidth = 1,
    tickColor = "black",
    lineColor = "black"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = common_style)
  ),
  plotOptions = list(
    line = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'
      )
    ),
    spline = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'
      )
    ),
    area = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'
      )
    ),
    areaspline = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'
      )
    ),
    arearange = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'
      )
    ),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = "black")
      )
    )
  )
)

#' Highcharts Theme for Maps
#'
#' This theme is specifically designed for maps in Highcharts.
#' It modifies the base theme to include larger titles and font sizes
#' while adjusting the inactive series opacity for better clarity.
#' @export
hc_theme_map <- hc_theme_merge(
  hc_theme_smpl(),
  base_hc_theme,
  hc_theme(
    chart = list(style = modifyList(common_chart_style, list(fontSize = "14px"))),
    title = list(align = alignment, style = modifyList(common_title_style, list(fontSize = "22px"))),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    ),
    legend = list(
      itemStyle = modifyList(common_style, list(fontSize = "16px"))
    )
  )
)

#' Generate Highcharts Stacked Bar Chart for Parole-Eligible Population
#'
#' This function generates a stacked bar chart in Highcharts representing
#' parole-eligible population categories (Missing or Not Parole-Eligible, Future, and Current).
#'
#' @param df Dataframe containing the input data
#' @param count_column Column name for the population count
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param categories_col Column name for the chart categories
#' @param colors Vector of colors for the different stacked series
#'
#' @return A Highcharts stacked bar chart object
#' @export
fnc_hc_stackedbar_pe_population <- function(df, count_column, title, subtitle, categories_col, colors) {
  data <- df |>
    filter(state == unique(df$state)) |>
    arrange(desc({{ count_column }}))

  highcharts <- highchart() |>
    hc_chart(type = "column") |>
    hc_title(text = title) |>
    hc_subtitle(text = subtitle) |>
    hc_xAxis(categories = unique(data[[categories_col]])) |>
    hc_yAxis(
      title = list(text = ""),
      min = 0,
      max = 1,
      labels = list(
        formatter = JS("function () { return Math.round(this.value * 100) + '%'; }")
      )
    ) |>
    hc_plotOptions(series = list(stacking = "normal")) |>
    hc_tooltip(formatter = JS("function() { return this.point.tooltip; }")) |>
    hc_add_series(data = data |> filter(parelig_status == "Missing or Not Parole-Eligible") |>
                    select({{ count_column }}, prop, tooltip) |>
                    rename(y = prop),
                  name = "Missing or Not Parole-Eligible",
                  color = colors[1]) |>
    hc_add_series(data = data |> filter(parelig_status == "Future") |>
                    select({{ count_column }}, prop, tooltip) |>
                    rename(y = prop),
                  name = "Future",
                  color = colors[2]) |>
    hc_add_series(data = data |> filter(parelig_status == "Current") |>
                    select({{ count_column }}, prop, tooltip) |>
                    rename(y = prop),
                  name = "Current",
                  color = colors[3]) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE, reversed = TRUE)

  return(highcharts)
}

#' Generate Highcharts Column Chart
#'
#' This function generates a column chart in Highcharts with custom styling
#' for the x and y axes, data labels, tooltips, and accessibility features.
#'
#' @param df Dataframe containing the input data
#' @param x_var Column name for the x-axis variable
#' @param y_var Column name for the y-axis variable
#' @param accessibility_text Accessibility text description for the chart
#'
#' @return A Highcharts column chart object
#' @export
fnc_hc_columnchart <- function(df, x_var, y_var, accessibility_text) {

  xaxis_order <- df[[x_var]]

  highcharts <- highchart() |>
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_var),
                        y = !!sym(y_var)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) |>
    # hc_xAxis(categories = xaxis_order,
    #          labels = list(
    #            style = list(fontSize = "1em", fontFamily = "Graphik")
    #          )) |>
    hc_xAxis(categories = xaxis_order,
             labels = list(
               formatter = JS(
                 "function() {
                    var label = this.value;
                    var maxLength = 15;
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
               ),
               style = list(fontSize = "1em", fontFamily = "Graphik")
             )) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
                  return this.value + '%';
                }")
             )) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)
}




















