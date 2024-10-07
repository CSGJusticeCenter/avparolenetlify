



fnc_round_to_power <- function(x) {
  sapply(x, function(val) {
    # Check if the value is NA, and return NA if true
    if (is.na(val)) {
      return(NA)
    }

    # Determine the number of digits in the number
    digits <- nchar(floor(val))

    # Define the rounding level: if digits >= 3, round to the next power of 10 down, else round to 10
    if (digits >= 3) {
      power <- 10^(digits - 2) # This rounds down to the next power of 10 below the number
      floor(val / power) * power  # Use floor to always round down
    } else {
      round(val, -1)
    }
  })
}


#-------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#-------------------------------------------------------------------------------

#' Format citation text for Markdown
#'
#' This function formats a citation by italicizing the report title and converting URLs
#' into Markdown hyperlinks. It also ensures that any period after a URL is placed outside
#' the hyperlink for correct formatting.
#'
#' @param citation A character string containing the full citation with a report title and URL.
#'
#' @return A character string with the formatted citation for use in a Markdown document.
#' The report title is italicized, and the URL is converted into a clickable Markdown link.
#'
#' @examples
#' citation <- "Kevin R. Reitz, Allegra Lukac, and Edward E. Rhine,
#' Prison-Release Discretion and Prison Population Size: State Report:
#' Alabama (Robina Institute of Criminal Law and Criminal, February 2023),
#' https://robinainstitute.umn.edu/sites/robinainstitute.umn.edu/files/2023-02/alabama_doi_report_2_13_22.pdf."
#' formatted_citation <- format_citation(citation)
#' cat(formatted_citation)
#'
fnc_format_citation <- function(citation) {
  # Italicize the report title: Add * around report title
  formatted_citation <- str_replace_all(
    citation,
    "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
    function(x) paste0("*", x, "*")
  )

  # Convert URLs to markdown hyperlinks and ensure the period is outside the link
  formatted_citation <- str_replace_all(
    formatted_citation,
    "(https?://[^\\s]+)\\.",  # Match the URL pattern followed by a period
    function(x) {
      url <- str_remove(x, "\\.$")  # Remove the period from the URL
      paste0("[", url, "](", url, ").")  # Place the period outside the link
    }
  )

  return(formatted_citation)
}



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
#' into broader categories such as "Murder and Nonnegligent Manslaughter", "Robbery", etc.
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
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder or Nonnegligent Manslaughter",
      offdetail == "Negligent manslaughter" ~ "Negligent Manslaughter",
      offdetail == "Other violent offenses" ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
      offdetail == "Robbery" ~ "Robbery",
      offdetail == "Other/Unspecified" ~ "Other or Unspecified",
      offdetail == "Drugs (includes possession, distribution, trafficking, other)" ~ "Drug",
      is.na(offdetail) | offgeneral == "NA" ~ "Unknown",
      TRUE ~ offgeneral
    )) |>
    fnc_apply_factor_levels(fbi_index, c("Murder or Nonnegligent Manslaughter", "Negligent Manslaughter",
                                     "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
                                     "Other Violent Offenses", "Property", "Public order", "Drug", "Other or Unspecified", "Unknown"))
}


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
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other",
      is.na(admtype) ~ "Unknown",
      TRUE ~ admtype
    )) |>
    fnc_apply_factor_levels(admtype, c("New court commitment", "Parole return/revocation", "Other", "Unknown"))
}

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
  abolished <- state_notes |>
    filter(abolished_parole == "N") |>
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
# fnc_filter_pe_population_criteria <- function(data,
#                                               admtype_filter = "New court commitment") {
#   # Get states that have not abolished parole
#   abolished <- state_notes |>
#     filter(abolished_parole == "N") |>
#     pull(state)
#
#   # Filter data based on the admission type, valid sentence lengths (calc_sent_lgth_compl > 0), and states that did not abolish parole
#   filtered_data <- data |>
#     filter(admtype == admtype_filter) |>
#     filter(!is.na(calc_sent_lgth_compl) & calc_sent_lgth_compl > 0) |>
#     filter(state %in% abolished)  # Only keep states that did not abolish parole
#
#   return(filtered_data)
# }


#' Filter Population Data Based on Parole Eligibility Criteria
#'
#' This function filters a dataset based on specific criteria related to parole eligibility.
#' It filters by admission type, sentence length, and states that have not abolished parole.
#' Additionally, it calculates and prints diagnostic information about states with missing data
#' for the `admtype` and `calc_sent_lgth_compl` columns.
#'
#' @param data A data frame containing parole population data.
#' @param admtype_filter A string specifying the admission type to filter on. Defaults to "New court commitment".
#' @param missing_threshold A numeric value between 0 and 1 specifying the threshold for considering a state as having
#' high missing data. Defaults to 0.5 (i.e., 50%).
#'
#' @return A filtered data frame with the population meeting the specified criteria.
#' The function also prints diagnostic information about included states and states with high missing data.
#'
#' @details The function first filters the dataset to include only states that have not abolished parole.
#' It then applies additional filters based on admission type and sentence length.
#' Missing data rates for each state are calculated, and states with missing rates above the threshold are identified.
#' Diagnostic information about the included states and states with high missing data is printed to the console.
#'
#' @examples
#' # Assuming `ncrp_yearendpop` is a data frame with the necessary columns:
#' filtered_data <- fnc_filter_pe_population_criteria(ncrp_yearendpop)
#'
#' @export
# fnc_filter_pe_population_criteria <- function(data,
#                                               admtype_filter = "New court commitment",
#                                               missing_threshold = 0.5) {
#   # Get states that have not abolished parole
#   abolished <- state_notes |>
#     filter(abolished_parole == "N") |>
#     pull(state)
#
#   # Filter data based on the admission type, valid sentence lengths (calc_sent_lgth_compl > 0), and states that did not abolish parole
#   filtered_data <- data |>
#     filter(admtype == admtype_filter) |>
#     filter(!is.na(calc_sent_lgth_compl) & calc_sent_lgth_compl > 0) |>
#     filter(state %in% abolished)  # Only keep states that did not abolish parole
#
#   # Calculate missing rates for each state in admtype and calc_sent_lgth_compl
#   missing_summary <- data |>
#     group_by(state) |>
#     summarize(
#       admtype_missing_rate = sum(is.na(admtype)) / n(),
#       calc_sent_lgth_missing_rate = sum(is.na(calc_sent_lgth_compl)) / n(),
#       total_records = n()
#     ) |>
#     ungroup()
#
#   # Identify states with more than the threshold of missing admtype and calc_sent_lgth_compl
#   states_high_missing_admtype <- missing_summary |>
#     filter(admtype_missing_rate > missing_threshold) |>
#     pull(state)
#
#   states_high_missing_calc_sent <- missing_summary |>
#     filter(calc_sent_lgth_missing_rate > missing_threshold) |>
#     pull(state)
#
#   # States that are included in the final filtered data
#   included_states <- unique(filtered_data$state)
#
#   # Print diagnostic information
#   cat("States included in the final filtered data:\n", included_states, "\n\n")
#   cat("States with more than", missing_threshold * 100, "% missing 'admtype':\n", states_high_missing_admtype, "\n\n")
#   cat("States with more than", missing_threshold * 100, "% missing 'calc_sent_lgth_compl':\n", states_high_missing_calc_sent, "\n\n")
#
#   # Return the filtered data
#   return(filtered_data)
# }
fnc_filter_pe_population_criteria <- function(data,
                                              admtype_filter = "New court commitment",
                                              missing_threshold = 0.5) {

  # Function to calculate and print diagnostic information for a given subset of data
  print_diagnostics <- function(data_subset, subset_label) {
    # Get states that have not abolished parole (inside this function to avoid scoping issues)
    abolished <- state_notes |>
      filter(abolished_parole == "N") |>
      pull(state)

    # Filter data based on the admission type, valid sentence lengths (calc_sent_lgth_compl > 0), and states that did not abolish parole
    filtered_data <- data_subset |>
      filter(admtype == admtype_filter) |>
      # filter(!is.na(calc_sent_lgth_compl) & calc_sent_lgth_compl > 0) |>
      filter(sentlgth %in% c("1-1.9 years",
                             "2-4.9 years",
                             "5-9.9 years",
                             "10-24.9 years",
                             ">=25 years")) |>
      filter(state %in% abolished)

    # Calculate missing rates for each state in admtype and calc_sent_lgth_compl
    missing_summary <- data_subset |>
      group_by(state) |>
      summarize(
        admtype_missing_rate = sum(admtype == "Unknown") / n(),
        # calc_sent_lgth_missing_rate = sum(is.na(calc_sent_lgth_compl)) / n(),
        total_records = n()
      ) |>
      ungroup()

    # Identify states with more than the threshold of missing admtype and calc_sent_lgth_compl
    states_high_missing_admtype <- missing_summary |>
      filter(admtype_missing_rate > missing_threshold) |>
      pull(state)

    # states_high_missing_calc_sent <- missing_summary |>
    #   filter(calc_sent_lgth_missing_rate > missing_threshold) |>
    #   pull(state)

    # Identify states with 100% missing admtype and calc_sent_lgth_compl
    states_100_missing_admtype <- missing_summary |>
      filter(admtype_missing_rate == 1) |>
      pull(state)

    # states_100_missing_calc_sent <- missing_summary |>
    #   filter(calc_sent_lgth_missing_rate == 1) |>
    #   pull(state)

    # States that are included in the final filtered data
    included_states <- unique(filtered_data$state)

    # Print diagnostic information
    cat("\nDiagnostic Information for", subset_label, ":\n")
    cat("States included in the final filtered data:\n", included_states, "\n\n")
    cat("States with more than", missing_threshold * 100, "% missing 'admtype':\n", states_high_missing_admtype, "\n\n")
    # cat("States with more than", missing_threshold * 100, "% missing 'calc_sent_lgth_compl':\n", states_high_missing_calc_sent, "\n\n")
    cat("States with 100% missing 'admtype':\n", states_100_missing_admtype, "\n\n")
    # cat("States with 100% missing 'calc_sent_lgth_compl':\n", states_100_missing_calc_sent, "\n\n")
  }

  # Print diagnostics for all data
  print_diagnostics(data, "All Data")

  # Print diagnostics for rptyear == 2019
  data_2019 <- data |> filter(rptyear == 2019)
  print_diagnostics(data_2019, "rptyear == 2019")

  # Print diagnostics for rptyear == 2020
  data_2020 <- data |> filter(rptyear == 2020)
  print_diagnostics(data_2020, "rptyear == 2020")

  # Return the filtered data for the entire dataset as before
  abolished <- state_notes |> filter(abolished_parole == "N") |> pull(state)

  return(data |>
           filter(admtype == admtype_filter) |>
           filter(sentlgth %in% c("1-1.9 years",
                                  "2-4.9 years",
                                  "5-9.9 years",
                                  "10-24.9 years",
                                  ">=25 years")) |>
           filter(state %in% abolished))
}



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
  df1 <- fnc_filter_pe_population_criteria(df) |>
    # Filter for the selected year and 'Current' parole eligibility status
    filter(rptyear == select_year & parelig_status == "Current") |>
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


fnc_xaxis_labels <- list(
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
)



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
             labels = fnc_xaxis_labels
             ) |>
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


#' @title Highcharter Lollipop Overview Chart
#' @description This function creates a lollipop chart using Highcharter, where lines
#' represent the data range and circles represent specific points like average length of stay (LOS).
#' @param df1 A data frame containing the main data points to be plotted as scatter points (e.g., average LOS).
#' @param df_lines A data frame containing line data, typically representing ranges (e.g., min and max LOS).
#' @param y_var The variable on the y-axis, typically a categorical variable.
#' @param group_var The variable to group the data by, used for color coding and labeling points.
#' @param y_labels A vector of labels for the y-axis.
#' @param max_los A numeric value representing the maximum length of stay (LOS) to set the x-axis limit.
#' @param base_hc_theme A theme object to apply to the Highcharter plot.
#' @return A Highcharter object representing the lollipop chart.
#' @export
# fnc_hc_lollipop_overview <- function(df1, df_lines, x_var, y_var, group_var, y_labels, max_los, base_hc_theme) {
#
#   # Initialize the highchart object
#   highchart() |>
#
#     # Add a line series to represent ranges (e.g., min-max LOS)
#     hc_add_series(
#       df_lines,
#       type = 'line',
#       hcaes(x = value, y = !!sym(y_var), group = !!sym(group_var)),  # Map the x and y aesthetics
#       lineWidth = 1,  # Set line width
#       color = "black",  # Line color
#       dashStyle = "solid",  # Line style
#       opacity = 1,  # Line opacity
#       marker = list(enabled = FALSE),  # Disable markers on the line
#       enableMouseTracking = FALSE,  # Disable mouse tracking for the line
#       showInLegend = FALSE  # Hide the line from the legend
#     ) |>
#
#     # Add scatter points to represent specific values (e.g., average LOS)
#     hc_add_series(
#       df1,
#       type = 'scatter',
#       marker = list(radius = 5),  # Customize scatter point markers
#       hcaes(x = !!sym(x_var), y = !!sym(y_var), group = !!sym(group_var), name = !!sym(group_var), color = color),  # Map scatter aesthetics
#       dataLabels = list(
#         enabled = TRUE,  # Enable data labels for scatter points
#         format = '{point.x:.1f} Years',  # Format the data label as years
#         align = "left",  # Align the data label to the left
#         y = 9,  # Adjust vertical position of the label
#         x = 8,  # Adjust horizontal position of the label
#         style = list(color = 'black', fontWeight = "regular", fontSize = "12px")  # Style for the data labels
#       )
#     ) |>
#
#     # Apply a base theme to the chart
#     hc_add_theme(base_hc_theme) |>
#
#     # Customize the y-axis
#     hc_yAxis(
#       labels = list(
#         style = list(
#           color = 'black',  # Y-axis label color
#           fontWeight = "regular",  # Font weight for the labels
#           fontSize = "12px"  # Font size for the labels
#         )
#       ),
#       title = list(text = ""),  # No title for the y-axis
#       majorGridLineColor = "transparent",  # Hide major grid lines
#       gridLineColor = "transparent",  # Hide grid lines
#       lineColor = "transparent",  # Hide axis line
#       minorGridLineColor = "transparent",  # Hide minor grid lines
#       tickColor = "black",  # Color of the axis ticks
#       categories = y_labels  # Set the y-axis categories based on the provided labels
#     ) |>
#
#     # Customize the x-axis
#     hc_xAxis(
#       title = list(text = ""),  # No title for the x-axis
#       labels = list(enabled = FALSE),  # Disable x-axis labels
#       lineColor = "transparent",  # Hide the x-axis line
#       minorGridLineColor = "transparent",  # Hide minor grid lines
#       tickLength = 0,  # No tick marks
#       gridLineColor = "transparent",  # Hide grid lines
#       tickColor = "transparent",  # Hide tick color
#       max = max_los * 1.5  # Set the maximum x-axis value based on max_los
#     ) |>
#
#     # Disable exporting features
#     hc_exporting(enabled = FALSE) |>
#
#     # Disable tooltips
#     hc_tooltip(enabled = FALSE) |>
#
#     # Disable the legend
#     hc_legend(enabled = FALSE)
# }

fnc_disparities_theme <- function(chart){
  chart <- chart |>
  hc_add_theme(base_hc_theme) |>
    hc_yAxis(
      labels = list(
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
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      minorGridLineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los * 1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE)
  return(chart)
}





#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

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

#' @title Icon Options for Plotting
#' @description Generates a list of plot options based on a partial fill value, colors, and a background for creating icon-based infographics.
#' @param partialval A numeric value between 0 and 1 representing the percentage of the partial icon fill.
#' @param empty Color for the empty part of the icon.
#' @param fill Color for the full part of the icon.
#' @param partial Color for the partially filled icon.
#' @param bg Background color for the icons.
#' @param fillHoriz A logical flag indicating whether to fill icons horizontally (TRUE) or vertically (FALSE).
#' @return A list of ggplot objects representing empty, full, and partial icon states.
#' @export
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

#' @title Create Icon Infographic
#' @description Generates an infographic representing a Relative Rate Index (RRI) using icons to indicate full, partial, and empty states.
#' @param rri_raw The raw Relative Rate Index value.
#' @param rri_digits Number of decimal places to round the RRI value.
#' @param fillcolor Color to fill the full icons.
#' @param partialcolor Color to fill partially filled icons.
#' @param emptyhumans Logical flag to indicate whether empty icons should be included.
#' @param emptycolor Color for the empty icons.
#' @param infogs Total number of icons to display.
#' @param infogs_ncol Number of columns for the grid layout of icons.
#' @param fillHoriz Logical flag to determine if icons should be filled horizontally or vertically.
#' @return A ggplot2 object representing the generated icon infographic.
#' @export
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

#' @title Create Infographic with Icons and RRI Label
#' @description Combines a label of the RRI value with a grid of icons representing the RRI.
#' @param rri_raw The raw Relative Rate Index value.
#' @param infographic_color The color for the infographic elements.
#' @return A ggplot object combining the RRI label and the icon grid.
#' @export
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







