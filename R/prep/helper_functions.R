
#-------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#-------------------------------------------------------------------------------

#' Helper to streamline the mutate transformations for common columns.
#'
#' @param df A data frame.
#' @param cols A character vector of column names to trim.
#' @param prefix_length An integer for number of characters to remove from the start.
#' @return A modified data frame with trimmed columns.
apply_column_trims <- function(df, cols, prefix_length = 5) {
  df |>
    mutate(across(all_of(cols), ~ str_sub(., prefix_length, -1))) |>
    mutate(across(everything(), trimws))
}

#' Factor transformation for categorical variables
#'
#' @param df A data frame containing categorical columns to be factored.
#' @param col_name Column to be factored.
#' @param levels Levels to apply to the factor.
#' @return A data frame with factored columns.
apply_factor_levels <- function(df, col_name, levels) {
  df |>
    mutate({{col_name}} := factor({{col_name}}, levels = levels))
}

#' Load NCRP Data with file validation
fnc_load_ncrp_data <- function(file_id) {
  file_path <- paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_38492-V1/ICPSR_38492/DS000", file_id, "/38492-000", file_id, "-Data.rda")
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }

  data_env <- new.env()
  load(file_path, envir = data_env)
  data_object <- ls(envir = data_env)[1]
  get(data_object, envir = data_env)
}

#' Streamline Admission Type Recategorization
fnc_create_admtype <- function(df) {
  df |>
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other or Unknown",
      is.na(admtype) ~ "Other or Unknown",
      TRUE ~ admtype
    )) |>
    apply_factor_levels(admtype, c("New court commitment", "Parole return/revocation", "Other or Unknown"))
}

#' Streamline Parole Eligibility Status
fnc_create_parelig_status <- function(df) {
  df |>
    mutate(
      time_between_ped_rptyear = parelig_year - rptyear,
      parelig_status = case_when(
        parelig_year <= rptyear ~ "Current",
        parelig_year > rptyear ~ "Future",
        is.na(parelig_year) ~ "Missing or Not Parole-Eligible"
      )
    ) |>
    apply_factor_levels(parelig_status, c("Current", "Future", "Missing or Not Parole-Eligible"))
}

#' Streamline FBI Index Categorization
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
    apply_factor_levels(fbi_index, c("Murder and Non-negligent Manslaughter", "Negligent Manslaughter",
                                     "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
                                     "Other Violent Offenses", "Property", "Public order", "Drugs", "Other", "Unknown"))
}


#' Clean BJS Data
#'
#' This function cleans BJS data.
#'
#' @param df A data frame containing BJS data.
#' @return A cleaned data frame.
#' @export
fnc_clean_bjs_data <- function(df) {
  df <- df |>
    mutate(state = str_replace(state, "/.*", "")) |>
    mutate(state = str_replace(state, "Alaskab", "Alaska")) |>
    mutate(state = str_replace(state, "Utahc", "Utah")) |>
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") |>
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) |>
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))
  return(df)
}








#-------------------------------------------------------------------------------
# DATA PREPARATION
#-------------------------------------------------------------------------------

#' Filter prison population data by admtype and sentence length
#'
#' This function filters the prison population data to include only those with
#' a specific admission type (e.g., "New court commitment") and sentence lengths
#' between 1-25 years.
#'
#' @param data A dataframe containing the prison population data.
#' @param admtype_filter A string indicating the admission type to filter by (default is "New court commitment").
#' @param sentence_lengths A vector of strings indicating the sentence lengths to include (default includes "1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years").
#' @return A filtered dataframe with prison population data based on the given admission type and sentence lengths.
#' @export
#' @examples
#' ncrp_filtered <- filter_population_criteria(ncrp_yearendpop)
filter_population_criteria <- function(data,
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

#' # Prepare data
#'
#' This function prepares the data for a simple bar graph, filtering for "Current" parole eligibility status and specific sentence lengths.
#'
#' @param df A data frame containing the data.
#' @param count_column The column to count occurrences.
#' @return A prepared data frame with necessary calculations.
#' @export
fnc_prepare_pe_data <- function(df, count_column) {
  count_column_title <- deparse(substitute(count_column))
  count_column_title <- case_when(count_column_title == "race"      ~ "Race and Ethnicity",
                                  count_column_title == "sex"       ~ "Sex",
                                  count_column_title == "ageyrend"  ~ "Age",
                                  count_column_title == "sentlgth"  ~ "Sentence Length",
                                  count_column_title == "fbi_index" ~ "Offense Type")

  df1 <- df |>
    filter(rptyear == select_year) |>
    filter({{ count_column }} != "Unknown") |>
    group_by(state, {{ count_column }}, parelig_status) |>
    summarize(n = n(), .groups = "drop") |>
    group_by(state, {{ count_column }}) |>
    mutate(
      prop = n / sum(n),  # Make sure you're calculating proportions within each group
      yearendpop_ped = sum(n),
      prop_label = paste0(round(prop * 100, 0), "%"),
      n_label = formattable::comma(n, 0)
    ) |>
    ungroup() |>
    mutate(tooltip = paste0("<b>", count_column_title, ":</b> ", {{ count_column }}, "<br>",
                            "<b>Parole Eligibility Status:</b> ", parelig_status, "<br>",
                            "<b>People:</b> ", n_label, "<br>",
                            "<b>Percentage of People:</b> ", prop_label))

  return(df1)
}


#-------------------------------------------------------------------------------
# VISUALIZATIONS
#-------------------------------------------------------------------------------

#' Common Style Elements
#'
#' This list defines the common style elements used across different themes.
#' @return A list of common style elements.
#' @export
common_style <- list(
  fontFamily = "Graphik",
  color = "black",
  fontSize = "12px",
  fontWeight = "regular"
)

#' Common Chart Style
#'
#' This list defines the common chart style elements used across different themes.
#' @return A list of common chart style elements.
#' @export
common_chart_style <- list(
  fontFamily = "Graphik",
  fontSize = "12px",
  color = "black"
)

#' Common Title Style
#'
#' This list defines the common title style elements used across different themes.
#' @return A list of common title style elements.
#' @export
common_title_style <- list(
  fontFamily = "Graphik",
  fontWeight = "bold",
  color = "black"
)

#' Base Highcharts Theme
#'
#' This theme serves as the base for other themes.
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
        symbol = 'circle'  # This line ensures that the dots are circles
      )
    ),
    spline = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'  # Ensures that the dots are circles
      )
    ),
    area = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'  # Ensures that the dots are circles
      )
    ),
    areaspline = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'  # Ensures that the dots are circles
      )
    ),
    arearange = list(
      marker = list(
        enabled = TRUE,
        symbol = 'circle'  # Ensures that the dots are circles
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


# Function to generate Highcharts stacked bar chart
fnc_hc_stackedbar_pe_population <- function(df, count_column, title, subtitle, categories_col, colors) {

  # Filter and sort data for the chart
  data <- df |>
    filter(state == unique(df$state)) |>
    arrange(desc({{ count_column }}))

  # Create highchart object
  highcharts <- highchart() |>
    hc_chart(type = "column") |>
    hc_title(text = title) |>
    hc_subtitle(text = subtitle) |>
    hc_xAxis(categories = unique(data[[categories_col]])) |>
    hc_yAxis(
      title = list(text = ""),
      min = 0,
      max = 1,  # Ensure proportions are displayed between 0 and 1 (100%)
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

fnc_prepare_pe_data2 <- function(df, count_column){
  df1 <- df |>
    filter(rptyear == select_year &
             parelig_status == "Current") |>
    filter(admtype == "New court commitment") |>
    filter(sentlgth == "1-1.9 years" |
             sentlgth == "2-4.9 years" |
             sentlgth == "5-9.9 years" |
             sentlgth == "10-24.9 years") |>
    group_by(state) |>
    filter(!is.na({{ count_column }})) |>
    count({{ count_column }}) |>
    mutate(
      prop = n/sum(n),
      yearendpop_ped = sum(n),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0)
    ) |>
    ungroup()
  return(df1)
}

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
    # hc_xAxis(categories = xaxis_order) |>
    # hc_xAxis(categories = xaxis_order,
    #          labels = list(style = list(fontSize = '12px', fontFamily = 'Graphik'),
    #                        useHTML = TRUE, rotation = -45, align = 'right',
    #                        formatter = JS("function () {
    #                            return this.value.split(' ').reduce(function (acc, word, index) {
    #                              return acc + (index && !(index % 3) ? '<br/>' : ' ') + word;
    #                            }, '');
    #                          }"))
    # ) |>
    # hc_xAxis(categories = xaxis_order,
    #          labels = list(style = list(fontSize = '12px', fontFamily = 'Graphik'),
    #                        useHTML = TRUE, align = 'center',
    #                        formatter = JS("function () {
    #                            return this.value.split(/(?<=\\S{12})\\s+/).join('<br/>');
    #                          }"))
    # ) |>
    hc_xAxis(categories = xaxis_order,
             labels = list(
               formatter = JS(
                 "function() {
                    var label = this.value;
                    var maxLength = 25;
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
    hc_yAxis(labels = list(enabled = TRUE),
             title = list(text = "")
    ) |>
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



# Retrieve and process census data for a given state
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
        variable %in% c("estimate_american_indian",
                        "estimate_asian",
                        "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    )
  return(df)
}
