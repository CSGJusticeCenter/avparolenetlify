
#######################################
# Project: AV Parole
# File: functions.R
# Authors: Mari Roberts
# Date last updated: October 5, 2023 (MAR)
# Description:
#    Custom functions
#######################################

# Filter by
fnc_parameters <- function(df){
  df <- df %>%
    filter(admtype == "New court commitment") %>%
    filter(sentlgth == "1-1.9 years" |
             sentlgth == "2-4.9 years" |
             sentlgth == "5-9.9 years" |
             sentlgth == "10-24.9 years")

}

# Create parole eligibility status
# if year of parole eligibility is less than year reported to NCRP, then "currently eligible for parole"
# if year of parole eligibility is more than or equal to year reported to NCRP, then "eligible for parole in the future"
# if year of parole eligibility NA, then "missing data on parole eligibility"
fnc_create_parelig_status <- function(df){

  df %>%
    mutate(time_between_ped_rptyear = parelig_year - rptyear) %>%
    mutate(
      parelig_status = case_when(
        parelig_year <=  rptyear ~ "Current",
        parelig_year > rptyear & time_between_ped_rptyear <= 5  ~ "Future 1-5 Years",
        parelig_year > rptyear & time_between_ped_rptyear > 5  ~ "Future 6+ Years",
        is.na(parelig_year) ~ "Missing"),
      parelig_status = factor(parelig_status,
                              levels = c("Current",
                                         "Future 1-5 Years",
                                         "Future 6+ Years",
                                         "Missing")))

}


# Re-categorize offense type
fnc_create_fbi_index <- function(df){
  df <- df %>%
    mutate(fbi_index = case_when(
      offdetail == "Aggravated or simple assault"                  ~ "Aggravated or Simple Assault",
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder and Non-negligent Manslaughter",
      offdetail == "Negligent manslaughter"                        ~ "Other Violent Offenses",
      offdetail == "Other violent offenses"                        ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault"                           ~ "Rape or Sexual Assault",
      offdetail == "Robbery"                                       ~ "Robbery",
      offdetail == "Other/unspecified"                             ~ "Other or Unknown",
      is.na(offdetail)                                             ~ "Other or Unknown",
      TRUE ~ offgeneral
    )) %>%
    mutate(fbi_index = factor(fbi_index,
                              levels = c("Murder and Non-negligent Manslaughter",
                                         "Rape or Sexual Assault",
                                         "Robbery",
                                         "Aggravated or Simple Assault",
                                         "Other Violent Offenses",
                                         "Property",
                                         "Public order",
                                         "Drugs",
                                         "Other or Unknown")))
}

# Re-categorize admission type
fnc_create_admtype <- function(df){
  df <- df %>%
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other or Unknown",
      is.na(admtype) ~ "Other or Unknown",
      TRUE ~ admtype)) %>%
    mutate(admtype = factor(admtype,
                            levels = c("New court commitment",
                                       "Parole return/revocation",
                                       "Other or Unknown")))
}

# Calculate n, prop, and create labels and tooltips
fnc_values_tooltip <- function(df, count_column) {
  df %>%
    count({{ count_column }}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       "<b>", {{ count_column }}, "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}

# Calculate n, prop, and create labels and tooltips when there are two columns of interest
fnc_values_tooltip2 <- function(df, count_column1, count_column2) {
  df %>%
    count({{count_column1}}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       "<b>", {{ count_column2 }}, "</b><br><br>",
                       "<b>", {{ count_column1 }}, "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}

# Prepare Annual Parole Survey data for analysis (after 2008)
fnc_aps_prepare <- function(df){

  df <- df %>%
    mutate(rptyear = as.numeric(rptyear)) %>%
    select(state,
           rptyear,
           endisrel,
           enmanrel,
           enreltsr,
           incarcerated_from_parole = exincrev) %>%
    mutate(released_to_parole =
             rowSums(.[c("endisrel", "enmanrel", "enreltsr")],
                     na.rm = TRUE),
           released_to_parole =
             ifelse(released_to_parole == 0, NA, released_to_parole))

  return(df)
}

# Prepare Annual Parole Survey data for analysis (before 2008)
# There is no enreltsr variable so make NA
fnc_aps_prepare_pre2008 <- function(df){

  df <- df %>%
    mutate(enreltsr = NA,
           rptyear = as.numeric(rptyear)) %>%
    select(state,
           rptyear,
           endisrel,
           enmanrel,
           enreltsr,
           incarcerated_from_parole = exincrev) %>%
    mutate(released_to_parole =
             rowSums(.[c("endisrel", "enmanrel")],
                     na.rm = TRUE),
           released_to_parole =
             ifelse(released_to_parole == 0, NA, released_to_parole))

  return(df)
}

# Prepare APS data depending on the year
fnc_prepare_aps_data <- function(data, year, pre_2008 = FALSE) {
  data <- data %>%
    clean_names() %>%
    mutate(rptyear = year)

  if (pre_2008) {
    data <- data %>%
      select(-stateid) %>%
      rename(stateid = state) %>%
      mutate(stateid = str_trim(stateid)) %>%
      left_join(state_names_abb, by = "stateid") %>%
      fnc_aps_prepare_pre2008()
  } else {
    data <- data %>%
      mutate(state = str_sub(stateid, 6, -1)) %>%
      fnc_aps_prepare()
  }
  return(data)
}

# Prepare BJS data
fnc_clean_bjs_data <- function(df){
  df <- df %>%
    mutate(state = str_replace(state, "/.*", "")) %>%
    mutate(state = str_replace(state, "Alaskab", "Alaska")) %>%
    mutate(state = str_replace(state, "Utahc", "Utah")) %>%
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") %>%
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) %>%
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))
}


# Prepare data for a simple bar graph
fnc_prepare_pe_data <- function(df, count_column){
  df1 <- df %>%
    filter(rptyear == select_year &
             parelig_status == "Current") %>%
    fnc_parameters() %>%
    group_by(state) %>%
    count({{ count_column }}) %>%
    mutate(
      prop = n/sum(n),
      yearendpop_ped = sum(n),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0)
    ) %>%
    ungroup() %>%
    mutate(tooltip = paste0("<b>", state, " - ",
                            {{ count_column }}, "</b><br>",
                            prop_label, "<br>"))
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








###################
# Reactable
###################

# Reactable table themes
reactable_theme <-
  reactableTheme(borderColor = neutralBkgndLight,
                 stripedColor = neutralBkgndLight,
                 cellStyle = list(display = "flex",
                                  flexDirection = "column",
                                  justifyContent = "center"))

reactable_style <- list(
  fontFamily = "Graphik, sans-serif",
  fontSize = "0.9rem",
  color = neutralBlackText
)








###################
# Highcharter
###################

# Overall highcharts theme for plots
hc_theme <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik",
                            fontSize = "12px",
                            color = neutralBlackText)),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = neutralBlackText,
                     fontSize = "12px",
                     fontWeight = "regular")
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE)),
    spline = list(marker = list(enabled = FALSE)),
    area = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange = list(marker = list(enabled = FALSE)),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = neutralBlackText)
      )
    )
  )
)

hc_theme_with_line <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik",
                            fontSize = "12px",
                            color = neutralBlackText)),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = neutralBlackText,
                     fontSize = "12px",
                     fontWeight = "regular")
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular"))
  ),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE)),
    spline = list(marker = list(enabled = FALSE)),
    area = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange = list(marker = list(enabled = FALSE)),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = neutralBlackText)
      )
    )
  )
)

# Create grouped, stacked bar chart
fnc_grouped_stacked_barchart <- function(df, x_column, group_by_col, accessibility_text) {

  highcharts <-
    hchart(df, "bar",
           hcaes(x = !!sym(x_column),
                 y = prop,
                 group = !!sym(group_by_col)
           ),
           dataLabels = list(enabled = TRUE,
                             format = "{point.prop_label}",
                             style = list(fontWeight = "regular",
                                          fontSize = "12px",
                                          fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
    ) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = accessibility_text,
        landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)

}

# Create basic horizontal bar chart that isn't grouped
fnc_basic_barchart <- function(df, filter_column, accessibility_text) {

  xaxis_order <- df[[filter_column]]

  highcharts <- highchart() %>%
    hc_add_series(df,
                  type = "bar",
                  hcaes(x = !!sym(filter_column),
                        y = prop),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
    ) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# Create pie chart
fnc_piechart <- function(df, x_column, accessibility_text){

  highcharts <- hchart(df,
                       "pie",
                       hcaes(x = !!sym(x_column), y = prop),
                       dataLabels = list(
                         style = list(fontSize = "1em",
                                      fontWeight = "regular",
                                      alignTo = "connectors",
                                      color = neutralBlackText),
                         enabled = TRUE,
                         formatter = JS(paste("function() { return this.point.name + ': <b>' + this.point.prop_label + '</b>';}"))
                       )
  ) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(30, 0, 10, 0)) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

}

























































































# Create single horizontal bar chart that is grouped
fnc_hc_single_grouped_columnchart <- function(df, value, group_by_column, x_axis, accessibility_text) {

  highchart <- hchart(df, "bar",
                      hcaes(x = !!sym(x_axis),
                            y = !!sym(value),
                            group = !!sym(group_by_column)),
                      dataLabels = list(enabled = TRUE,
                                        format = "{point.prop_label}",
                                        style = list(fontWeight = "bold",
                                                     fontSize = "12px",
                                                     fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(format = "{value}%",
                           enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_plotOptions(
      series = list(stacking = "normal",
                    animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3,
                    minPointLength = 4),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))
  return(highchart)
}

fnc_stackedbar_admtype_chart <- function(df, group_by_col, accessibility_text) {
  highchart <- hchart(df, "bar",
                      hcaes(x = admtype,
                            y = prop,
                            group = !!sym(group_by_col)
                      ),
                      dataLabels = list(enabled = TRUE,
                                        format = "{point.prop_label}",
                                        style = list(fontWeight = "bold",
                                                     fontSize = "12px",
                                                     fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
    ) %>%
    hc_xAxis(categories = c("New court commitment",
                            "Parole return/revocation",
                            "Other or Unknown"),
             title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal", animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))
  return(highchart)
}





























































# Create basic horizontal bar chart that isn't grouped
fnc_hc_basic_columnchart <- function(df, value, x_axis, x_axis_text, y_axis_text, theme, accessibility_text) {

  xaxis_order <- levels(df[[x_axis]]) # must be factor to work

  highchart <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_axis),
                        y = !!sym(value)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.data_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "18px",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order,
             title = list(text = x_axis_text)) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = y_axis_text)) %>%
    # make bars wider
    hc_plotOptions(column = list(
      pointPadding = 0.05,
      groupPadding = 0.1)) %>%
    hc_add_theme(theme) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))
  return(highchart)
}


# Create basic horizontal bar chart that is grouped
fnc_hc_grouped_columnchart <- function(df, value, group_by_column, x_axis, x_axis_text, y_axis_text, theme, accessibility_text) {

  xaxis_order <- levels(df[[x_axis]]) # must be factor to work

  highchart <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_axis),
                        y = !!sym(value),
                        group = !!sym(group_by_column)), # fixed here
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.data_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "18px",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order,
             title = list(text = x_axis_text)) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = y_axis_text)) %>%
    # make bars wider
    hc_plotOptions(column = list(
      pointPadding = 0.05,
      groupPadding = 0.1)) %>%
    hc_add_theme(theme) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))

  return(highchart)
}

fnc_hc_stacked_columnchart <- function(df, value, group_by_column, x_axis, x_axis_text, y_axis_text, theme, accessibility_text) {

  xaxis_order <- levels(df[[x_axis]]) # must be factor to work

  highchart <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_axis),
                        y = !!sym(value),
                        group = !!sym(group_by_column)), # fixed here
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.data_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "18px",
                                                 color = "white",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order,
             title = list(text = x_axis_text)) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = y_axis_text)) %>%
    # make bars wider
    hc_plotOptions(column = list(
      pointPadding = 0.05,
      groupPadding = 0.1)) %>%
    hc_add_theme(theme) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3,
                    minPointLength = 4,
                    stacking = "normal"),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

  return(highchart)
}










# Create a sentence based on pct changes by year
fnc_generate_sentence <- function(data, value, metric_name, year_start1, year_end1, year_start2, year_end2) {

  # extract the relevant populations
  pop_start1 <- data[[value]][data$year == as.character(year_start1)]
  pop_end1 <- data[[value]][data$year == as.character(year_end1)]
  pop_start2 <- data[[value]][data$year == as.character(year_start2)]
  pop_end2 <- data[[value]][data$year == as.character(year_end2)]

  # calculate percent changes
  pct_change_1 <- ((pop_end1 - pop_start1) / pop_start1) * 100
  pct_change_2 <- ((pop_end2 - pop_start2) / pop_start2) * 100

  # determine the direction of the change for the sentences
  direction_1 <- ifelse(pct_change_1 >= 0, "increase", "decrease")
  direction_2 <- ifelse(pct_change_2 >= 0, "increase", "decrease")

  # create the sentences
  sentence_1 <- paste0("From FY ", year_start1, " to FY ", year_end1, ", there was a ",
                       round(pct_change_1, 2), "% ", direction_1,
                       " in ", metric_name, ".")
  sentence_2 <- paste0("From FY ", year_start2, " to FY ", year_end2, ", there was a ",
                       round(pct_change_2, 2), "% ", direction_2,
                       " in ", metric_name, ".")
  sentences <- paste0(sentence_1, " ", sentence_2)

  return(sentences)
}



#######################################
# Project: AV Parole
# File: functions.R
# Authors: Mari Roberts
# Date last updated: October 5, 2023 (MAR)
# Description:
#    Custom functions
#######################################

# Filter by
fnc_parameters <- function(df){
  df <- df %>%
    filter(admtype == "New court commitment") %>%
    filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years")

}

# Create parole eligibility status
# if year of parole eligibility is less than year reported to NCRP, then "currently eligible for parole"
# if year of parole eligibility is more than or equal to year reported to NCRP, then "eligible for parole in the future"
# if year of parole eligibility NA, then "missing data on parole eligibility"
fnc_create_parelig_status <- function(df){

  df %>%
    mutate(time_between_ped_rptyear = parelig_year - rptyear) %>%
    mutate(
      parelig_status = case_when(
        parelig_year <=  rptyear ~ "Current",
        parelig_year > rptyear & time_between_ped_rptyear <= 5  ~ "Future 1-5 Years",
        parelig_year > rptyear & time_between_ped_rptyear > 5  ~ "Future 6+ Years",
        is.na(parelig_year) ~ "Missing"),
      parelig_status = factor(parelig_status,
                              levels = c("Current",
                                         "Future 1-5 Years",
                                         "Future 6+ Years",
                                         "Missing")))

}


# Re-categorize offense type
fnc_create_fbi_index <- function(df){
  df <- df %>%
    mutate(fbi_index = case_when(
      offdetail == "Aggravated or simple assault"                  ~ "Aggravated or Simple Assault",
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder and Non-negligent Manslaughter",
      offdetail == "Negligent manslaughter"                        ~ "Other Violent Offenses",
      offdetail == "Other violent offenses"                        ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault"                           ~ "Rape or Sexual Assault",
      offdetail == "Robbery"                                       ~ "Robbery",
      offdetail == "Other/unspecified"                             ~ "Other or Unknown",
      is.na(offdetail)                                             ~ "Other or Unknown",
      TRUE ~ offgeneral
    )) %>%
    mutate(fbi_index = factor(fbi_index,
                              levels = c("Murder and Non-negligent Manslaughter",
                                         "Rape or Sexual Assault",
                                         "Robbery",
                                         "Aggravated or Simple Assault",
                                         "Other Violent Offenses",
                                         "Property",
                                         "Public order",
                                         "Drugs",
                                         "Other or Unknown")))
}

# Re-categorize admission type
fnc_create_admtype <- function(df){
  df <- df %>%
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other or Unknown",
      is.na(admtype) ~ "Other or Unknown",
      TRUE ~ admtype)) %>%
    mutate(admtype = factor(admtype,
                            levels = c("New court commitment",
                                       "Parole return/revocation",
                                       "Other or Unknown")))
}

# Calculate n, prop, and create labels and tooltips
fnc_values_tooltip <- function(df, count_column) {
  df %>%
    count({{ count_column }}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       "<b>", {{ count_column }}, "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}

# Calculate n, prop, and create labels and tooltips when there are two columns of interest
fnc_values_tooltip2 <- function(df, count_column1, count_column2) {
  df %>%
    count({{count_column1}}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       "<b>", {{ count_column2 }}, "</b><br><br>",
                       "<b>", {{ count_column1 }}, "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}

# Prepare Annual Parole Survey data for analysis (after 2008)
fnc_aps_prepare <- function(df){

  df <- df %>%
    mutate(rptyear = as.numeric(rptyear)) %>%
    select(state,
           rptyear,
           endisrel,
           enmanrel,
           enreltsr,
           incarcerated_from_parole = exincrev) %>%
    mutate(released_to_parole =
             rowSums(.[c("endisrel", "enmanrel", "enreltsr")],
                     na.rm = TRUE),
           released_to_parole =
             ifelse(released_to_parole == 0, NA, released_to_parole))

  return(df)
}

# Prepare Annual Parole Survey data for analysis (before 2008)
# There is no enreltsr variable so make NA
fnc_aps_prepare_pre2008 <- function(df){

  df <- df %>%
    mutate(enreltsr = NA,
           rptyear = as.numeric(rptyear)) %>%
    select(state,
           rptyear,
           endisrel,
           enmanrel,
           enreltsr,
           incarcerated_from_parole = exincrev) %>%
    mutate(released_to_parole =
             rowSums(.[c("endisrel", "enmanrel")],
                     na.rm = TRUE),
           released_to_parole =
             ifelse(released_to_parole == 0, NA, released_to_parole))

  return(df)
}

# Prepare APS data depending on the year
fnc_prepare_aps_data <- function(data, year, pre_2008 = FALSE) {
  data <- data %>%
    clean_names() %>%
    mutate(rptyear = year)

  if (pre_2008) {
    data <- data %>%
      select(-stateid) %>%
      rename(stateid = state) %>%
      mutate(stateid = str_trim(stateid)) %>%
      left_join(state_names_abb, by = "stateid") %>%
      fnc_aps_prepare_pre2008()
  } else {
    data <- data %>%
      mutate(state = str_sub(stateid, 6, -1)) %>%
      fnc_aps_prepare()
  }
  return(data)
}

# Prepare BJS data
fnc_clean_bjs_data <- function(df){
  df <- df %>%
    mutate(state = str_replace(state, "/.*", "")) %>%
    mutate(state = str_replace(state, "Alaskab", "Alaska")) %>%
    mutate(state = str_replace(state, "Utahc", "Utah")) %>%
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") %>%
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) %>%
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))
}


# Prepare data for a simple bar graph
fnc_prepare_pe_data <- function(df, count_column){
  df1 <- df %>%
    filter(rptyear == select_year &
           parelig_status == "Current") %>%
    fnc_parameters() %>%
    group_by(state) %>%
    count({{ count_column }}) %>%
    mutate(
      prop = n/sum(n),
      yearendpop_ped = sum(n),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0)
    ) %>%
    ungroup() %>%
    mutate(tooltip = paste0("<b>", state, " - ",
                            {{ count_column }}, "</b><br>",
                            prop_label, "<br>"))
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








###################
# Reactable
###################

# Highcharts theme for reactable tables
hc_reactable_theme <-
  reactableTheme(borderColor = neutralBkgndLight,
                 stripedColor = neutralBkgndLight,
                 cellStyle = list(display = "flex",
                                  flexDirection = "column",
                                  justifyContent = "center"))

hc_reactable_style <- list(
  fontFamily = "Graphik, sans-serif",
  fontSize = "0.9rem",
  color = neutralBlackText
)





###################
# Plots
###################

# Overall highcharts theme for plots
hc_theme_jc <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik",
                            fontSize = "12px",
                            color = neutralBlackText)),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = neutralBlackText,
                     fontWeight = "regular")
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  plotOptions = list(
    line = list(marker = list(enabled = FALSE)),
    spline = list(marker = list(enabled = FALSE)),
    area = list(marker = list(enabled = FALSE)),
    areaspline = list(marker = list(enabled = FALSE)),
    arearange = list(marker = list(enabled = FALSE)),
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = neutralBlackText)
      )
    )
  )
)

# Highcharts theme for hex map
hc_theme_map_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = "Graphik",
                   fontSize = "14px",
                   color = neutralBlackText)
    ),
    caption = list(align = "right", y = 15),
    xAxis = list(
      labels = list(
        style = list(fontSize = "15px"),
        staggerLines = 2
      ),
      gridLineColor = "transparent"
    ),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    ),
    legend = list(
      itemStyle = list(fontSize = "16px",
                       fontWeight = "regular")
    )
  )
)

# Highcharts theme for plots, has axis lines
hc_theme_jc_line <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik",
                            fontSize = "14px",
                            color = neutralBlackText)),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "18px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = neutralBlackText,
      fontSize = "16px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = neutralBlackText,
                     fontWeight = "regular")
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "12px",
                                               fontWeight = "regular"))
  ),
  plotOptions = list(
    bubble = list(maxSize = "10%"),
    column = list(
      dataLabels = list(
        style = list(color = neutralBlackText)
      )
    )
  )
)

# Create basic horizontal bar chart that isn't grouped
fnc_basic_barchart <- function(df, filter_column, accessibility_text) {

  xaxis_order <- df[[filter_column]]

  highcharts <- highchart() %>%
    hc_add_series(df,
                  type = "bar",
                  hcaes(x = !!sym(filter_column),
                        y = prop),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# Create basic horizontal bar chart that isn't grouped
fnc_basic_columnchart <- function(df, filter_column, accessibility_text) {

  xaxis_order <- levels(df[[filter_column]])

  highcharts <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(filter_column),
                        y = prop),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# Create grouped, stacked bar chart
fnc_grouped_stacked_barchart <- function(df, x_column, group_by_col, accessibility_text) {

  highcharts <-
    hchart(df, "bar",
         hcaes(x = !!sym(x_column),
               y = prop,
               group = !!sym(group_by_col)
         ),
         dataLabels = list(enabled = TRUE,
                           format = "{point.prop_label}",
                           style = list(fontWeight = "regular",
                                        fontSize = "12px",
                                        fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = accessibility_text,
        landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)

}


# Create grouped, not stacked bar chart
fnc_grouped_barchart <- function(df, x_column, group_by_col, accessibility_text) {

  highcharts <-
    hchart(df, "bar",
           hcaes(x = !!sym(x_column),
                 y = prop,
                 group = !!sym(group_by_col)
           ),
           dataLabels = list(enabled = TRUE,
                             format = "{point.prop_label}",
                             style = list(fontWeight = "regular",
                                          fontSize = "12px",
                                          fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = accessibility_text,
        landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)

}

# Create grouped, not stacked bar chart
fnc_grouped_columnchart <- function(df, x_column, group_by_col, accessibility_text) {

  highcharts <-
    hchart(df, "column",
           hcaes(x = !!sym(x_column),
                 y = prop,
                 group = !!sym(group_by_col)
           ),
           dataLabels = list(enabled = TRUE,
                             format = "{point.prop_label}",
                             style = list(fontWeight = "regular",
                                          fontSize = "12px",
                                          fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = accessibility_text,
        landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)

}





# Create basic horizontal bar chart that is grouped by adm type
############################ NEEDS ACCESSIBILITY TEXT CHANGE IN POP FILE
fnc_stackedbar_admtype_chart <- function(df, group_by_col) {
  highcharts <- hchart(df, "bar",
         hcaes(x = admtype,
               y = prop,
               group = !!sym(group_by_col)
         ),
         dataLabels = list(enabled = TRUE,
                           format = "{point.prop_label}",
                           style = list(fontWeight = "bold",
                                        fontSize = "12px",
                                        fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1
             ) %>%
    hc_xAxis(categories = c("New court commitment",
                            "Parole return/revocation",
                            "Other or Unknown"),
             title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal", animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = "TBD.", landmarkVerbosity = "one"),
      area = list(accessibility = list(description = "TBD.")))
  return(highcharts)
}

# Create pie chart
fnc_piechart <- function(df, x_column, accessibility_text){

  highcharts <- hchart(df,
                       "pie",
                       hcaes(x = !!sym(x_column), y = prop),
                       dataLabels = list(
                         style = list(fontSize = "1em",
                                      fontWeight = "regular",
                                      alignTo = "connectors",
                                      color = neutralBlackText),
                         enabled = TRUE,
                         formatter = JS(paste("function() { return this.point.name + ': <b>' + this.point.prop_label + '</b>';}"))
                         )
                       ) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(30, 0, 10, 0)) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))

}
























fnc_create_infograph <- function(setrri, infogs = 9, emptyhumans = TRUE, fillcolor = "#00aba0", fillHoriz=TRUE) {

  #######COLORS
  #not full human
  cols2 <- c(rgb(255,255,255,maxColorValue = 255), #white
             rgb(167,169,172,maxColorValue = 255), #CSGJC gray
             rgb(col2rgb(fillcolor)[1],col2rgb(fillcolor)[2],col2rgb(fillcolor)[3],
                 maxColorValue = 255))    #CSGJC blue
  #empty human colors
  cols0 <- c(rgb(255,255,255,maxColorValue = 255),
             rgb(167,169,172,maxColorValue = 255))
  #full human
  cols1 <- c(rgb(255,255,255,maxColorValue = 255),
             rgb(col2rgb(fillcolor)[1],col2rgb(fillcolor)[2],col2rgb(fillcolor)[3],
                 maxColorValue = 255))

  #########set RRI
  RRI       <- setrri        #RRI
  numfull   <- floor(RRI)    #round RRI to determine how many filled infographics
  numremain <- RRI - numfull #find partial fill for single infographic

  #########set number of rows to plot infographics
  if (infogs-setrri<1) {
    infogs<-floor(setrri)+2;
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ",floor(setrri)+2))
  }
  if (infogs>=10) {
    rows<-2
  } else {
    rows<-1
  }

  #########starting position of blank infographic humans
  blank <- numfull + 2

  # Find the rows where left arm starts and right arm ends
  if (fillHoriz==TRUE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y==1)))
    max  <- 182 #max position must be adjusted due to issues with finding max PNG fill
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
    max  <- 437 #max position must be adjusted due to issues with finding max PNG fill
  }
  h     <- dim(img)[1]
  w     <- dim(img)[2]
  min   <- min(pos1)

  #set colors, plots, and RRIs for looping graphics
  finalcolors <- c('cols2',       'cols0', 'cols1')
  finalplots  <- c('plot2',       'plot0', 'plot1')
  finalpcts   <- c(numremain*100, 0,       100)

  #configure how many plots to create based on user request
  if (emptyhumans==TRUE) {
    if (RRI>1) {numplots<-1:3} else {numplots<-1:2}
  } else {
    if (RRI>1) {numplots<-c(1,3)} else {numplots<-1}
  }

  #create three types of plots (not full, empty, full human)
  for (j in numplots) {
    #percent of interest
    pcts    <- finalpcts[j]
    pospct  <- round((max-min)*pcts/100+min)

    # Fill bodies with a different color according to percentages
    finalimg                 <- img[h:1,,1]
    bkgr                     <- (finalimg==1)
    colfill                  <- matrix(rep(FALSE,h*w),nrow=h)
    if (fillHoriz==TRUE) {
      colfill[1:h,max:pospct]  <- TRUE
    } else {
      colfill[max:pospct,1:w]  <- TRUE
    }
    finalimg[bkgr & colfill] <- 0.5

    #convert matrix into  df for ggplot
    df <- reshape2::melt(finalimg)

    #plot df
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_tile() +
      scale_fill_manual(values = unlist(mget(finalcolors[j]), use.names=FALSE)) +
      blankitout
    assign(finalplots[j],plot)
  }

  ############create grid of RRIs
  plot_list <- list()

  ############SET UP PLOTTING LIST
  #plot empty humans
  if (emptyhumans==TRUE) {

    #for RRI>1, create full human(s), not full human, empty human(s)
    if (RRI>1) {

      #create initial list of filled in infographics
      for (i in 1:numfull){
        #RRI>1, full human
        plot_list[[i]] <- plot1
      }
      #RRI>1, not full human
      plot_list[[numfull+1]] <- plot2

      #RRI>1, empty human
      for (i in blank:infogs){
        plot_list[[i]] <- plot0
      }

      #for RRI<1, not full human, empty humans
    } else {
      plot_list[[1]] <- plot2

      for (i in blank:infogs){
        plot_list[[i]] <- plot0
      }
    }

    #otherwise, DO NOT plot empty humans
  } else {

    #for RRI>1, create full human(s), not full human
    if (RRI>1) {

      #create initial list of filled in infographics
      for (i in 1:numfull){
        #RRI>1, full human
        plot_list[[i]] <- plot1
      }
      #RRI>1, not full human
      plot_list[[numfull+1]] <- plot2

      #for RRI<1, not full human
    } else {
      plot_list[[1]] <- plot2
    }
  }

  #plot the infographics!
  plot_grid(plotlist=plot_list,nrow=rows)

}


fnc_create_and_save_infograph <- function(state_name, race_name) {

  df1 <- rri_in_prison_data %>%
    filter(state == state_name) %>%
    filter(race == "Black, non-Hispanic")

  infographics <- fnc_create_infograph(df1$rri, emptyhumans = FALSE)

  file_name <- paste("rri_infograph_", state_name, ".png", sep = "")

  ggsave(file_name, plot = infographics, device = "png",
         width = 9, height = 5,
         path = "C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole/data/analysis/app/ggplots")
}

















# Create sentence length and timeserved order since they are categorical
# calculate proportion of sentence length served
# determine timing of release
#  https://www.icpsr.umich.edu/web/NACJD/studies/38492/datasets/0003/variables/PARELIG_YEAR?archive=nacjd
# fnc_sentlgth_timesrvd_rel <- function(data) {
#   data %>%
#     mutate(
#       sentlgth_order = case_when(
#         sentlgth == "< 1 year"      ~ 1,
#         sentlgth == "1-1.9 years"   ~ 2,
#         sentlgth == "2-4.9 years"   ~ 3,
#         sentlgth == "5-9.9 years"   ~ 4,
#         sentlgth == "10-24.9 years" ~ 5,
#         sentlgth == ">=25 years"    ~ 5,
#         sentlgth == "Life, LWOP, Life plus additional years, Death" ~ 5,
#         TRUE ~ NA),
#       timesrvd_rel_order = case_when(
#         timesrvd_rel == "< 1 year"      ~ 1,
#         timesrvd_rel == "1-1.9 years"   ~ 2,
#         timesrvd_rel == "2-4.9 years"   ~ 3,
#         timesrvd_rel == "5-9.9 years"   ~ 4,
#         timesrvd_rel == ">=10 years"    ~ 5,
#         TRUE ~ NA),
#       timesrvd_rel_order = as.numeric(timesrvd_rel_order),
#       sentlgth_order = as.numeric(sentlgth_order),
#       proportion_served = ifelse(is.na(timesrvd_rel_order) |
#                                    is.na(sentlgth_order), NA,
#                                  timesrvd_rel_order / sentlgth_order)
#     ) %>%
#     mutate(
#       timesrvd_rel_vs_sentlgth = case_when(
#         is.na(timesrvd_rel_order) | is.na(sentlgth_order) ~ NA,
#         timesrvd_rel_order == sentlgth_order ~ "Full Sentence Length Served",
#         timesrvd_rel_order > sentlgth_order  ~ "More than Sentence Length Served",
#         timesrvd_rel_order < sentlgth_order  ~ "Less than Sentence Length Served"),
#       time_served = relyr - admityr
#     ) %>%
#     mutate(
#       parelig_year_clean =
#         ifelse(parelig_year <= 2105, parelig_year, NA),
#       mand_prisrel_year_clean =
#         ifelse(mand_prisrel_year <= 2105, mand_prisrel_year, NA),
#       time_between_release_ped = relyr - parelig_year_clean,
#       time_between_ped_admission = parelig_year_clean - admityr,
#       time_between_mandatoryrelease_release = mand_prisrel_year_clean - relyr,
#       time_between_release_admissions = relyr - admityr
#     ) %>%
#     mutate(
#       released_at_ped_status = case_when(
#         time_between_release_ped < 0 ~ "Released Before Parole Eligibility Year",
#         time_between_release_ped == 0 ~ "Released on Parole Eligibility Year",
#         time_between_release_ped > 0 ~ "Released After Parole Eligibility Year",
#         is.na(time_between_release_ped) ~ NA
#       )
#     )
# }
