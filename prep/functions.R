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

# Calculate n, prop, and create labels and tooltips
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

# Prepare Annual Parole Survey data for analysis
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

# Prepare Annual Parole Survey data for analysis
# Before 2008, there was no enreltsr variable so make NA
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

# Define  function to retrieve and process census data for a given state
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
        variable %in% c("estimate_american_indian", "estimate_asian", "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
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
fnc_basic_piechart <- function(df, x_column, accessibility_text){

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
































# # Create highchart bar chart showing timing of release (released before, on, after PED) by offense type
# fnc_create_bar_chart_released_at_ped <- function(selected_offgeneral, accessibility_text) {
#
#   states <- ncrp_released_at_ped_offgeneral_select_year %>%
#     filter(offgeneral == selected_offgeneral) %>%
#     pull(state) %>%
#     unique()
#
#   all_bar <- map(states, function(x) {
#
#     df1 <- ncrp_released_at_ped_offgeneral_select_year %>%
#       filter(state == x, offgeneral == selected_offgeneral) %>%
#       arrange(match(released_at_ped_status, desired_order))
#
#     # assign color for each race
#     df1$color <- case_when(df1$released_at_ped_status == "Released Before Parole Eligibility Year" ~ purple,
#                            df1$released_at_ped_status == "Released on Parole Eligibility Year" ~ teal,
#                            df1$released_at_ped_status == "Released After Parole Eligibility Year" ~ orange)
#     df1$color <- htmltools::parseCssColors(df1$color)
#
#     highcharts <- highchart() %>%
#       hc_add_series(df1, type = "column",
#                     hcaes(x = factor(released_at_ped_status), y = prop, color = color),
#                     dataLabels = list(enabled = TRUE,
#                                       format = "{point.prop_label}",
#                                       style = list(fontWeight = "bold",
#                                                    fontSize = "1em",
#                                                    fontFamily = "Graphik",
#                                                    textOutline = 0))) %>%
#       hc_xAxis(categories = df1$released_at_ped_status) %>%
#       hc_yAxis(labels = list(enabled = FALSE)) %>%
#       hc_add_theme(hc_theme_jc) %>%
#       hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#       hc_legend(enabled = FALSE) %>%
#       hc_exporting(enabled = FALSE) %>%
#       hc_plotOptions(series = list(animation = FALSE,
#                                    cursor = "pointer",
#                                    borderWidth = 3,
#                                    minPointLength = 4),
#                      accessibility = list(enabled = TRUE,
#                                           keyboardNavigation = list(enabled = TRUE),
#                                           linkedDescription = "TBD",
#                                           landmarkVerbosity = "one"),
#                      area = list(accessibility = list(description = "TBD")))
#
#     return(highcharts)
#   })
#
#   return(setNames(all_bar, states))
# }
#
#
# # Create highchart bar chart showing LOS by offense type
# fnc_create_bar_chart_los <- function(selected_offgeneral, accessibility_text) {
#
#   states <- ncrp_proportion_served_offenses_select_year %>%
#     filter(offgeneral == selected_offgeneral) %>%
#     pull(state) %>%
#     unique()
#
#   all_bar <- map(states, function(x) {
#
#     df1 <- ncrp_proportion_served_offenses_select_year %>%
#       filter(state == x, offgeneral == selected_offgeneral) %>%
#       arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
#
#     # assign color for each race
#     df1$color <- case_when(df1$timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served" ~ yellow,
#                            df1$timesrvd_rel_vs_sentlgth == "Full Sentence Length Served" ~ purple)
#     df1$color <- htmltools::parseCssColors(df1$color)
#
#     highcharts <- highchart() %>%
#       hc_add_series(df1, type = "column",
#                     hcaes(x = factor(timesrvd_rel_vs_sentlgth), y = prop, color = color),
#                     dataLabels = list(enabled = TRUE,
#                                       format = "{point.prop_label}",
#                                       style = list(fontWeight = "bold",
#                                                    fontSize = "1em",
#                                                    fontFamily = "Graphik",
#                                                    textOutline = 0))) %>%
#       hc_xAxis(categories = df1$timesrvd_rel_vs_sentlgth) %>%
#       hc_yAxis(labels = list(enabled = FALSE)) %>%
#       hc_add_theme(hc_theme_jc) %>%
#       hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#       hc_legend(enabled = FALSE) %>%
#       hc_exporting(enabled = FALSE) %>%
#       hc_plotOptions(series = list(animation = FALSE,
#                                    cursor = "pointer",
#                                    borderWidth = 3,
#                                    minPointLength = 4),
#                      accessibility = list(enabled = TRUE,
#                                           keyboardNavigation = list(enabled = TRUE),
#                                           linkedDescription = "TBD",
#                                           landmarkVerbosity = "one"),
#                      area = list(accessibility = list(description = "TBD")))
#
#     return(highcharts)
#   })
#
#   return(setNames(all_bar, states))
# }
