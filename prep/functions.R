#######################################
# Project: AV Parole
# File: functions.R
# Authors: Mari Roberts
# Date last updated: October 23, 2023 (MAR)
# Description:
#    Custom functions
#######################################

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

# Filter by
fnc_parameters <- function(df){
  df <- df %>%
    filter(admtype == "New court commitment") %>%
    filter(sentlgth == "1-1.9 years" |
             sentlgth == "2-4.9 years" |
             sentlgth == "5-9.9 years" |
             sentlgth == "10-24.9 years")

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



# Create tooltip
fnc_tooltip <- function(df, xaxis_column, count_column, count_column_name) {
  df %>%
    mutate(tooltip =
             paste0("<b>", state, "</b><br><br>",
                    {{ count_column_name }}, "<br>",
                    {{ xaxis_column }}, "<br><br>",
                    "<b>", {{ count_column }}, "</b><br><br>"))
}

# Calculate n, prop, and create labels
fnc_values_labels <- function(df, count_column) {
  df %>%
    count({{ count_column }}) %>%
    mutate(
      prop = (n / sum(n)),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0)
    )
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
  color = "black"
)






###################
# Highcharter
###################

# base Highcharts theme
base_hc_theme <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(
    style = list(fontFamily = "Graphik",
                 fontSize = "12px",
                 color = "black")
  ),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(
      color = "black",
      fontSize = "12px",
      fontWeight = "regular"
    )
  ),
  xAxis = list(
    labels = list(
      enabled = TRUE,
      style = list(
        color = "black",
        fontSize = "12px",
        fontWeight = "regular"
      )
    ),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(
      enabled = TRUE,
      style = list(
        color = "black",
        fontSize = "12px",
        fontWeight = "regular"
      )
    ),
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
        style = list(color = "black")
      )
    )
  )
)

# specific themes based on the base theme
hc_theme <- base_hc_theme

# with lines and y axis labels
# hc_theme_with_line <- base_hc_theme
# hc_theme_with_line$yAxis$labels <-
#   list(enabled = TRUE, style = list(color = "black",
#                                     fontSize = "12px",
#                                     fontWeight = "regular"))
hc_theme_with_line <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik",
                            fontSize = "12px",
                            color = "black")),
  title = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "bold",
      color = "black",
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = "black",
                     fontSize = "12px",
                     fontWeight = "regular")
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = "black",
                                               fontSize = "12px",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = "black",
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
        style = list(color = "black")
      )
    )
  )
)

# for map
hc_theme_map <- hc_theme_merge(
  hc_theme_smpl(),
  base_hc_theme,
  hc_theme(
    chart = list(
      style = list(fontFamily = "Graphik",
                   fontSize = "14px",
                   color = "black")
    ),
    title = list(
      align = "center",
      style = list(
        fontFamily = "Graphik",
        fontWeight = "bold",
        color = "black",
        fontSize = "22px"
      )
    ),
    plotOptions = list(
      series = list(states = list(inactive = list(opacity = 1))),
      line = list(marker = list(enabled = TRUE)),
      spline = list(marker = list(enabled = TRUE)),
      area = list(marker = list(enabled = TRUE)),
      areaspline = list(marker = list(enabled = TRUE))
    ),
    legend = list(
      itemStyle = list(fontSize = "16px", fontWeight = "regular")
    )
  )
)

# Column chart (vertical bars)
fnc_columnchart <- function(df, filter_column, yAxis_text, accessibility_text) {

  xaxis_order <- df[[filter_column]]

  highcharts <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(filter_column),
                        y = total)) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = TRUE),
             title = list(text = yAxis_text)) %>%
    hc_add_theme(hc_theme_with_line) %>%
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

# Number of people by category by admission type
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
        stacking = "normal",
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

# Create basic horizontal bar chart that isn't grouped
fnc_barchart <- function(df, filter_column, accessibility_text) {

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




























# fnc_generate_rri_highlight <- function(df, scenario){
#
#   rri_black <- pull(df %>% filter(race == "Black, non-Hispanic") %>% select(rri))
#   rri_hispanic <- pull(df %>% filter(race == "Hispanic, any race") %>% select(rri))
#   rri_other <- pull(df %>% filter(race == "Other race(s), non-Hispanic") %>% select(rri))
#
#   use_percent <- any(c(rri_black, rri_hispanic, rri_other) < 1)
#
#   if(use_percent) {
#     rri_black <- round(rri_black * 100)
#     rri_hispanic <- round(rri_hispanic * 100)
#     rri_other <- round(rri_other * 100)
#   }
#
#   text_black    <- case_when(rri_black < 100  ~ "less likely",
#                              rri_black > 100  ~ "more likely",
#                              TRUE             ~ "equally as likely")
#   text_hispanic <- case_when(rri_hispanic < 100  ~ "less likely",
#                              rri_hispanic > 100  ~ "more likely",
#                              TRUE                ~ "equally as likely")
#   text_other    <- case_when(rri_other < 100  ~ "less likely",
#                              rri_other > 100  ~ "more likely",
#                              TRUE             ~ "equally as likely")
#
#   unit_text <- ifelse(use_percent, "%", "times")
#
#   rri_black_display    <- ifelse(use_percent, paste0(rri_black, "%"), rri_black)
#   rri_hispanic_display <- ifelse(use_percent, paste0(rri_hispanic, "%"), rri_hispanic)
#   rri_other_display    <- ifelse(use_percent, paste0(rri_other, "%"), rri_other)
#
#   div(id = "body-section",
#       div(
#         id = "grid-container",
#         style = "display: grid; grid-template-columns: repeat(3, 1fr); justify-items: center; column-gap: 30px;",
#         div(id = "bold-text", HTML("Black, non-Hispanic")),
#         div(id = "bold-text", HTML("Hispanic, any race")),
#         div(id = "bold-text", HTML("Other race(s), non-Hispanic")),
#         div(id = "highlight-text", format(as.numeric(rri_black), big.mark = ","), ifelse(use_percent, "%", "")),
#         div(id = "highlight-text", format(as.numeric(rri_hispanic), big.mark = ","), ifelse(use_percent, "%", "")),
#         div(id = "highlight-text", format(as.numeric(rri_other), big.mark = ","), ifelse(use_percent, "%", "")),
#         div(id = "regular-text", HTML(text_black,    " to be ", scenario, " compared to White people")),
#         div(id = "regular-text", HTML(text_hispanic, " to be ", scenario, " compared to White people")),
#         div(id = "regular-text", HTML(text_other,    " to be ", scenario, " compared to White people"))
#       ), br(),
#       div(id = "note-text", "Note: The Relative Rate Index (RRI) measures racial and
#       ethnic disparities by comparing rates between groups, often using White individuals as the reference.
#       The RRI provides insight into the degree of overrepresentation or underrepresentation,
#       with an RRI greater than 1 indicating that a particular racial or ethnic group is
#       disproportionately represented.")
#   )
# }
fnc_generate_rri_highlight <- function(df, scenario){

  rri_black <- pull(df %>% filter(race == "Black, non-Hispanic") %>% select(rri))
  rri_hispanic <- pull(df %>% filter(race == "Hispanic, any race") %>% select(rri))
  rri_other <- pull(df %>% filter(race == "Other race(s), non-Hispanic") %>% select(rri))

  use_percent <- any(c(rri_black, rri_hispanic, rri_other) >= 1)

  if(use_percent) {
    rri_black <- round(rri_black * 100)
    rri_hispanic <- round(rri_hispanic * 100)
    rri_other <- round(rri_other * 100)
  }

  text_black    <- case_when(rri_black < 100  ~ "less likely",
                             rri_black >= 100  ~ "more likely",
                             TRUE             ~ "equally as likely")
  text_hispanic <- case_when(rri_hispanic < 100  ~ "less likely",
                             rri_hispanic >= 100  ~ "more likely",
                             TRUE                ~ "equally as likely")
  text_other    <- case_when(rri_other < 100  ~ "less likely",
                             rri_other >= 100  ~ "more likely",
                             TRUE             ~ "equally as likely")

  unit_text <- ifelse(use_percent, "%", "times")

  rri_black_display    <- ifelse(use_percent, paste0(rri_black, unit_text), rri_black)
  rri_hispanic_display <- ifelse(use_percent, paste0(rri_hispanic, unit_text), rri_hispanic)
  rri_other_display    <- ifelse(use_percent, paste0(rri_other, unit_text), rri_other)

  div(id = "body-section",
      div(
        id = "grid-container",
        style = "display: grid; grid-template-columns: repeat(3, 1fr); justify-items: center; column-gap: 30px;",
        div(id = "bold-text", "Black, non-Hispanic"),
        div(id = "bold-text", "Hispanic, any race"),
        div(id = "bold-text", "Other race(s), non-Hispanic"),
        div(id = "highlight-text", rri_black_display),
        div(id = "highlight-text", rri_hispanic_display),
        div(id = "highlight-text", rri_other_display),
        div(id = "regular-text", text_black,    " to be ", scenario, " compared to White people"),
        div(id = "regular-text", text_hispanic, " to be ", scenario, " compared to White people"),
        div(id = "regular-text", text_other,    " to be ", scenario, " compared to White people")
      ), br(), br(),
      div(id = "note-text", "Note: The Relative Rate Index (RRI) measures racial and
      ethnic disparities by comparing rates between groups, often using White individuals as the reference.
      The RRI provides insight into the degree of overrepresentation or underrepresentation,
      with an RRI greater than 1 indicating that a particular racial or ethnic group is
      disproportionately represented. Source: Census (2020) and BJS Prisoners (2020)")
  )
}




