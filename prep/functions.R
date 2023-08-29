#######################################
# Project: AV Parole
# File: functions.R
# Authors: Mari Roberts
# Date last updated: August 28, 2023 (MAR)
# Description:
#    Custom functions
#######################################



###################
# Data prep
###################

# prepare annual parole survey data for analysis
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

# prepare annual parole survey data for analysis
# before 2008, there was no enreltsr variable so make NA
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


# custom function to create parole eligibility status
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

# create sentence length and timeserved order since they are categorical
# calculate proportion of sentence length served
# determine timing of release
#  https://www.icpsr.umich.edu/web/NACJD/studies/38492/datasets/0003/variables/PARELIG_YEAR?archive=nacjd
fnc_sentlgth_timesrvd_rel <- function(data) {
  data %>%
    mutate(
      sentlgth_order = case_when(
        sentlgth == "< 1 year"      ~ 1,
        sentlgth == "1-1.9 years"   ~ 2,
        sentlgth == "2-4.9 years"   ~ 3,
        sentlgth == "5-9.9 years"   ~ 4,
        sentlgth == "10-24.9 years" ~ 5,
        sentlgth == ">=25 years"    ~ 5,
        sentlgth == "Life, LWOP, Life plus additional years, Death" ~ 5,
        TRUE ~ NA),
      timesrvd_rel_order = case_when(
        timesrvd_rel == "< 1 year"      ~ 1,
        timesrvd_rel == "1-1.9 years"   ~ 2,
        timesrvd_rel == "2-4.9 years"   ~ 3,
        timesrvd_rel == "5-9.9 years"   ~ 4,
        timesrvd_rel == ">=10 years"    ~ 5,
        TRUE ~ NA),
      timesrvd_rel_order = as.numeric(timesrvd_rel_order),
      sentlgth_order = as.numeric(sentlgth_order),
      proportion_served = ifelse(is.na(timesrvd_rel_order) |
                                   is.na(sentlgth_order), NA,
                                 timesrvd_rel_order / sentlgth_order)
    ) %>%
    mutate(
      timesrvd_rel_vs_sentlgth = case_when(
        is.na(timesrvd_rel_order) | is.na(sentlgth_order) ~ NA,
        timesrvd_rel_order == sentlgth_order ~ "Full Sentence Length Served",
        timesrvd_rel_order > sentlgth_order  ~ "More than Sentence Length Served",
        timesrvd_rel_order < sentlgth_order  ~ "Less than Sentence Length Served"),
      time_served = relyr - admityr
    ) %>%
    mutate(
      parelig_year_clean =
        ifelse(parelig_year <= 2105, parelig_year, NA),
      mand_prisrel_year_clean =
        ifelse(mand_prisrel_year <= 2105, mand_prisrel_year, NA),
      time_between_release_ped = relyr - parelig_year_clean,
      time_between_ped_admission = parelig_year_clean - admityr,
      time_between_mandatoryrelease_release = mand_prisrel_year_clean - relyr,
      time_between_release_admissions = relyr - admityr
    ) %>%
    mutate(
      released_at_ped_status = case_when(
        time_between_release_ped < 0 ~ "Released Before Parole Eligibility Year",
        time_between_release_ped == 0 ~ "Released on Parole Eligibility Year",
        time_between_release_ped > 0 ~ "Released After Parole Eligibility Year",
        is.na(time_between_release_ped) ~ NA
      )
    )
}




###################
# Reactable
###################

# highcharts theme for reactable tables
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

# overall highcharts theme for plots
hc_theme_jc <- hc_theme(
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
      fontSize = "16px"
    )
  ),
  subtitle = list(
    align = "center",
    style = list(
      fontFamily = "Graphik",
      fontWeight = "regular",
      color = neutralBlackText,
      fontSize = "14px"
    )
  ),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = list(color = neutralBlackText)
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "1em",
                                               fontWeight = "regular")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontSize = "1em")),
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

# highcharts theme for hex map
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

# highcharts theme for plots, has axis lines
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
    itemStyle = list(color = neutralBlackText)
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText,
                                               fontWeight = "bold")),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    labels = list(enabled = TRUE, style = list(color = neutralBlackText))
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



# Create highchart bar chart showing timing of release (released before, on, after PED) by offense type
fnc_create_bar_chart_released_at_ped <- function(selected_offgeneral, accessibility_text) {

  states <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(offgeneral == selected_offgeneral) %>%
    pull(state) %>%
    unique()

  all_bar <- map(states, function(x) {

    df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
      filter(state == x, offgeneral == selected_offgeneral) %>%
      arrange(match(released_at_ped_status, desired_order))

    # assign color for each race
    df1$color <- case_when(df1$released_at_ped_status == "Released Before Parole Eligibility Year" ~ purple,
                           df1$released_at_ped_status == "Released on Parole Eligibility Year" ~ teal,
                           df1$released_at_ped_status == "Released After Parole Eligibility Year" ~ orange)
    df1$color <- htmltools::parseCssColors(df1$color)

    highcharts <- highchart() %>%
      hc_add_series(df1, type = "column",
                    hcaes(x = factor(released_at_ped_status), y = prop, color = color),
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.prop_label}",
                                      style = list(fontWeight = "bold",
                                                   fontSize = "1em",
                                                   fontFamily = "Graphik",
                                                   textOutline = 0))) %>%
      hc_xAxis(categories = df1$released_at_ped_status) %>%
      hc_yAxis(labels = list(enabled = FALSE)) %>%
      hc_add_theme(hc_theme_jc) %>%
      hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE) %>%
      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3,
                                   minPointLength = 4),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = "TBD",
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = "TBD")))

    return(highcharts)
  })

  return(setNames(all_bar, states))
}


# Create highchart bar chart showing LOS by offense type
fnc_create_bar_chart_los <- function(selected_offgeneral, accessibility_text) {

  states <- ncrp_proportion_served_offenses_2020 %>%
    filter(offgeneral == selected_offgeneral) %>%
    pull(state) %>%
    unique()

  all_bar <- map(states, function(x) {

    df1 <- ncrp_proportion_served_offenses_2020 %>%
      filter(state == x, offgeneral == selected_offgeneral) %>%
      arrange(match(timesrvd_rel_vs_sentlgth, desired_order))

    # assign color for each race
    df1$color <- case_when(df1$timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served" ~ yellow,
                           df1$timesrvd_rel_vs_sentlgth == "Full Sentence Length Served" ~ purple)
    df1$color <- htmltools::parseCssColors(df1$color)

    highcharts <- highchart() %>%
      hc_add_series(df1, type = "column",
                    hcaes(x = factor(timesrvd_rel_vs_sentlgth), y = prop, color = color),
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.prop_label}",
                                      style = list(fontWeight = "bold",
                                                   fontSize = "1em",
                                                   fontFamily = "Graphik",
                                                   textOutline = 0))) %>%
      hc_xAxis(categories = df1$timesrvd_rel_vs_sentlgth) %>%
      hc_yAxis(labels = list(enabled = FALSE)) %>%
      hc_add_theme(hc_theme_jc) %>%
      hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE) %>%
      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3,
                                   minPointLength = 4),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = "TBD",
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = "TBD")))

    return(highcharts)
  })

  return(setNames(all_bar, states))
}
