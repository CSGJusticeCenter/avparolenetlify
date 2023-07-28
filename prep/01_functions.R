#######################################
# Project: AV Parole
# File: functions.R
# Authors: Mari Roberts
# Date last updated: June 12, 2023 (MAR)
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





###################
# Plots
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
  fontSize = "0.75rem",
  color = neutralBlackText
)

# highcharts theme for hex map
hc_theme_map_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = "Graphik",
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
    )
  )
)


# highcharts theme for plots
hc_theme_jc <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik", color = neutralBlackText)),
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
    labels = list(enabled = TRUE, style = list(color = neutralBlackText)),
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

# highcharts theme for line plots
hc_theme_jc_line <- hc_theme(
  colors = c(orange, yellow, purple, darkblue, teal, blue),
  chart = list(style = list(fontFamily = "Graphik", color = neutralBlackText)),
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

# highcharts theme for pie plots
hc_theme_jc_pie <- hc_theme(
  colors = c(teal, neutralBkgndMedium),
  chart =
    list(style =
           list(fontFamily = "Graphik",
                color      = neutralBlackText)),
  title =
    list(align = "center",
         style =
           list(fontFamily = "Graphik",
                fontWeight = "bold",
                color      = neutralBlackText,
                fontSize   = "16px")),
  subtitle =
    list(align = "center",
         style =
           list(fontFamily = "Graphik",
                color      = neutralBlackText,
                fontSize   = "14px")),
  chart =
    list(style =
           list(fontFamily = "Graphik",
                color      = neutralBlackText)),
  legend =
    list(align = "center", verticalAlign = "top"),

  xAxis =
    list(labels =
           list(enabled = TRUE),
         gridLineColor = "transparent",
         lineColor = "transparent",
         minorGridLineColor = "transparent",
         tickColor = "transparent"),
  yAxis =
    list(labels =
           list(enabled = TRUE),
         gridLineColor = "transparent",
         lineColor = "transparent",
         majorGridLineColor = "transparent",
         minorGridLineColor = "transparent",
         tickColor = "transparent"),
  plotOptions =
    list(line =
           list(marker = list(enabled = FALSE)),
         spline = list(marker = list(enabled = FALSE)),
         area = list(marker = list(enabled = FALSE)),
         areaspline = list(marker = list(enabled = FALSE)),
         arearange = list(marker = list(enabled = FALSE)),
         bubble = list(maxSize = "10%")))

# Highcharts theme for plots
hc_theme_jc_minimal <- hc_theme(

  colors = c(orange, yellow, purple, darkblue, teal, blue),

  chart = list(style = list(fontFamily = "Graphik",
                            color = neutralBlackText)),
  title = list(align = "center",
               style = list(fontFamily = "Graphik",
                            fontWeight = "bold",
                            color = neutralBlackText,
                            fontSize   = "18px")),
  subtitle = list(align = "center",
                  style = list(fontFamily = "Graphik",
                               fontWeight = "bold",
                               color = neutralBlackText,
                               fontSize   = "16px")),
  chart = list(style = list(fontFamily = "Graphik",
                            color = neutralBlackText)),
  legend = list(align = "center", verticalAlign = "top"),
  xAxis = list(labels = list(enabled = FALSE),
               gridLineColor = "transparent",
               lineColor = "transparent",
               minorGridLineColor = "transparent",
               tickColor = "transparent"),
  yAxis = list(labels = list(enabled = FALSE),
               gridLineColor = "transparent",
               lineColor = "transparent",
               majorGridLineColor = "transparent",
               minorGridLineColor = "transparent",
               tickColor = "transparent"),
  plotOptions = list(line = list(marker = list(enabled = FALSE)),
                     spline = list(marker = list(enabled = FALSE)),
                     area = list(marker = list(enabled = FALSE)),
                     areaspline = list(marker = list(enabled = FALSE)),
                     arearange = list(marker = list(enabled = FALSE)),
                     bubble = list(maxSize = "10%")))

# Highcharts download buttons
hc_setup <- function(x) {
  highcharter::hc_add_dependency(x, name = "plugins/series-label.js") %>%
    highcharter::hc_add_dependency(name = "plugins/accessibility.js") %>%
    highcharter::hc_add_dependency(name = "plugins/exporting.js") %>%
    highcharter::hc_add_dependency(name = "plugins/export-data.js") %>%
    highcharter::hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    highcharter::hc_exporting(enabled = TRUE)
}

# Create donut chart with overall finding label in middle
fnc_donut_chart <- function(df,
                            df_pct,
                            x_variable,
                            y_variable,
                            accessibility_text){

  df$x_variable <- get(x_variable, df)
  df$y_variable <- get(y_variable, df)

  df_pct$x_variable <- get(x_variable, df_pct)
  df_pct$y_variable <- get(y_variable, df_pct)

  highchart() %>%

    hc_add_series(type = "pie",
                  data = df_pct,
                  hcaes(x_variable, y_variable),
                  size = "100%",
                  center = c(50, 50),
                  innerSize="60%",
                  dataLabels = list(
                    style = list(fontSize = "2.7em",
                                 color = orange),
                    enabled = TRUE,
                    distance= -85,
                    format = "{point.prop_label}")) %>%
    hc_add_series(type = "pie",
                  data = df,
                  hcaes(x_variable, y_variable),
                  size = "100%",
                  center = c(50, 50),
                  innerSize="60%",
                  dataLabels = list(enabled = FALSE)) %>%

    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_add_theme(hc_theme_jc_pie) %>%
    hc_plotOptions(innersize = "50%",
                   startAngle = 90,
                   endAngle = 90,
                   center = list('50%', '75%'),
                   size = '75%',
                   series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))


  # highchart() %>%
  #
  #   hc_add_series(type = "pie",
  #                 data = df_pct,
  #                 hcaes(x_variable, y_variable),
  #                 size = "100%",
  #                 center = c(50, 50),
  #                 innerSize="60%",
  #                 dataLabels = list(
  #                   style = list(fontSize = "2em",
  #                                color = neutralBlackText),
  #                   enabled = TRUE,
  #                   distance= -60,
  #                   format = point_format)) %>%
  #   hc_add_series(type = "pie",
  #                 data = df,
  #                 hcaes(x_variable, y_variable),
  #                 size = "100%",
  #                 center = c(50, 50),
  #                 innerSize="60%",
  #                 dataLabels = list(enabled = FALSE)) %>%
  #
  #   hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  #   hc_add_theme(hc_theme_jc_pie) %>%
  #   hc_plotOptions(innersize = "50%",
  #                  startAngle = 90,
  #                  endAngle = 90,
  #                  center = list('50%', '75%'),
  #                  size = '110%',
  #                  series = list(animation = FALSE,
  #                                cursor = "pointer",
  #                                borderWidth = 3),
  #                  accessibility = list(enabled = TRUE,
  #                                       keyboardNavigation = list(enabled = TRUE),
  #                                       linkedDescription = accessibility_text,
  #                                       landmarkVerbosity = "one"),
  #                  area = list(accessibility = list(description = accessibility_text)))
}


# Create pie chart with labels
fnc_pie_chart <- function(df,
                          x_variable,
                          y_variable,
                          point_format,
                          accessibility_text){

  df$x_variable <- get(x_variable, df)
  df$y_variable <- get(y_variable, df)

  df %>%
    hchart("pie",
           # margin = c(0, NA, 0, NA), not working
           # size = "70%", makes too big
           hcaes(x = x_variable, y = y_variable),
           dataLabels = list(
             style = list(fontSize = "1.25em",
                          fontWeight = "bold",
                          alignTo = "connectors",
                          color = neutralBlackText),
             enabled = TRUE,
             # y = -10,
             format = point_format)) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(100, 0, 18, 0)
             # spacing = c(10, 0, 0, 0),
             ) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(#pie = list(startAngle = 100),
                   series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))
}

# Create pie chart with labels
fnc_pie_chart_highlight <- function(df,
                                    # state_name,
                                    x_variable,
                                    y_variable,
                                    point_format,
                                    accessibility_text){

  df$x_variable <- get(x_variable, df)
  df$y_variable <- get(y_variable, df)

  # df_pct <- df %>%
  #   filter(rptyear == 2020 &
  #          state == state_name &
  #          parelig_status == "Current" &
  #          admtype == "Parole return/revocation") %>%
  #   mutate(prop_label = paste0(round(prop, 0), "%"))

  # parole_return_percentage <-
  #   df %>%
  #   filter(rptyear == 2020 &
  #          state == state_name &
  #          parelig_status == "Current" &
  #          admtype == "Parole return/revocation") %>%
  #   pull(prop)
  # parole_return_percentage <- paste0(round(parole_return_percentage, 0), "%")

  df %>%
    hchart("pie",
           hcaes(x = x_variable, y = y_variable),
           dataLabels = list(
             style = list(fontSize = "0.9em",
                          fontWeight = "regular",
                          alignTo = "connectors",
                          color = neutralBlackText),
             enabled = TRUE,
             format = point_format)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(
      pie = list(innerSize = "60%"),
      series = list(animation = FALSE,
                    cursor = "pointer"),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = accessibility_text,
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = accessibility_text)))
}




# Create bar chart with labels showing PE status by adm type
fnc_percent_bar_chart_pestatus_admtype <-

  function(df, point_format, accessibility_text) {

    highcharts <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("New court commitment",
                              "Parole return/revocation")) %>%
      hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
      hc_add_series(data = subset(df, released_at_ped_status == "Released Before Parole Eligibility Year"),
                    name = "Released Before Parole Eligibility Year",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_series(data = subset(df, released_at_ped_status == "Released on Parole Eligibility Year"),
                    name = "Released on Parole Eligibility Year",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_series(data = subset(df, released_at_ped_status == "Released After Parole Eligibility Year"),
                    name = "Released After Parole Eligibility Year",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_theme(hc_theme_jc) %>%
      hc_colors(colors = c(purple, teal, orange)) %>%
      hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3,
                                   minPointLength = 4),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = accessibility_text,
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = accessibility_text))
      )

}

# create all percent bar chart for each admission type and offense type
fnc_create_all_percent_bar_chart_pestatus_admtype <- function(selected_offgeneral) {
  states <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(offgeneral == selected_offgeneral) %>%
    pull(state) %>%
    unique()

  all_bar <- map(states, function(x) {
    df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
      filter(state == x, offgeneral == selected_offgeneral) %>%
      arrange(match(released_at_ped_status, desired_order))
    highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                                         point_format = "{point.prop_label}",
                                                         accessibility_text = "TBD.")
    return(highcharts)
  })

  return(setNames(all_bar, states))
}

# Create bar chart with labels showing sentence duration by adm type
fnc_percent_bar_chart_sentence_admtype <-
  function(df, point_format, accessibility_text) {

    highcharts <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("New court commitment",
                              "Parole return/revocation")) %>%
      hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
      hc_add_series(data = subset(df, timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served"),
                    name = "Less than Sentence Length Served",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_series(data = subset(df, timesrvd_rel_vs_sentlgth == "Full Sentence Length Served"),
                    name = "Full Sentence Length Served",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_series(data = subset(df, timesrvd_rel_vs_sentlgth == "More than Sentence Length Served"),
                    name = "More than Sentence Length Served",
                    type = "column",
                    dataLabels = list(enabled = TRUE, format = point_format,
                                      style = list(fontWeight = "regular")),
                    hcaes(x = admtype, y = prop)) %>%
      hc_add_theme(hc_theme_jc) %>%
      hc_colors(colors = c(purple, yellow, orange)) %>%
      hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE,
                                   cursor = "pointer",
                                   borderWidth = 3,
                                   minPointLength = 4),
                     accessibility = list(enabled = TRUE,
                                          keyboardNavigation = list(enabled = TRUE),
                                          linkedDescription = accessibility_text,
                                          landmarkVerbosity = "one"),
                     area = list(accessibility = list(description = accessibility_text))
      )

  }
