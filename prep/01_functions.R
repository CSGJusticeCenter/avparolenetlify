


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
  color = "#3E4B4B"
)

# highcharts theme for hex map
hc_theme_map_jc <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      marginTop = 75,
      style = list(fontFamily = "Graphik",
                   color = "#3E4B4B")
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

  lev_parelig_status <- c("Current", "Future", "Missing")

  df %>%
    mutate(
      parelig_status = case_when(
        parelig_year <  rptyear ~ lev_parelig_status[1],
        parelig_year >= rptyear ~ lev_parelig_status[2],
        is.na(parelig_year)     ~ lev_parelig_status[3]),
      parelig_status = factor(parelig_status,
                              levels = lev_parelig_status))

}

# Highcharts theme for plots
hc_theme_jc <- hc_theme(#colors = c("#D25E2D", "#EDB799", "#C7E8F5", "#236ca7", "#D6C246", "#dcdcdc"),

  # colors = c(orange, yellow, red, purple, darkblue, teal, blue, neutralBkgndMedium),
  colors = c(orange, yellow, purple, darkblue, teal, blue),

  chart = list(style = list(fontFamily = "Graphik",
                            color      = neutralBlackText)),
  title = list(align = "center",
               style = list(fontFamily = "Graphik",
                            fontWeight = "bold",
                            color = neutralBlackText,
                            fontSize   = "16px")),
  subtitle = list(align = "center",
                  style = list(fontFamily = "Graphik",
                               fontWeight = "bold",
                               color = neutralBlackText,
                               fontSize   = "14px")),
  chart = list(style = list(fontFamily = "Graphik", color = neutralBlackText)),
  legend = list(align = "center", verticalAlign = "top"),
  xAxis = list(labels = list(enabled = TRUE),
               gridLineColor = "transparent",
               lineColor = "transparent",
               minorGridLineColor = "transparent",
               tickColor = "transparent"),
  yAxis = list(labels = list(enabled = TRUE),
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

# Highcharts theme for plots
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
                            point_format,
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
                    style = list(fontSize = "2em",
                                 color = neutralBlackText),
                    enabled = TRUE,
                    distance= -60,
                    format = point_format)) %>%
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
                   size = '110%',
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
fnc_pie_chart <- function(df,
                          x_variable,
                          y_variable,
                          point_format,
                          accessibility_text){

  df$x_variable <- get(x_variable, df)
  df$y_variable <- get(y_variable, df)

  df %>%
    hchart("pie",
           hcaes(x = x_variable, y = y_variable),
           dataLabels = list(
             style = list(fontSize = "1.25em",
                          fontWeight = "regular",
                          alignTo = "connectors",
                          color = neutralBlackText),
             enabled = TRUE,
             y = -10,
             format = point_format)) %>%

    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))
}

