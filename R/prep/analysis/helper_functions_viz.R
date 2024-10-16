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
  fontSize = "1em",
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
  fontSize = "1em",
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
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "1.2em"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "1em"))),
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
    column = list(
      dataLabels = list(
        style = common_style
      )
    )
  ),
  caption = list(
    align = "left",
    style = list(
      fontSize = "0.8em",
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

# hc_theme_map <- hc_theme_merge(
#   hc_theme_smpl(),
#   base_hc_theme,
#   hc_theme(
#     chart = list(style = modifyList(common_chart_style, list(fontSize = "16px"))),
#     title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "22px"))),
#     plotOptions = list(
#       series = list(states = list(inactive = list(opacity = 1))),
#       line = list(marker = list(enabled = TRUE)),
#       spline = list(marker = list(enabled = TRUE)),
#       area = list(marker = list(enabled = TRUE)),
#       areaspline = list(marker = list(enabled = TRUE))
#     ),
#     legend = list(
#       itemStyle = modifyList(common_style, list(fontSize = "1em"))
#     )
#   )
# )
#
# hc_theme_with_line <- hc_theme(
#   colors = c(color1, color2, color3, color4, color5),
#   chart = list(style = common_chart_style),
#   title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "1.2em"))),
#   subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "1em"))),
#   legend = list(align = "center", verticalAlign = "top", itemStyle = common_style),
#   xAxis = list(
#     labels = list(enabled = TRUE, style = common_style),
#     tickmarkPlacement = 'on',
#     tickLength = 5,
#     tickWidth = 1,
#     tickColor = "white",
#     lineColor = "black"
#   ),
#   yAxis = list(
#     labels = list(enabled = TRUE, style = common_style)
#   ),
#   plotOptions = list(
#     column = list(
#       dataLabels = list(
#         style = list(color = "black")
#       )
#     )
#   )
# )

# Define a function to add accessibility and plot options to a highchart object
# fnc_add_hc_accessibility <- function(hc_object, accessibility_text) {
#   hc_object |>
#     hc_plotOptions(series = list(animation = FALSE,
#                                  cursor = "pointer",
#                                  borderWidth = 3,
#                                  minPointLength = 4),
#                    accessibility = list(enabled = TRUE,
#                                         keyboardNavigation = list(enabled = TRUE),
#                                         linkedDescription = text,
#                                         landmarkVerbosity = "one"),
#                    area = list(accessibility = list(description = accessibility_text)))
# }
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


fnc_hc_pie <- function(df, variable, title, accessibility_text, year = select_year, source = ncrp_source) {
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


fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type, year = select_year, source = ncrp_source) {

  df1 <- df |>
    filter(state == state_var) |>
    fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var))

  # Conditionally arrange by prop if x_var is "race" "fbi_index" or "sex"
  # Don't arrange if sentence length or age since these need to be in order
  if (x_var %in% c("race", "fbi_index", "sex")) {
    df1 <- df1 |> arrange(desc(prop))
  }

  title <- paste0(title_type, " by ", metric)

  accessibility_text <- paste0("This graph shows the percentage of ", type,
                               " by ", tolower(metric), " in ",
                               year, " in the state of ", state_var, ".")

  xaxis_order <- df1[[x_var]]

  highcharts <- highchart() |>
    hc_add_series(df1,
                  type = "column",
                  hcaes(x = !!sym(x_var),
                        y = !!sym(y_var)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) |>
    hc_xAxis(categories = xaxis_order,
             labels = list(
               useHTML = TRUE,
               enabled = TRUE,
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
               style = list(fontSize = "1em", fontFamily = "Graphik",
                            textAlign = "center" )
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

fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type, year = select_year) {

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

# fnc_generate_columnchart_sentence <- function(state, df, x_var, type, year = select_year) {
#
#   df1 <- df |>
#     filter(state == x) |>
#     arrange(-prop) |>
#     slice(1)
#
#   # Modify df1[[x_var]] to lowercase if x_var is "sex"
#   if (x_var == "sex") {
#     df1[[x_var]] <- tolower(df1[[x_var]])
#   }
#
#   # Check if x_var is "ageyrend" to format the sentence differently
#   if (x_var == "ageyrend") {
#     age_range <- strsplit(as.character(df1[[x_var]]), "-")[[1]]
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were between the ages of ",
#                         age_range[1], " and ", age_range[2], " years old.")
#   } else if (x_var == "fbi_index") {
#     # Lowercase the crime for proper sentence structure
#     crime <- tolower(df1[[x_var]])
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were incarcerated for ",
#                         crime, ".")
#   } else {
#     sentences <- paste0("In ", year, ", ", round(df1$prop, 0),
#                         " percent of people ", type, " were ",
#                         df1[[x_var]], ".")
#   }
#
#   return(sentences)
# }



# fnc_hc_columnchart <- function(df, x_var, y_var, accessibility_text) {
#
#   xaxis_order <- df[[x_var]]
#
#   highcharts <- highchart() |>
#     hc_add_series(df,
#                   type = "column",
#                   hcaes(x = !!sym(x_var),
#                         y = !!sym(y_var)),
#                   dataLabels = list(enabled = TRUE,
#                                     format = "{point.prop_label}",
#                                     style = list(fontWeight = "regular",
#                                                  fontSize = "1em",
#                                                  fontFamily = "Graphik",
#                                                  textOutline = 0))) |>
#     hc_xAxis(categories = xaxis_order,
#              labels = list(
#                useHTML = TRUE,
#                enabled = TRUE,
#                formatter = JS(
#                  "function() {
#                     var label = this.value;
#                     var maxLength = 15;
#                     if (label.length > maxLength) {
#                       var words = label.split(' ');
#                       var result = [];
#                       var line = [];
#                       var lineLength = 0;
#
#                       words.forEach(function(word) {
#                         if (lineLength + word.length > maxLength) {
#                           result.push(line.join(' '));
#                           line = [];
#                           lineLength = 0;
#                         }
#                         line.push(word);
#                         lineLength += word.length + 1;
#                       });
#                       if (line.length > 0) {
#                         result.push(line.join(' '));
#                       }
#                       return result.join('<br>');
#                     } else {
#                       return label;
#                     }
#                   }"
#                ),
#                style = list(fontSize = "1em", fontFamily = "Graphik",
#                             textAlign = "center" )
#              )) |>
#     hc_yAxis(max = 100,
#              labels = list(
#                formatter = JS("function() { return this.value + '%'; }")
#              )) |>
#
#     hc_add_theme(base_hc_theme) |>
#
#     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>
#
#     hc_legend(enabled = FALSE) |>
#
#     hc_title(text = paste0(title, ", ", select_year)) |>
#
#     hc_exporting(enabled = TRUE,
#                  filename = paste0(gsub(" ", "_", tolower(title)), "_", select_year)) |>
#
#     fnc_add_hc_accessibility(accessibility_text)
#
#   return(highcharts)
# }

fnc_xaxis_labels_right <- list(
  useHTML = TRUE,
  enabled = TRUE,
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
  style = list(fontSize = "1em", fontFamily = "Graphik",
               textAlign = "right" )
)

