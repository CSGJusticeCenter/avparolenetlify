####################
# Author:
# Date Last Updated:
# File Name:
# File Description:
####################


#------ Highchart Themes and Functions ------#

#' Common Style Elements
#'
#' This list defines the common style elements used across different themes.
#' @return A list of common style elements.
#' @export
common_style <- list(
  fontFamily = "Franklin Gothic Book",
  color = "black",
  fontSize = "14px",
  fontWeight = "regular"
)

#' Common Chart Style
#'
#' This list defines the common chart style elements used across different themes.
#' @return A list of common chart style elements.
#' @export
common_chart_style <- list(
  fontFamily = "Franklin Gothic Book",
  fontSize = "14px",
  color = "black"
)

#' Common Title Style
#'
#' This list defines the common title style elements used across different themes.
#' @return A list of common title style elements.
#' @export
common_title_style <- list(
  fontFamily = "Franklin Gothic Book",
  fontWeight = "bold",
  color = "black"
)

#' Base Highcharts Theme
#'
#' This theme serves as the base for other themes.
#' @export
base_hc_theme <- hc_theme(
  colors = c(blue, orange, green, red),
  chart = list(style = common_chart_style),
  title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
  subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
  legend = list(
    align = "center",
    verticalAlign = "top",
    itemStyle = common_style
  ),
  xAxis = list(
    labels = list(enabled = TRUE, style = common_style),
    gridLineColor = "transparent",
    lineColor = "transparent",
    minorGridLineColor = "transparent",
    tickColor = "transparent"
  ),
  yAxis = list(
    title = list(text = ""),
    labels = list(enabled = TRUE,
                  style = common_style,
                  formatter = JS("function() { return Highcharts.numberFormat(this.value, 0, '.', ','); }")),
    #gridLineColor = "transparent",
    #tickColor = "transparent",
    lineColor = "transparent",
    majorGridLineColor = "transparent",
    minorGridLineColor = "transparent"
  ),
  tooltip = list(
    style = common_style
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

# Create basic horizontal bar chart that isn't grouped
fnc_hc_barchart <- function(df, x_var, y_var, accessibility_text) {

  xaxis_order <- df[[x_var]]

  highcharts <- highchart() %>%
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_var),
                        y = !!sym(y_var)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Franklin Gothic Book",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1.1
    ) %>%
    hc_add_theme(base_hc_theme) %>%
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
