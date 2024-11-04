library(highcharter)
library(dplyr)
library(readr)

# Load the required data
hex_gj <- readRDS("path/to/your/hex_gj.rds")
map_data_breaks <- readRDS("path/to/your/map_data_breaks.rds")
breaks <- readRDS("path/to/your/breaks.rds")

# Define colors
green1 <- "#b1d4d5"
green2 <- "#49a7a1"
green3 <- "#176f6d"
green4 <- "#104040"
yellow <- "#decf64"
darkgray <- "#969696"

# Create hex map
highchart(height = 625) %>%
  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(
      enabled = TRUE,
      useHTML = TRUE,
      align = "center",
      verticalAlign = "middle",
      formatter = JS("function() {
        return '<div style=\"text-align:center; font-weight:regular; white-space:nowrap; line-height:1.2em;\">' +
                       this.point.state_abb + '<br>' + this.point.change_label + '</div>';
      }"),
      style = list(
        fontSize = "16px",
        fontWeight = "regular",
        fontFamily = "Graphik",
        textOutline = 0,
        whiteSpace = "nowrap"
      )
    ),
    borderColor = "white",
    nullColor = darkgray
  ) %>%

  hc_colorAxis(dataClassColor = "category",
               dataClasses = list(
                 list(from = 1, to = 1, color = green1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = green2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = green3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = green4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = yellow, name = "Abolished Parole"),
                 list(from = 6, to = 6, color = darkgray, name = "Missing Data")
               )) %>%

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = -10,
            y = -40,
            itemMarginTop = 2,
            itemMarginBottom = 2) %>%

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) %>%

  hc_exporting(
    enabled = TRUE,
    filename = gsub(" ", "_", tolower("Map Past Parole Eligibility by State 2023")),
    scale = 1,
    allowHTML = TRUE,
    sourceWidth = 800,
    sourceHeight = 600)

