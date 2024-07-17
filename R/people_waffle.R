library(highcharter)
library(dplyr)
library(tidyverse)
library(base64enc)

# Define data
data <- tibble(
  group = c("Black", "White"),
  percentage = c(30, 70)
)

data <- current_ped_race |>
  filter(state == "Georgia") |>
  arrange(desc(n)) |>
  mutate(prop*100)


# Custom SVG icon with color placeholder
iconSVG <- "
<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24'>
  <path fill='%s' d='M12 12c2.21 0 4-1.79 4-4s-1.79-4-4-4-4 1.79-4 4 1.79 4 4 4zm0 2c-2.67 0-8 1.34-8 4v2h16v-2c0-2.66-5.33-4-8-4z'/>
</svg>"

# Colors for the groups
colors <- c("blue", "gray", "purple", "pink")

# Function to encode SVG icon with color
encode_icon <- function(color) {
  base64encode(charToRaw(sprintf(iconSVG, color)))
}

# Create the plot
highchart() %>%
  hc_chart(type = "item") %>%
  hc_title(text = "Percentage by Race") %>%
  hc_xAxis(categories = data$race) %>%
  hc_yAxis(title = list(text = "Percentage"), max = 100) %>%
  hc_series(
    list(
      name = "Percentage",
      data = lapply(1:nrow(data), function(i) {
        list(
          y = data$prop[i],
          name = data$race[i],
          color = colors[i],
          marker = list(symbol = sprintf("url(data:image/svg+xml;base64,%s)", encode_icon(colors[i])))
        )
      }),
      type = "item",
      size = '100%',
      itemMargin = 10
    )
  ) %>%
  hc_tooltip(
    formatter = JS("function() {
      return '<b>' + this.point.name + ':</b> ' + this.y + '%';
    }")
  )
