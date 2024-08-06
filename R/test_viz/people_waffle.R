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



library(highcharter)
library(dplyr)
library(purrr)
library(jsonlite)

# Define the function to encode the SVG icon
encode_icon <- function(color) {
  iconSVG <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24'>
      <g transform='translate(1 1)' fill='%s' fill-rule='evenodd'>
        <rect width='20' height='20' x='2' y='2' fill='%s' stroke='#FFFFFF' stroke-width='1'/>
        <line x1='2' y1='2' x2='22' y2='2' stroke='#FFFFFF' stroke-width='1'/>
        <line x1='2' y1='22' x2='22' y2='22' stroke='#FFFFFF' stroke-width='1'/>
        <line x1='7' y1='2' x2='7' y2='22' stroke='#FFFFFF' stroke-width='1'/>
        <line x1='12' y1='2' x2='12' y2='22' stroke='#FFFFFF' stroke-width='1'/>
        <line x1='17' y1='2' x2='17' y2='22' stroke='#FFFFFF' stroke-width='1'/>
      </g>
    </svg>",
    color, color
  )
  base64encode(charToRaw(iconSVG))
}

# # Sample function to prepare the parole eligibility data by race
# fnc_prepare_pe_data <- function(ncrp_yearendpop, race) {
#   # Placeholder function to simulate data preparation
#   tibble(
#     state = rep(c("Georgia", "California"), each = 4),
#     race = rep(c("White", "Black", "Hispanic", "Other"), 2),
#     prop = runif(8, 0, 1),
#     n_label = sample(1:100, 8)
#   )
# }

# Placeholder variables
ncrp_yearendpop <- NULL
race <- NULL
select_year <- 2024

# Prepare the parole eligibility data by race
current_ped_race <- fnc_prepare_pe_data(ncrp_yearendpop, race) |>
  mutate(prop_label = paste0("<b>", prop * 100, "</b> (", n_label, ")"),
         prop = prop * 100)

# Get unique states
states <- unique(current_ped_race$state)

# Define colors for the groups
colors_list <- c(color1, color2, colo3)

# Create Highcharts visualizations for each state
all_waffle_parole_elgibility_race <- map(.x = states, .f = function(x) {

  data <- current_ped_race |>
    filter(state == x) |>
    arrange(desc(n_label)) |>
    mutate(prop = round(prop, 0))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  race and ethnicity in ", select_year, " in the state of ", x, ".")

  highcharts <- highchart() |>
    hc_chart(type = "item",
             marginTop = 140
    ) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_xAxis(categories = data$race) |>
    hc_yAxis(title = list(text = "Percentage"), max = 100) |>
    hc_series(
      list(
        name = "Percentage",
        data = lapply(1:nrow(data), function(i) {
          list(
            y = data$prop[i],
            name = data$race[i],
            color = colors_list[i],
            marker = list(symbol = sprintf("url(data:image/svg+xml;base64,%s)", encode_icon(colors_list[i])))
          )
        }),
        type = "item",
        size = '100%',
        itemMargin = 10,
        rows = 10
      )
    ) |>
    hc_tooltip(
      formatter = JS("function() {
        return '<b>' + this.point.name + ':</b> ' + this.y + '%';
      }")
    ) |>
    hc_add_theme(hc_theme_google())

  return(highcharts)
})

# Name the list of charts by state
all_waffle_parole_elgibility_race <- setNames(all_waffle_parole_elgibility_race, states)

# Display the chart for Georgia as an example
all_waffle_parole_elgibility_race$Georgia
