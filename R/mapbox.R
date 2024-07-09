
mydata <- map_data |> select(state, current_perc)

# Install and load the necessary packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("hrbrmstr/albersusa")
library(albersusa)
library(mapdeck)
library(dplyr)

# Load state boundaries with Alaska and Hawaii repositioned
us_states <- usa_sf()

# Merge your data with the US states geometry data
us_states$state_name <- us_states$name
df_merged <- merge(us_states, mydata, by.x = "name", by.y = "state", all.x = TRUE)

# Set your Mapbox token
mapdeck::set_token('pk.eyJ1IjoibWFyaWdhdG8iLCJhIjoiY2x5NG53OGQ3MDI1MzJsb2hudHRmaXA5diJ9.hTNPE8VG3gEMnvYujUnI3w')


# Create the map with a white background
map <- mapdeck(style = 'mapbox://styles/mapbox/light-v10') %>%
  add_polygon(
    data = df_merged,
    fill_colour = "current_perc",
    layer_id = "choropleth",
    palette = "viridis",
    tooltip = "name"
  )

map



#------ Mapbox ------#

# Install and load the necessary packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("hrbrmstr/albersusa")
library(albersusa)
library(mapdeck)
library(dplyr)

# Load state boundaries with Alaska and Hawaii repositioned
us_states <- usa_sf()

df <- data.frame(
  state = c("NY", "CA", "TX", "FL", "IL", "AK", "HI"),
  value = c(10, 20, 30, 40, 50, 10, 10)
)

# Create a mapping between state abbreviations and full names
state_abbrev <- data.frame(
  state_abbr = state.abb,
  state_name = state.name
)

# Merge your data with the state abbreviation mapping
df <- df %>%
  left_join(state_abbrev, by = c("state" = "state_abbr"))

# Merge with the US states geometry data
us_states$state_name <- us_states$name
df_merged <- merge(us_states, df, by.x = "name", by.y = "state_name")

# Set your Mapbox token
mapdeck::set_token('pk.eyJ1IjoibWFyaWdhdG8iLCJhIjoiY2x5NG53OGQ3MDI1MzJsb2hudHRmaXA5diJ9.hTNPE8VG3gEMnvYujUnI3w')

# Create the map
map <- mapdeck(style = mapdeck_style("light")) %>%
  add_polygon(
    data = df_merged,
    fill_colour = "value",
    layer_id = "choropleth",
    palette = "viridis",
    tooltip = "name"
  )

map
