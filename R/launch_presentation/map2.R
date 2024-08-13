library(leaflet)
library(tigris)
library(dplyr)
library(sf)
library(tidycensus)

# Load West Virginia counties shapefile
wv_counties <- counties(state = "WV", year = 2023, class = "sf")

# Transform the CRS to WGS84
wv_counties <- st_transform(wv_counties, crs = 4326)

# Get population data from tidycensus
population_data <- get_acs(
  geography = "county",
  state = "WV",
  variables = "B01001_001", # Total population
  year = 2021,
  survey = "acs5"
)

# Clean and rename the columns for joining
population_data <- population_data |>
  select(GEOID, estimate) |>
  rename(adult_population = estimate)

# Join population data with the shapefile
wv_counties <- wv_counties |>
  left_join(population_data, by = c("GEOID" = "GEOID"))

# Calculate the centroid of West Virginia for initial map view
wv_centroid <- st_centroid(st_union(wv_counties))

# Get coordinates for the centroid
centroid_coords <- st_coordinates(wv_centroid)

# Define colors
darkblue <- "#263C4B"
blue <- "#3F95B0"
lightblue <- "#E0F7FA"

# Create a color palette function
pal <- colorNumeric(
  palette = c(lightblue, blue, darkblue),
  domain = wv_counties$adult_population
)

# Plot the map with leaflet
leaflet_wv_map <- leaflet(wv_counties, options = leafletOptions(minZoom = 7.5, maxZoom = 7.5, dragging = FALSE)) |>
  addPolygons(color = "#444444", weight = 1, opacity = 1, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              fillColor = "white",
              label = ~NAME) |>
  addPolygons(
    group = "Adult Population",
    color = "#444444", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    label = ~lapply(
      paste0("<b>", NAME, "</b><br/>Adult Population: ", comma(adult_population)),
      htmltools::HTML
    ),
    fillColor = ~pal(adult_population),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~adult_population,
    title = "Adult Population",
    position = "bottomright",
    group = "Adult Population"
  ) %>%
  addLayersControl(
    overlayGroups = c("Adult Population"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 7.5)

# Add CSS to set the background color to white
leaflet_wv_map <- leaflet_wv_map %>% htmlwidgets::onRender("
  function(el, x) {
    document.querySelector('.leaflet-container').style.background = 'white';
  }
")

# Hide the legend when the layer is not checked
leaflet_wv_map <- leaflet_wv_map %>% htmlwidgets::onRender("
  function(el, x) {
    var legend = document.querySelector('.leaflet-control-container .legend');
    var checkboxes = document.querySelectorAll('.leaflet-control-layers-selector');

    function updateLegend() {
      var anyChecked = Array.prototype.slice.call(checkboxes).some(function(checkbox) {
        return checkbox.checked && checkbox.nextElementSibling.innerHTML.includes('Adult Population');
      });
      if (anyChecked) {
        legend.style.display = 'block';
      } else {
        legend.style.display = 'none';
      }
    }

    checkboxes.forEach(function(checkbox) {
      checkbox.addEventListener('change', updateLegend);
    });

    updateLegend();
  }
")

leaflet_wv_map
