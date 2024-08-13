####################
# Author: Mari Roberts
# Date Last Updated: 2024-07-29
# File Name: maps.R
# File Description: This script loads and processes data for West Virginia counties,
#                   including population, poverty, and drug overdose rates, and visualizes
#                   the data using a Leaflet map.
####################

# Rename variable for drug overdose data
drug_overdose_data <- county_overdoses |>
  mutate(drug_overdose = county_overdoses_per_100_000)

# Merge jail capacity data with coordinates
jail_data <- merge(jail_coordinates, jail_capacity, by = "name", all.x = TRUE, all.y = TRUE)

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

# Get poverty data from tidycensus
poverty_data <- get_acs(
  geography = "county",
  state = "WV",
  variables = "B17001_002", # Population below poverty level
  year = 2021,
  survey = "acs5"
)

# Clean and rename the columns for joining
population_data <- population_data |>
  select(GEOID, estimate) |>
  rename(adult_population = estimate)

poverty_data <- poverty_data |>
  select(GEOID, estimate) |>
  rename(below_poverty = estimate)

# Join population and poverty data with the shapefile
wv_counties <- wv_counties |>
  left_join(population_data, by = c("GEOID" = "GEOID")) |>
  left_join(poverty_data, by = c("GEOID" = "GEOID")) |>
  left_join(drug_overdose_data, by = c("GEOID" = "GEOID"))

# Calculate the percentage of the population below the poverty level
wv_counties <- wv_counties |>
  mutate(poverty_rate = (below_poverty / adult_population) * 100)

# Calculate the centroid of West Virginia for initial map view
wv_centroid <- st_centroid(st_union(wv_counties))

# Get coordinates for the centroid
centroid_coords <- st_coordinates(wv_centroid)

# Create color palette functions for population, poverty, and drug overdose rates
pal_population <- colorNumeric(
  palette = c("lightgreen", "green", "darkgreen"),
  domain = wv_counties$adult_population
)

pal_poverty <- colorNumeric(
  palette = c("white", "lightblue", "blue", "darkblue"),
  domain = wv_counties$poverty_rate
)

pal_drug <- colorNumeric(
  palette = c("white", "lightred", "red", "darkred"),
  domain = wv_counties$drug_overdose
)

# Plot the map with leaflet
leaflet_wv_map <- leaflet(wv_counties, options = leafletOptions(minZoom = 7.5, maxZoom = 7.5, dragging = FALSE),
                          height = "700px") |>
  addPolygons(color = "#444444", weight = 1, opacity = 1, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              fillColor = "white",
              label = ~NAME) |>
  addPolygons(
    group = "County Lines",
    color = "#444444", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    fillColor = lightgreen#"#e7e6e6"
  ) |>
  addPolygons(
    group = "Poverty Rate",
    color = "#444444", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    label = ~lapply(
      paste0("<b>", NAME, "</b><br/>Poverty Rate: ", round(poverty_rate, 2), "%<br/>Adults Below Poverty: ", comma(below_poverty)),
      htmltools::HTML
    ),
    fillColor = ~pal_poverty(poverty_rate),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  ) |>
  addPolygons(
    group = "Adult Population",
    color = "#444444", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    label = ~lapply(
      paste0("<b>", NAME, "</b><br/>Adult Population: ", comma(adult_population)),
      htmltools::HTML
    ),
    fillColor = ~pal_population(adult_population),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  ) |>
  addPolygons(
    group = "Drug Overdose",
    color = "#444444", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    label = ~lapply(
      paste0("<b>", NAME, "</b><br/>Drug Overdose (per 100,000): ", comma(drug_overdose)),
      htmltools::HTML
    ),
    fillColor = ~pal_drug(drug_overdose),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  ) |>

  addLegend(
    pal = pal_population,
    values = ~adult_population,
    title = "Adult Population",
    position = "bottomright",
    group = "Adult Population",
    className = "legend legend-population"
  )|>
  addLegend(
    pal = pal_poverty,
    values = ~poverty_rate,
    title = "Poverty Rate (%)",
    position = "bottomright",
    group = "Poverty Rate",
    className = "legend legend-poverty"
  ) |>
  addLegend(
    pal = pal_drug,
    values = ~drug_overdose,
    title = "Drug Overdose (per 100,000)",
    position = "bottomright",
    group = "Drug Overdose",
    className = "legend legend-drugs"
  ) |>

  addMarkers(
    data = jail_data,
    lat = ~lat,
    lng = ~lng,
    icon = icons(
      iconUrl = "img/jail_icon.png",
      iconWidth = 15, iconHeight = 15
    ),
    label = ~paste0(
      "<b>", name, "</b><br/>Address: ", address,
      "<br/>2023 Population: ", ifelse(is.na(population), "N/A", population),
      "<br/>Capacity: ", ifelse(is.na(capacity), "N/A", capacity),
      "<br/>Over Capacity: ", ifelse(is.na(overcapacity), "N/A", overcapacity)
    ) |>
      lapply(htmltools::HTML),
    group = "Regional Jails",
    options = markerOptions(riseOnHover = TRUE)
  ) |>

  addLayersControl(
    baseGroups = c("Adult Population", "Poverty Rate", "Drug Overdose", "County Lines"),
    overlayGroups = c("Regional Jails"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = 7.5)

# Add CSS to set the background color to white
leaflet_wv_map <- leaflet_wv_map |> htmlwidgets::onRender("
  function(el, x) {
    document.querySelector('.leaflet-container').style.background = 'white';
  }") |>
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('Facilities');
            $('.leaflet-control-layers-list').prepend('Map Layers');
        }
    ") |>
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          if (selectedGroup == 'Poverty Rate') {
            document.querySelector('.legend-poverty').hidden=false;
          } else if (selectedGroup == 'Adult Population') {
            document.querySelector('.legend-population').hidden=false;
          } else if (selectedGroup == 'Drug Overdose') {
            document.querySelector('.legend-drugs').hidden=false;
          }
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")

leaflet_wv_map





#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){
  save(leaflet_wv_map, file = file.path(folder, "leaflet_wv_map.rds"))
}

