#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################

hex_gj <- read_sf(file.path(sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(3857) |>
  sf_geojson() |>
  fromJSON(simplifyVector = FALSE)


#-----Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop_consolidated |>
  group_by(state, rptyear) |>
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment with sentences 1+ years but not life
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- fnc_filter_pe_population_criteria(ncrp_yearendpop_consolidated)

# Get total prison population for new court commitments and with sentences 1+ years but not life
filtered_pop_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(filtered_total_pop = n(), .groups = 'drop')

# Get number of people in prison by parole eligibility status for the specified criteria
# Get proportion of parole eligibility statuses out of everyone in the filtered population
filtered_parole_status_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear, parelig_status) |>
  summarise(count = n(), .groups = 'drop') |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  mutate(proportion = count / filtered_total_pop)

# Reshape data for table
filtered_parole_elig_table_by_year <- filtered_parole_status_by_year |>
  pivot_longer(cols = c(count, proportion), names_to = "metric", values_to = "value") |>
  mutate(metric_name = case_when(
    metric == "count" ~ paste(parelig_status, "count"),
    metric == "proportion" ~ paste(parelig_status, "perc.")
  )) |>
  select(state, rptyear, metric_name, value) |>
  pivot_wider(names_from = metric_name, values_from = value) |>
  clean_names()

# Filter to select analysis year specified in the config file
filtered_parole_elig_table_analysis_year_with_missing_states <- filtered_parole_elig_table_by_year |>
  filter(rptyear == select_year)

# Find missing states and combine with the original dataframe
missing_states <- tibble(state = setdiff(state.name, filtered_parole_elig_table_analysis_year_with_missing_states$state),
                         rptyear = select_year)

# Add missing states to table so we have a complete table of 50 states
filtered_parole_elig_table_analysis_year <- filtered_parole_elig_table_analysis_year_with_missing_states |>
  bind_rows(missing_states) |>
  full_join(total_pop_by_year, by = c("state", "rptyear")) |>
  full_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  arrange(state) |>
  select(state, rptyear, total_pop, filtered_total_pop,
         contains("current"), contains("future"), contains("missing")) |>
  filter(rptyear == select_year) |>
  mutate(current_perc           = current_perc * 100,
         # future_perc            = future_perc * 100,
         # missing_perc           = missing_perc * 100,
         current_count_rounded = fnc_round_to_power(current_count))



#-----Parole Board Members by State ------#

# Get parole status information by state
# Get number of parole board members
states_parole <- state_notes |>
  select(state, abolished_parole, members)



#-----Parole Eligibility Table ------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- filtered_parole_elig_table_analysis_year |>
  left_join(states_parole, by = "state") |>
  mutate(current_perc = round(current_perc, 1)) |>
  select(state, current_perc, current_count_rounded, filtered_total_pop, members, abolished_parole)





# Load required libraries
library(leaflet)
library(sf)
library(geojsonio)
library(dplyr)

# Load the hex grid GeoJSON
us_hex <- read_sf(file.path(sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(4326)  # Transform to WGS84 (EPSG:4326)

parole_eligibility_table1 <- parole_eligibility_table |>
  mutate(state_abb = state.abb[match(state, state.name)]) |>
  mutate(current_perc = current_perc) |>
  select(state, state_abb, current_perc, abolished_parole)

us_hex <- us_hex |>
  left_join(parole_eligibility_table1,
            by = "state_abb")

us_hex_abolished <- us_hex |>
  filter(abolished_parole == "Y")

# Define the color palette
pal_pe_pct <- colorNumeric(
  palette = c(green1, green2, green3, green4),
  domain = us_hex$current_perc,
  na.color = darkgray
)

# https://github.com/rstudio/leaflet/issues/615
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

# Create a Leaflet map
leaflet(us_hex) |>
  addTiles(options = tileOptions(opacity = 0)) |>
  addPolygons(data = us_hex,
              fillColor = "white",
              color = "white",
              weight = 1,
              opacity = 0.7,
              fillOpacity = 0.5,
              popup = ~state_abb) |>
  addPolygons(
    group = "Past Parole Eligibility",
    color = "white", weight = 1, opacity = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
    label = ~lapply(
      paste0("<b>", state_abb, "</b><br/>Past Parole Eligibility: ", round(current_perc, 0), "%"),
      htmltools::HTML
    ),
    fillColor = ~pal_pe_pct(current_perc),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  ) |>
  addPolygons(group = "Abolished Parole White Hex",
              data = us_hex_abolished,
              fillColor = "white",
              color = "white",
              weight = 1,
              opacity = 0.7,
              fillOpacity = 1,
              popup = ~state_abb) |>
  addPolygons(group = "Abolished Parole",
              data = us_hex_abolished,
              fillColor = yellow,
              color = "white",
              weight = 1,
              opacity = 0.7,
              fillOpacity = 1,
              popup = ~state_abb) |>
  addLegend(
    pal = pal_pe_pct,
    values = ~current_perc,
    title = "Percentage of Prison Population<br>Past Parole Eligibility",
    position = "bottomright",
    group = "Past Parole Eligibility",
    className = "legend leaflet-control",
    labFormat = labelFormat(
      suffix = "%",  # Add '%' to each label
      between = " - "  # Optional, customize as needed
    ),
    na.label = "Missing Data"  # Change 'NA' to 'Missing Data'
  ) |>
  addLayersControl(
    overlayGroups = c("Abolished Parole"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  htmlwidgets::onRender("
    function(el) {
      // Set a white background for the map container
      document.querySelector('.leaflet-container').style.background = 'white';

      // Uncheck the 'Abolished Parole' layer by default
      var control = el.layersControl;
      if (control) {
        var abolishedLayer = control.getLayers().find(function(layer) {
          return layer.name === 'Abolished Parole';
        });
        if (abolishedLayer) {
          abolishedLayer.checkbox.checked = false;
        }
      }
    }
  ")

