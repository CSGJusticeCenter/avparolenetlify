library(leaflet)
library(sf)
library(htmltools)
library(leaflet.extras)
library(albersusa)
library(dplyr)

# Load state geometries using albersusa
states <- usa_sf("longlat")

# Sample data (replace this with your actual data)
mydata <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California"),
  current_perc = c(10, 20, 30, 40, 50)
)

# Merge the data with the geometries
us_map <- states %>%
  left_join(mydata, by = c("name" = "state"))

# Define the NYT-like color palette
nyt_palette <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026")

pal <- colorNumeric(palette = nyt_palette, domain = us_map$current_perc, na.color = "darkgray")

# Background style
backg <- htmltools::tags$style(".leaflet-container { background: white; }")

# Custom legend
legend_html <- HTML(
  '<div style="background:white; padding: 5px; display: flex; align-items: center; justify-content: center;">
    <div style="margin-right: 10px;">0%</div>
    <div style="background: #ffffcc; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #ffeda0; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #fed976; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #feb24c; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #fd8d3c; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #fc4e2a; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #e31a1c; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #bd0026; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="background: #800026; width: 30px; height: 15px; margin: 0 2px;"></div>
    <div style="margin-left: 10px;">100%</div>
  </div>'
)

# Create the map
leaflet(us_map) %>%
  addPolygons(
    fillColor = ~pal(current_perc),
    fillOpacity = 0.9,
    color = "white",
    weight = 2,
    dashArray = '1',
    opacity = 1,
    popup = ~paste0("<strong>State: </strong>", name),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>%
  addControl(legend_html, position = "bottomleft") %>%
  htmlwidgets::prependContent(backg) %>%
  addResetMapButton() %>%
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;

      function zoomToFeature(e) {
        map.fitBounds(e.target.getBounds());
      }

      map.eachLayer(function(layer) {
        if (layer.feature) {
          layer.on({
            mouseover: zoomToFeature
          });
        }
      });
    }
  ")
