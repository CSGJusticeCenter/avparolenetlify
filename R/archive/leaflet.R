
library(leaflet)
library(sf)
library(htmltools)
library(leaflet.extras)
library(albersusa)

# Load state geometries using albersusa
states <- usa_sf("longlat")

# Sample data (replace this with your actual data)
mydata <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California"),
  current_perc = c(10, 20, 30, 40, 50)
)

# Merge the data with the geometries
us_map <- states |>
  left_join(mydata, by = c("name" = "state"))

# Define color palette
pal <- colorNumeric(palette = c(colors$green1, colors$green2,
                                colors$green3, colors$green4), domain = us_map$current_perc, na.color = "darkgray")

# Background style
backg <- htmltools::tags$style(".leaflet-container { background: white; }")

# Create the map
leaflet(us_map) |>
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
  ) |>
  addLegend(
    pal = pal,
    values = ~current_perc,
    title = "Current Percentage",
    position = "bottomright"
  ) |>
  htmlwidgets::prependContent(backg) |>
  addResetMapButton() |>
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
