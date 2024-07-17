
# Load the packages
library(plotly)
library(dplyr)
library(RColorBrewer)

# Function to create waffle chart data
create_waffle_data <- function(categories, values, rows = 10) {
  total_values <- sum(values)
  num_cols <- ceiling(total_values / rows)
  waffle_data <- expand.grid(x = 1:num_cols, y = 1:rows)
  waffle_data <- waffle_data[1:total_values,]
  waffle_data$value <- rep(categories, values)
  return(waffle_data)
}

# Sample data
categories <- c("Category A", "Category B", "Category C")
values <- c(50, 30, 20)

# Create waffle chart data
waffle_data <- create_waffle_data(categories, values)

# Plot the interactive waffle chart using plotly
interactive_waffle <- plot_ly(
  data = waffle_data,
  x = ~x,
  y = ~y,
  text = ~value,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    symbol = 'square',
    size = 20,
    color = ~factor(value),
    colors = RColorBrewer::brewer.pal(length(categories), "Set3")
  )
) %>%
  layout(
    title = "Interactive Waffle Chart",
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    hovermode = "closest"
  )

# Display the interactive waffle chart
interactive_waffle
