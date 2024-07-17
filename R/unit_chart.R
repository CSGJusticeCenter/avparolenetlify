# Load necessary libraries
library(plotly)

# Create a sample dataset
set.seed(123)  # for reproducibility
data <- data.frame(
  id = 1:100,
  race = rep(c("White", "Black", "Asian", "Hispanic", "Other"), times = c(50, 20, 10, 15, 5))
)

# Convert the race to a factor to ensure it is treated as categorical
data$race <- factor(data$race, levels = c("White", "Black", "Asian", "Hispanic", "Other"))

# Create a column to represent the x-position based on the number of people
data$x_pos <- ave(data$id, data$race, FUN = seq_along)

# Calculate the number and proportion of people for each race
race_summary <- aggregate(id ~ race, data = data, FUN = length)
race_summary$proportion <- race_summary$id / sum(race_summary$id)
race_summary$label <- paste(race_summary$id, " (", scales::percent(race_summary$proportion), ")", sep = "")

# Calculate the maximum x_pos for each race to position the labels
max_x_pos <- aggregate(x_pos ~ race, data = data, FUN = max)
race_summary <- merge(race_summary, max_x_pos, by = "race")
race_summary$x_label_pos <- race_summary$x_pos + 2  # Add a constant to shift labels

# Create an interactive unit graph using plotly
p <- plot_ly(
  data = data,
  x = ~x_pos,
  y = ~race,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste("Race:", race),
  hoverinfo = 'text'
)

# Add data labels at the end of dots
p <- p %>% add_annotations(
  x = race_summary$x_label_pos +10,  # Use calculated x positions for labels
  y = race_summary$race,
  text = race_summary$label,
  xref = 'x',
  yref = 'y',
  showarrow = FALSE,
  font = list(family = 'Franklin Gothic Book', size = 12, color = '#000')
)

# Customize the layout
p <- p %>% layout(
  title = list(text = 'Race and Ethnicity', font = list(family = 'Franklin Gothic Book', size = 16)),
  xaxis = list(title = 'Number of People', showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE, titlefont = list(family = 'Franklin Gothic Book', size = 14)),
  yaxis = list(title = '', zeroline = FALSE, showgrid = FALSE, tickfont = list(family = 'Franklin Gothic Book', size = 12)),
  plot_bgcolor = "white",
  paper_bgcolor = "white",
  margin = list(l = 50, r = 50, b = 50, t = 50)  # Adjust margins to reduce space
)
p









# Load necessary libraries
library(plotly)

# Create a sample dataset
set.seed(123)  # for reproducibility
data <- data.frame(
  id = 1:100,
  race = rep(c("White", "Black", "Asian", "Hispanic", "Other"), times = c(50, 20, 10, 15, 5))
)

# Convert the race to a factor to ensure it is treated as categorical
data$race <- factor(data$race, levels = c("White", "Black", "Asian", "Hispanic", "Other"))

# Create a column to represent the x-position based on the number of people
data$x_pos <- ave(data$id, data$race, FUN = seq_along)

# Calculate the number and proportion of people for each race
race_summary <- aggregate(id ~ race, data = data, FUN = length)
race_summary$proportion <- race_summary$id / sum(race_summary$id)
race_summary$label <- paste(race_summary$id, " (", scales::percent(race_summary$proportion), ")", sep = "")

# Calculate the maximum x_pos for each race to position the labels
max_x_pos <- aggregate(x_pos ~ race, data = data, FUN = max)
race_summary <- merge(race_summary, max_x_pos, by = "race")
uniform_label_shift <- 15  # Define a uniform shift for the labels
race_summary$x_label_pos <- race_summary$x_pos + uniform_label_shift  # Apply the uniform shift

# Create an interactive unit graph using plotly
p <- plot_ly(
  data = data,
  x = ~x_pos,
  y = ~race,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10, line = list(color = 'white', width = 2)),
  text = ~paste("Race:", race),
  hoverinfo = 'text'
)

# Add data labels at the end of dots
p <- p %>% add_annotations(
  x = race_summary$x_label_pos,  # Use calculated x positions for labels
  y = race_summary$race,
  text = race_summary$label,
  xref = 'x',
  yref = 'y',
  showarrow = FALSE,
  font = list(family = 'Franklin Gothic Book', size = 12, color = '#000')
)

# Customize the layout
p <- p %>% layout(
  title = list(text = 'Race and Ethnicity', font = list(family = 'Franklin Gothic Book', size = 16)),
  xaxis = list(title = 'Number of People', range = c(0, max(race_summary$x_label_pos) + uniform_label_shift), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE, titlefont = list(family = 'Franklin Gothic Book', size = 14)),
  yaxis = list(title = '', zeroline = FALSE, showgrid = FALSE, tickfont = list(family = 'Franklin Gothic Book', size = 12)),
  plot_bgcolor = "white",
  paper_bgcolor = "white",
  margin = list(l = 50, r = 50, b = 50, t = 50)  # Adjust margins to reduce space
)

# Display the plot
p
