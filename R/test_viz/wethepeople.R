# Example data
data <- c(`Category 1` = 30, `Category 2` = 20)

# Convert data to a waffle chart
waffle_chart <- waffle(data)

# Convert to a data frame for ggplot
waffle_df <- as.data.frame(waffle_chart$data)

# Create the waffle chart using ggplot2 with circles
p1 <- ggplot(waffle_df, aes(x = x, y = y, fill = value)) +
  geom_point(shape = 21, size = 5, color = "black") + # using circles
  scale_fill_manual(values = c("Category 1" = "red", "Category 2" = "blue", "Category 3" = "green")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  labs(fill = "Categories", title = "Waffle Chart with Circles")
p1

# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("showtext")) install.packages("showtext")

# Load the required libraries
library(ggplot2)
library(showtext)

# Add the Wee People font
font_add("weepeople", "fonts/weepeople.ttf")
showtext_auto()

# Sample data
data <- data.frame(
  category = c("Category A", "Category B", "Category C"),
  value = c(5, 10, 7)
)

# Create the plot
p <- ggplot(data, aes(x=category, y=value)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label=rep("\uE000", value)), family="weepeople", size=10, vjust=-0.2) +  # Use the Wee People font
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

# Print the plot
print(p)
