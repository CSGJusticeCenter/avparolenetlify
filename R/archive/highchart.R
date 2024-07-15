# Load necessary package
library(grDevices)

# Define the colors for the gradient
blue <- "#1F449C"
red <- "#F05039"

# Create a gradient function
gradient_function <- colorRampPalette(c(blue, red))

# Generate a gradient with a specified number of colors
gradient_colors <- gradient_function(10) # Generates 10 colors in the gradient

# Display the gradient colors
print(gradient_colors)
"#1F449C" "#364591" "#4D4686" "#64487B" "#7B4970" "#934A65" "#AA4C5A"
[8] "#C14D4F" "#D84E44" "#F05039"
