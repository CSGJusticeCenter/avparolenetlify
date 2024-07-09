# # Load necessary libraries
# library(ggplot2)
#
# # Set the number of surrounding dots
# n <- 500
#
# # Generate data for the surrounding dots
# theta <- runif(n, 0, 2 * pi) # Random angles
# r <- sqrt(runif(n, 0, 1))    # Random radii (to ensure uniform distribution)
#
# # Convert polar coordinates to Cartesian coordinates
# x <- r * cos(theta)
# y <- r * sin(theta)
#
# # Create a data frame with the coordinates
# data <- data.frame(x = c(0, x), y = c(0, y), size = c(1.5, rep(1, n)),
#                    color = c("red", rep("darkgray", n)),
#                    alpha = c(1, rep(0.5, n)))
#
# # Plot the graphic
# ggplot(data, aes(x, y, color = color, size = size, alpha = alpha)) +
#   geom_point() +
#   scale_color_identity() +  # Use the color column directly
#   scale_size_identity() +   # Use the size column directly
#   scale_alpha_identity() +  # Use the alpha column directly
#   theme_void() +  # Remove axis and background
#   theme(aspect.ratio = 1) +  # Ensure the plot is square
#   coord_fixed() + # Ensure the aspect ratio is fixed
#   ggtitle("1 parole board member\nper XXX people in prison\neligible for parole") +
#   theme(
#     plot.title = element_text(hjust = 0.5, vjust = 5, face = "bold", size = 16),
#     plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#   )



library(ggtext)

# Set the number of surrounding dots
n <- 5000

# Calculate the number of rows and columns for the grid
grid_size <- ceiling(sqrt(n))

# Generate data for the surrounding dots using a grid pattern
x <- rep(seq(-grid_size, grid_size, length.out = grid_size), grid_size)
y <- rep(seq(-grid_size, grid_size, length.out = grid_size), each = grid_size)

# Select the first n points
x <- x[1:n]
y <- y[1:n]

# Normalize the coordinates to fit within a unit circle
max_r <- max(sqrt(x^2 + y^2))
x <- x / max_r
y <- y / max_r

# Create a data frame with the coordinates
data <- data.frame(x = c(0, x), y = c(0, y), size = c(2, rep(1, n)),
                   color = c(colors$red, rep(colors$darkgray, n)),
                   alpha = c(1, rep(0.5, n)))

# Adjust the alpha column to apply transparency only to gray circles
data$alpha[data$color == colors$red] <- 1

# Plot the graphic
square_dot_parole_graphic <- ggplot(data, aes(x, y, color = color, size = size, alpha = alpha)) +
  geom_point() +
  scale_color_identity() +  # Use the color column directly
  scale_size_identity() +   # Use the size column directly
  scale_alpha_identity() +  # Use the alpha column directly
  theme_void() +  # Remove axis and background
  theme(aspect.ratio = 1) +  # Ensure the plot is square
  coord_fixed() + # Ensure the aspect ratio is fixed
  labs(title = "<span style='color:#F05039;'><b>1 parole board member</span></b><br>per 5,000 people in prison<br>eligible for parole") +
  theme(
    # plot.title = element_markdown(hjust = 0.5, vjust = -20, size = 16),
    plot.title = element_markdown(hjust = 0.5, margin = margin(b = 10), size = 16),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )
square_dot_parole_graphic

# Save the combined map
ggsave(filename =  "square_dot_parole_graphic.png", plot = square_dot_parole_graphic,
       width  = 5, height = 5, dpi = 300)


# # SQUARE
# # Load necessary libraries
# library(ggplot2)
#
# # Set the number of surrounding dots
# n <- 500
#
# # Calculate the number of rows and columns for the grid
# grid_size <- ceiling(sqrt(n))
#
# # Generate data for the surrounding dots using a grid pattern
# x <- rep(seq(-grid_size, grid_size, length.out = grid_size), grid_size)
# y <- rep(seq(-grid_size, grid_size, length.out = grid_size), each = grid_size)
#
# # Select the first n points
# x <- x[1:n]
# y <- y[1:n]
#
# # Normalize the coordinates to fit within a unit circle
# max_r <- max(sqrt(x^2 + y^2))
# x <- x / max_r
# y <- y / max_r
#
# # Create a data frame with the coordinates
# data <- data.frame(x = c(0, x), y = c(0, y), size = c(1.5, rep(1, n)),
#                    color = c("red", rep("darkgray", n)),
#                    alpha = c(1, rep(0.5, n)))
#
# # Plot the graphic
# ggplot(data, aes(x, y, color = color, size = size, alpha = alpha)) +
#   geom_point() +
#   scale_color_identity() +  # Use the color column directly
#   scale_size_identity() +   # Use the size column directly
#   scale_alpha_identity() +  # Use the alpha column directly
#   theme_void() +  # Remove axis and background
#   theme(aspect.ratio = 1) +  # Ensure the plot is square
#   coord_fixed() + # Ensure the aspect ratio is fixed
#   ggtitle("1 parole board member\nper XXX people in prison\neligible for parole") +
#   theme(
#     plot.title = element_text(hjust = 0.5, vjust = 5, face = "bold", size = 16),
#     plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#   )






# # Spiral
# # Load necessary libraries
# library(ggplot2)
#
# # Set the number of surrounding dots
# n <- 500
#
# # Generate data for the surrounding dots using a spiral pattern
# t <- seq(0, 10*pi, length.out = n)  # Parameter for spiral
# r <- sqrt(t)  # Increasing radius
# x <- r * cos(t)
# y <- r * sin(t)
#
# # Create a data frame with the coordinates
# data <- data.frame(x = c(0, x), y = c(0, y), size = c(1.5, rep(1, n)),
#                    color = c("red", rep("darkgray", n)),
#                    alpha = c(1, rep(0.5, n)))
#
# # Plot the graphic
# ggplot(data, aes(x, y, color = color, size = size, alpha = alpha)) +
#   geom_point() +
#   scale_color_identity() +  # Use the color column directly
#   scale_size_identity() +   # Use the size column directly
#   scale_alpha_identity() +  # Use the alpha column directly
#   theme_void() +  # Remove axis and background
#   theme(aspect.ratio = 1) +  # Ensure the plot is square
#   coord_fixed() + # Ensure the aspect ratio is fixed
#   ggtitle("1 parole board member\nper XXX people in prison\neligible for parole") +
#   theme(
#     plot.title = element_text(hjust = 0.5, vjust = 5, face = "bold", size = 16),
#     plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#   )




# # Load necessary libraries
# library(ggplot2)
#
# # Set the number of surrounding dots
# n <- 10000
#
# # Generate data for the surrounding dots
# theta <- seq(0, 2 * pi, length.out = n) # Evenly spaced angles
# r <- sqrt(seq(0, 1, length.out = n))    # Evenly spaced radii
#
# # Convert polar coordinates to Cartesian coordinates
# x <- r * cos(theta)
# y <- r * sin(theta)
#
# # Create a data frame with the coordinates
# data <- data.frame(x = c(0, x), y = c(0, y),
#                    size = c(3, rep(1, n)),
#                    color = c("red", rep("gray", n)))
#
# # Plot the graphic
# ggplot(data, aes(x, y, color = color, size = size)) +
#   geom_point(alpha = 0.7) +
#   scale_color_identity() +  # Use the color column directly
#   scale_size_identity() +  # Use the size column directly
#   theme_void() +  # Remove axis and background
#   theme(aspect.ratio = 1) +  # Ensure the plot is square
#   coord_fixed()  # Ensure the aspect ratio is fixed
