library(ggplot2)
library(cowplot)
library(grid)
library(rsvg)

# Set up colors
mclc_dk_blue  <- "#004270"
mclc_lt_blue  <- "#C7E8F5"
empty_color   <- "#FFFFFF"
default_ncols <- 12

# Image setup (with SVG)
whichimage <- "Person_icon_BLACK-01.svg"

# Function to render the SVG as a grob for plotting
read_svg_as_grob <- function(svg_file) {
  if (!file.exists(svg_file)) {
    stop(glue("SVG file not found at path: {svg_file}"))
  }

  # Convert SVG to rasterGrob
  svg <- rsvg::rsvg(svg_file)
  grob <- grid::rasterGrob(svg, interpolate = TRUE)

  return(grob)
}

# Create Plot list of full and partially filled icons
icon_options <- function(partialval, fill = mclc_dk_blue, partial = mclc_lt_blue) {
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Load the SVG as a grob
  grob <- read_svg_as_grob("img/Person_icon_BLACK-01.svg")  # Change this to your path

  # Return a list of full and partial icons
  plot_lst <- list(
    "full" = ggplot() + annotation_custom(grob) + theme_void(),
    "partial" = ggplot() + annotation_custom(grob, xmin = 0.5, xmax = 1) + theme_void()
  )

  return(plot_lst)
}

# Create the icons
create_icons <- function(rri_raw, infogs = default_ncols, infogs_ncol = default_ncols) {
  numfull <- floor(rri_raw)
  numremain <- rri_raw - numfull

  plot_opts <- icon_options(partialval = numremain)

  plot_list <- list()

  # Add full icons
  for (i in 1:numfull) {
    plot_list[[i]] <- plot_opts$full
  }

  # Add partial icon
  if (numremain > 0) {
    plot_list[[numfull + 1]] <- plot_opts$partial
  }

  # Fill the rest with empty icons
  if (length(plot_list) < infogs) {
    for (i in (length(plot_list) + 1):infogs) {
      plot_list[[i]] <- plot_opts$full  # You can customize this for empty icons
    }
  }

  # Create the grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(infogs / infogs_ncol), 1)
  plot_grid(plotlist = plot_list, nrow = rows)
}

# Main function to create the infographic
create_infographic <- function(rri_raw) {
  ggtemp_justpeople <- create_icons(rri_raw)
  print(ggtemp_justpeople)
}

# Call the function to create the infographic
create_infographic(3.5)
