#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

#' @title Blank Out Plot Theme
#' @description This function sets up a theme for blanking out plot elements like axes, scales, and legends.
#' @return A list of ggplot2 theme and scale elements for use in plots.
#' @export
fnc_blankitout_homepage <- function(){
  list(
    theme_void(),  # Removes background and gridlines for a clean appearance.
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
    theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
  )
}

#' @title Icon Options for Plotting
#' @description Generates a list of plot options based on a partial fill value, colors, and a background for creating icon-based infographics.
#' @param partialval A numeric value between 0 and 1 representing the percentage of the partial icon fill.
#' @param empty Color for the empty part of the icon.
#' @param fill Color for the full part of the icon.
#' @param partial Color for the partially filled icon.
#' @param bg Background color for the icons.
#' @param fillHoriz A logical flag indicating whether to fill icons horizontally (TRUE) or vertically (FALSE).
#' @return A list of ggplot objects representing empty, full, and partial icon states.
#' @export
fnc_icon_options_homepage <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
  # Ensure partialval is within valid range
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  # Define color sets for different states of the icon (empty, full, partial)
  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )

  # Define percentage fills for each icon state
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )

  # Initialize the plot list to store generated plots for each state
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  # Determine the boundaries for filling either horizontally or vertically
  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
    max <- max(pos1)
  }
  h <- dim(img)[1]  # Icon height
  w <- dim(img)[2]  # Icon width
  min <- min(pos1)

  # Loop through each icon state and generate corresponding plot
  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
    pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
    finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
    bkgr <- (finalimg == 1)  # Background mask
    colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix

    # Apply the fill either horizontally or vertically
    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    # Assign partially filled cells in the image
    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting

    # Remove partial fill for the 'full' state
    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    # Create the ggplot for each icon state
    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
      fnc_blankitout_homepage()  # Apply the blank theme

    plot_lst[[j]] <- plot  # Store the plot in the list
  }

  return(plot_lst)  # Return the list of generated plots
}

#' @title Create Icon Infographic
#' @description Generates an infographic representing a Relative Rate Index (RRI) using icons.
#' @param rri_raw The raw Relative Rate Index value.
#' @param rri_digits Number of decimal places to round the RRI value.
#' @param fillcolor Color to fill the full icons.
#' @param partialcolor Color to fill partially filled icons.
#' @param emptyhumans Logical flag to indicate whether empty icons should be included.
#' @param emptycolor Color for the empty icons.
#' @param infogs Total number of icons to display.
#' @param infogs_ncol Number of columns for the grid layout of icons.
#' @param fillHoriz Logical flag to determine if icons should be filled horizontally or vertically.
#' @return A ggplot2 object representing the generated icon infographic.
#' @export
fnc_create_icons_homepage <- function(rri_raw, rri_digits = 1, fillcolor = "darkgray", partialcolor = "white",
                                      emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                                      infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value and compute full and partial icons
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partial icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options_homepage(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()  # Initialize list for storing plots

  # Set the first icon in green
  first_icon_color <- color4
  first_icon_opts <- fnc_icon_options_homepage(partialval = 0, empty = emptycolor, fill = first_icon_color, partial = first_icon_color, fillHoriz = fillHoriz)
  plot_list[[1]] <- first_icon_opts$full

  # Create full icons in gray based on RRI value
  for (i in 2:(numfull + 1)) {
    plot_list[[i]] <- plot_opts$full
  }

  # Add a partially filled icon if needed
  if (numremain > 0) {
    plot_list[[numfull + 1]] <- plot_opts$partial
  }

  # Add empty icons if needed
  if (emptyhumans && length(plot_list) < infogs) {
    for (i in (numfull + 2):infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(length(plot_list) / infogs_ncol), 1)

  # Return the grid of icon plots
  plot_grid(plotlist = plot_list, nrow = rows)
}

#' #' @title Create Infographic with Icons and RRI Label
#' #' @description Combines a label of the RRI value with a grid of icons representing the RRI.
#' #' @param rri_raw The raw Relative Rate Index value.
#' #' @param infographic_color The color for the infographic elements.
#' #' @return A ggplot object combining the RRI label and the icon grid.
#' #' @export
#' fnc_create_infographic_homepage <- function(rri_raw, infographic_color) {
#'
#'   # Generate the icons for the infographic
#'   ggtemp_justpeople <- fnc_create_icons_homepage(
#'     rri_raw = rri_raw,
#'     infogs = default_ncols,
#'     infogs_ncol = default_ncols,
#'     fillcolor = infographic_color,
#'     partialcolor = light_color,
#'     emptyhumans = TRUE,
#'     emptycolor = "white",
#'     fillHoriz = FALSE
#'   )
#'
#'   print(ggtemp_justpeople)
#' }

# General setup
wd <- getwd()
whichimage <- "person-2745706-bw"

# Set up colors
light_color  <- "white"
empty_color   <- "#FFFFFF"
default_ncols <- 15

# Image setup
if (whichimage == "person-2745706-bw") {
  px_h <- 521
  px_w <- 323
  ex_h <- 0.005
  ex_w <- 0.02
  img_ar_hw <- (px_h * (1 + ex_h)) / (px_w * (1 + ex_w))
  img_ar_wh <- (px_w * (1 + ex_w)) / (px_h * (1 + ex_h))
  rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0)
}

# Example usage
fnc_create_icons_homepage(2.5, emptyhumans = TRUE)

