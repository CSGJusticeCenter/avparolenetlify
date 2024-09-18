# THIS WORKS BUT IS PIXELATED FROM SVG

library(ggplot2)
library(png)
library(cowplot)
library(reshape2)
library(glue)

# Set up colors
mclc_dk_blue  <- color1
mclc_lt_blue  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 12

# Image setup
whichimage <- "person-2745706-bw"

# Make sure you have the correct image path
if (whichimage == "person-2745706-bw"){
  px_h <- 521
  px_w <- 323
  ex_h <- 0.005
  ex_w <- 0.02
  img_ar_hw <- (px_h*(1+ex_h)) / (px_w*(1+ex_w))
  img_ar_wh <- (px_w*(1+ex_w)) / (px_h*(1+ex_h))
  rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0)
}

# Plotting setup
blankitout <- function(){
  list(
    theme_void(),
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),
    theme(legend.position = "none", aspect.ratio = img_ar_hw)
  )
}

# Create Plot list of empty, full, and partial icons
icon_options <- function(partialval, empty = "#FFFFFF", fill = mclc_dk_blue, partial = mclc_lt_blue, bg = "#FFFFFF", fillHoriz = FALSE) {
  if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")

  cols_lst <- list(
    "empty" = c(bg, empty),
    "full" = c(bg, fill),
    "partial" = c(bg, partial, fill)
  )
  pcts_lst <- list(
    "empty" = 0,
    "full" = 100,
    "partial" = partialval * 100
  )
  plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)

  if (fillHoriz == FALSE) {
    pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))
    max <- max(pos1)
  } else {
    pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))
    max <- max(pos1)
  }
  h <- dim(img)[1]
  w <- dim(img)[2]
  min <- min(pos1)

  for (j in names(plot_lst)) {
    pcts <- pcts_lst[[j]]
    pospct <- round((max - min) * pcts / 100 + min)
    finalimg <- img[h:1,,1]
    bkgr <- (finalimg == 1)
    colfill <- matrix(rep(FALSE, h*w), nrow = h)

    if (fillHoriz == FALSE) {
      colfill[1:h, max:pospct] <- TRUE
    } else {
      colfill[max:pospct, 1:w] <- TRUE
    }

    finalimg[bkgr & colfill] <- 0.5
    df <- reshape2::melt(finalimg)

    if (j == "full") {
      df[df$value == 0.5, ] <- 0
    }

    plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
      geom_raster() +
      scale_fill_manual(values = cols_lst[[j]]) +
      blankitout()

    plot_lst[[j]] <- plot
  }

  return(plot_lst)
}

# Create the icons
create_icons <- function(rri_raw, rri_digits = 1, fillcolor = mclc_dk_blue, partialcolor = mclc_lt_blue, emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols, infogs_ncol = default_ncols, fillHoriz = FALSE) {
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)
  numremain <- RRI - numfull

  plot_opts <- icon_options(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)

  plot_list <- list()

  if (RRI > 1 & numremain != 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
    plot_list[[numfull + 1]] <- plot_opts$partial
  } else if (RRI > 1 & numremain == 0) {
    for (i in 1:numfull) {
      plot_list[[i]] <- plot_opts$full
    }
  } else if (RRI == 1) {
    plot_list[[1]] <- plot_opts$full
  } else if (RRI < 1) {
    plot_list[[1]] <- plot_opts$partial
  }

  if (emptyhumans == TRUE & length(plot_list) != infogs) {
    st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
    for (i in st_empty:infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  rows <- ifelse(infogs > infogs_ncol, ceiling(rri_raw / infogs_ncol), 1)
  plot_grid(plotlist = plot_list, nrow = rows)
}

# Main function to create infographic
create_infographic <- function(rri_raw) {
  ggtemp_justpeople <- create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = mclc_dk_blue,
    partialcolor = mclc_lt_blue,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  print(ggtemp_justpeople)
}

# Call the function to create the infographic
create_infographic(3.5)
