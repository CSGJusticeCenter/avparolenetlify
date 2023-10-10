# Load png file with human
library(png)
img <- readPNG("C:/repos/avparolenetlify/img/human2.png")
img <- ifelse(img > 0 & img < 1, 1, ifelse(img == 0, 0, 1))

blankitout <- theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_blank()
)

cols_orange <- c(rgb(255, 255, 255, maxColorValue = 255), # white
                 rgb(255, 165, 0, maxColorValue = 255))   # orange

df_orange <- reshape2::melt(img[,,1])
plot_orange <- ggplot(df_orange, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_tile() +
  coord_cartesian(ylim = c(nrow(img), 1)) +
  scale_fill_manual(values = unlist(cols_orange)) +
  blankitout

create_infograph <- function(setrri, infogs = 9, emptyhumans = TRUE, fillcolor = "#00aba0", fillHoriz = TRUE) {
  cols2 <- c(rgb(255, 255, 255, maxColorValue = 255),
             rgb(167, 169, 172, maxColorValue = 255),
             rgb(col2rgb(fillcolor)[1], col2rgb(fillcolor)[2], col2rgb(fillcolor)[3], maxColorValue = 255))
  cols0 <- c(rgb(255, 255, 255, maxColorValue = 255),
             rgb(167, 169, 172, maxColorValue = 255))
  cols1 <- c(rgb(255, 255, 255, maxColorValue = 255),
             rgb(col2rgb(fillcolor)[1], col2rgb(fillcolor)[2], col2rgb(fillcolor)[3], maxColorValue = 255))

  RRI <- setrri
  numfull <- floor(RRI)
  numremain <- RRI - numfull

  if (infogs - setrri < 1) {
    infogs <- floor(setrri) + 2
    warning(paste0("There are not enough infographics to plot! Number of infographics reset to ", floor(setrri) + 2))
  }
  if (infogs >= 10) {
    rows <- 2
  } else {
    rows <- 1
  }

  blank <- numfull + 2 + 1

  pos1 <- if(fillHoriz) which(apply(img[,,1], 2, function(y) any(y == 1))) else which(apply(img[,,1], 1, function(y) any(y == 1)))
  max_pos <- if(fillHoriz) 182 else 437
  h <- dim(img)[1]
  w <- dim(img)[2]
  min_pos <- min(pos1)

  # Code for generating the different states of humans (full, empty, partial) goes here...
  # It appears that in your original code, you generate these plots and store them in plot1, plot2, etc.

  plot_list <- list()

  # Add orange figure to the plot_list
  plot_list[[1]] <- plot_orange
  if (RRI > 1) {
    for (i in 2:(numfull+1)){
      plot_list[[i]] <- plot1  # Example for full teal human
    }
    plot_list[[numfull+2]] <- plot2  # Example for partial teal human
    for (i in (numfull+3):infogs){
      plot_list[[i]] <- plot0  # Example for empty gray human
    }
  } else {
    plot_list[[2]] <- plot2  # Example for partial teal human
    for (i in 3:infogs){
      plot_list[[i]] <- plot0  # Example for empty gray human
    }
  }

  plot_grid(plotlist = plot_list, nrow = rows)
}

graph2 <- create_infograph(6.35, emptyhumans = FALSE)
graph2
