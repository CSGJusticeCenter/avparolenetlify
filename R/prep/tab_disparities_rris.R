#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#    Focusing on RRIs
#######################################


# ---------------------------------------------------------------------------- #
# RRIs

# The Relative Rate Index (RRI) compares the rate of an event (e.g., incarceration)
# between groups. It is calculated as the ratio of the rate for the target group to
# the rate for the reference group (usually White, non-Hispanic).
# RRI = 1 means equal rates, RRI > 1 means higher rate for the target group (disparity exists),
# and RRI < 1 means a lower rate for the target group.
# ---------------------------------------------------------------------------- #

# These are the IDs of race variables that we want to pull from tidycensus.
# Each ID corresponds to a specific racial/ethnic group from the decennial census data.
# 18+ years - https://api.census.gov/data/2020/dec/pl/variables.html
race_vars <- c(estimate_white              = "P4_005N",
               estimate_black              = "P4_006N",
               estimate_hispanic           = "P4_002N")

# List of state names used to pull census data for each state
states <- state.name
states <- carl_state_notes |>
  filter(abolished_parole_16_total == "N", state %in% states) |>
  pull(state)

# Using lapply to apply the function `fnc_get_census_data` for each state in `states`.
# This will return a list where each element contains the processed census data for a state.
census_state_race_population_list <- lapply(states, fnc_get_census_data)

# Convert the list of state-level census data into a single data frame.
# Each state's data is stacked row-wise.
census_state_race_population <- bind_rows(census_state_race_population_list)

# Add a "state" column to the dataframe to identify which state each row belongs to.
# We repeat the state names for each state's corresponding number of rows.
census_state_race_population$state <- rep(states, each = nrow(census_state_race_population) / length(states))

# Grouping the census data by both state and race.
# Summing the population of each racial group across states (na.rm = TRUE ensures missing values are ignored).
census_state_race_population <- census_state_race_population |>
  group_by(state, race) |>
  summarise(state_population = sum(value, na.rm = TRUE), .groups = "drop")

# Merge the census data with prison population data by state and race.
# The prison data (`bjs_prison_pop_by_race_2020`) contains the number of people incarcerated by race in each state.
# After merging, the result will have both population and prison population data.
merged_data <- census_state_race_population %>%
  inner_join(bjs_prison_pop_by_race_2020, by = c("state", "race")) |>
  rename(prison_population = n)

# Calculate the incarceration rate per 100,000 people for each racial group in each state.
# This helps to compare the incarceration levels while accounting for the population sizes.
merged_data <- merged_data %>%
  mutate(incarceration_rate = prison_population / state_population * 100000)

# Reference Rate Calculation:
# To compute Relative Rate Index (RRI), we first select the incarceration rate for White, non-Hispanic people
# in each state, which will serve as the baseline or reference rate for other groups.
reference_rate <- merged_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, incarceration_rate) %>%
  rename(reference_rate = incarceration_rate)  # Rename for clarity.

# Calculate RRI (Relative Rate Index) by dividing the incarceration rate of each racial group by the reference rate.
# This gives us a comparison of how much more or less likely other racial groups are incarcerated compared to Whites.
all_rri_data <- merged_data %>%
  inner_join(reference_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate) %>%  # Calculate the RRI.
  select(state, race, rri)





# Dynamic sentence generation for Black people
# We use `map` to iterate through each state and create a sentence summarizing the RRI disparities for Hispanic people.
states <- unique(all_rri_data$state)
all_sentence_rri_black <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_rri_data %>%
    filter(state == x, race == "Black, non-Hispanic")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("<span style='color:#49a7a1; font-weight:bold;'>Black people</span> are incarcerated in state prison at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                             round(df1$rri, 1), " times</span> higher than <span style='color:#55b4e5; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_rri_black <- setNames(all_sentence_rri_black, states)
all_sentence_rri_black$Georgia

# Dynamic sentence generation for Hispanic people
# We use `map` to iterate through each state and create a sentence summarizing the RRI disparities for Hispanic people.
states <- unique(all_rri_data$state)
all_sentence_rri_hispanic <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_rri_data %>%
    filter(state == x, race == "Hispanic, any race")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("<span style='color:#d97d68; font-weight:bold;'>Hispanic people</span> are incarcerated in state prison at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                             round(df1$rri, 1), "</span> times higher than <span style='color:#55b4e5; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_rri_hispanic <- setNames(all_sentence_rri_hispanic, states)
all_sentence_rri_hispanic$Georgia




# ---------------------------------------------------------------------------- #
# PEOPLE INFOGRAPHICS FOR RRI's
# ---------------------------------------------------------------------------- #

# Image setup
whichimage <- "person-2745706-bw"
wd <- getwd()

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
icon_options <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
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
create_icons <- function(rri_raw, rri_digits = 1, fillcolor = dark_color, partialcolor = light_color, emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols, infogs_ncol = default_ncols, fillHoriz = FALSE) {
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

rri_greater_than_1 <- all_rri_data |>
  filter(race != "White, non-Hispanic" &
           race != "Other race(s), non-Hispanic" &
           rri > 1) |>
  select(state, race, rri)

rri_greater_than_1_black <- rri_greater_than_1 |>
  filter(race == "Black, non-Hispanic")

rri_greater_than_1_hispanic <- rri_greater_than_1 |>
  filter(race == "Hispanic, any race")

# Set up colors
light_color  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 15

create_infographic <- function(rri_raw, infographic_color) {
  # Round the RRI value and append "x" for display
  rri_text <- paste0(round(rri_raw, digits = 1), "x")

  # Create the people infographic
  ggtemp_justpeople <- create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = infographic_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Create a base plot for the RRI number with customized font and color
  rri_label_plot <- ggplot() +
    annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
             fontface = "bold",
             color = infographic_color,
             family = "Franklin Gothic Book") +
    theme_void()

  # Combine the RRI label and the people infographic using patchwork or cowplot
  final_plot <- plot_grid(
    rri_label_plot, ggtemp_justpeople,
    nrow = 1, rel_widths = c(1, 6)  # Adjust the widths as needed
  )

  print(final_plot)
}
# create_infographic(2.5)

# Create infographics and save them as PNGs for each state
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_black$state)
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_black |>
    filter(state == x)

  create_infographic(df_state$rri, color4)

  # Save the infographic
  ggsave(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"), plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"))
})

# RRI for Hispanic
# Create infographics and save them as PNGs for each state
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_hispanic |>
    filter(state == x)

  create_infographic(df_state$rri, color1)

  # Save the infographic
  ggsave(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_hispanic_", x, ".png"), plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_hispanic_", x, ".png"))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_hispanic_", x, ".png"))
})



# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

save(all_sentence_rri_black,                   file = file.path(app_folder, "all_sentence_rri_black.rds"))
save(all_sentence_rri_hispanic,                file = file.path(app_folder, "all_sentence_rri_hispanic.rds"))
save(all_rri_data,                             file = file.path(app_folder, "all_rri_data.rds"))



