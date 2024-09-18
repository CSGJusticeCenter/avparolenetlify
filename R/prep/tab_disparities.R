

# ---------------------------------------------------------------------------- #
# RRIs
# ---------------------------------------------------------------------------- #

# These are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P4_005N",
               estimate_black              = "P4_006N",
               estimate_asian              = "P4_008N",
               estimate_native_hawaiian_pi = "P4_009N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P4_007N",
               estimate_more_than_one_race = "P4_011N")

# Use lapply to retrieve and process data for each state
states <- state.name
census_state_race_population_list <- lapply(states, fnc_get_census_data)

# Convert list into a dataframe
census_state_race_population <- bind_rows(census_state_race_population_list)

# Add "state" column
census_state_race_population$state <- rep(states, each = nrow(census_state_race_population) / length(states))

census_state_race_population <- census_state_race_population |>
  group_by(state, race) |>
  summarise(state_population = sum(value, na.rm = TRUE))

# Merge census and prison data
merged_data <- census_state_race_population %>%
  inner_join(bjs_prison_pop_by_race_2020, by = c("state", "race")) |>
  rename(prison_population = n)

# Calculate incarceration rates
merged_data <- merged_data %>%
  mutate(incarceration_rate = prison_population / state_population * 100000) # per 100,000 people

# Calculate RRI
reference_rate <- merged_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, incarceration_rate) %>%
  rename(reference_rate = incarceration_rate)

all_rri_data <- merged_data %>%
  inner_join(reference_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate) %>%
  select(state, race, rri, incarceration_rate) |>
  mutate(incarceration_rate_10 = incarceration_rate/10)

# Generate sentences dynamically
states <- unique(all_rri_data$state)
# all_sentence_rri <- map(.x = states, .f = function(x) {
#
#   df1 <- all_rri_data %>%
#     filter(state == x)
#
#   higher_rates <- df1 %>%
#     filter(rri > 1 & race != "White, non-Hispanic") %>%
#     mutate(sentence = paste0(race, " people are incarcerated in state prison at a rate <b>",
#                              round(rri, 1), " times</b> higher")) %>%
#     pull(sentence)
#
#   if (length(higher_rates) > 0) {
#     final_sentence <- paste0(paste(higher_rates, collapse = " and "),
#                              " than White, non-Hispanic people, when accounting for population sizes in the community.")
#   } else {
#     final_sentence <- paste0("There are no disparities in prison incarceration rates compared to White, non-Hispanic people.")
#   }
#
#   return(final_sentence)
# })
# Generate sentences dynamically
all_sentence_rri <- map(.x = states, .f = function(x) {

  df1 <- all_rri_data %>%
    filter(state == x)

  higher_rates <- df1 %>%
    filter(rri > 1 & race != "White, non-Hispanic") %>%
    mutate(sentence = case_when(
      race == "Black, non-Hispanic" ~ paste0("<span style='color:#49a7a1; font-weight:bold;'>Black people</span> are incarcerated in state prison at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                                             round(rri, 1), " times</span> higher"),
      race == "Hispanic, any race" ~ paste0("<span style='color:#d97d68; font-weight:bold;'>Hispanic people</span> are incarcerated in state prison at a rate <span style='color:#d97d68; font-weight:bold;'>",
                                            round(rri, 1), " times</span> higher")
    )) %>%
    pull(sentence)

  if (length(higher_rates) > 0) {
    final_sentence <- paste0(paste(higher_rates, collapse = " and "),
                             " than <span style='color:#55b4e5; font-weight:bold;'>White people</span>, when accounting for population sizes in the community.")
  } else {
    final_sentence <- paste0("There are no disparities in prison incarceration rates compared to White, non-Hispanic people.")
  }

  return(final_sentence)
})

# Set names of the list to states
all_sentence_rri <- setNames(all_sentence_rri, states)
all_sentence_rri$Georgia



# ---------------------------------------------------------------------------- #
# PEOPLE INFOGRAPHICS FOR RRI's
# ---------------------------------------------------------------------------- #

# Image setup
whichimage <- "person-2745706-bw"

# Make sure you have the correct image path
# if (whichimage == "Person_icon_BLACK-01"){
if (whichimage == "person-2745706-bw"){
  px_h <- 521
  px_w <- 323
  # px_h <- 751
  # px_w <- 299
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

# Main function to create infographic
create_infographic <- function(rri_raw) {
  ggtemp_justpeople <- create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = dark_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  print(ggtemp_justpeople)
}

# # Call the function to create the infographic
# create_infographic(12)
#
# ggsave("high_res_infographic.png", plot = last_plot(), width = 8, height = 8, dpi = 300)

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
dark_color  <- color4
light_color  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 15

# create_infographic <- function(rri_raw) {
#   # Round the RRI value for display
#   rri_text <- round(rri_raw, digits = 1)
#
#   # Create the people infographic
#   ggtemp_justpeople <- create_icons(
#     rri_raw = rri_raw,
#     infogs = default_ncols,
#     infogs_ncol = default_ncols,
#     fillcolor = dark_color,
#     partialcolor = light_color,
#     emptyhumans = TRUE,
#     emptycolor = "white",
#     fillHoriz = FALSE
#   )
#
#   # Create a base plot for the RRI number
#   rri_label_plot <- ggplot() +
#     annotate("text", x = 1, y = 1, label = rri_text, size = 10, hjust = 1) +
#     theme_void()
#
#   # Combine the RRI label and the people infographic using patchwork or cowplot
#   final_plot <- plot_grid(
#     rri_label_plot, ggtemp_justpeople,
#     nrow = 1, rel_widths = c(1, 6)  # Adjust the widths as needed
#   )
#
#   print(final_plot)
# }
# create_infographic <- function(rri_raw) {
#   # Round the RRI value and append "x" for display
#   rri_text <- paste0(round(rri_raw, digits = 1), "x")
#
#   # Create the people infographic
#   ggtemp_justpeople <- create_icons(
#     rri_raw = rri_raw,
#     infogs = default_ncols,
#     infogs_ncol = default_ncols,
#     fillcolor = dark_color,
#     partialcolor = light_color,
#     emptyhumans = TRUE,
#     emptycolor = "white",
#     fillHoriz = FALSE
#   )
#
#   # Create a base plot for the RRI number
#   rri_label_plot <- ggplot() +
#     annotate("text", x = 1, y = 1, label = rri_text, size = 10, hjust = 1) +
#     theme_void()
#
#   # Combine the RRI label and the people infographic using patchwork or cowplot
#   final_plot <- plot_grid(
#     rri_label_plot, ggtemp_justpeople,
#     nrow = 1, rel_widths = c(1, 8)  # Adjust the widths as needed
#   )
#
#   print(final_plot)
# }
# create_infographic(2.5)
create_infographic <- function(rri_raw) {
  # Round the RRI value and append "x" for display
  rri_text <- paste0(round(rri_raw, digits = 1), "x")

  # Create the people infographic
  ggtemp_justpeople <- create_icons(
    rri_raw = rri_raw,
    infogs = default_ncols,
    infogs_ncol = default_ncols,
    fillcolor = dark_color,
    partialcolor = light_color,
    emptyhumans = TRUE,
    emptycolor = "white",
    fillHoriz = FALSE
  )

  # Create a base plot for the RRI number with customized font and color
  rri_label_plot <- ggplot() +
    annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
             fontface = "bold",
             color = color4,
             family = "Franklin Gothic Book") +
    theme_void()

  # Combine the RRI label and the people infographic using patchwork or cowplot
  final_plot <- plot_grid(
    rri_label_plot, ggtemp_justpeople,
    nrow = 1, rel_widths = c(1, 6)  # Adjust the widths as needed
  )

  print(final_plot)
}
create_infographic(2.5)

# Create infographics and save them as PNGs for each state
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_black$state)
states <- "Georgia"
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_black |>
    filter(state == x)

  create_infographic(df_state$rri)

  # Save the infographic
  ggsave(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"), plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_black_", x, ".png"))
})















# Set up colors
dark_color  <- color1

# Create infographics and save them as PNGs for each state
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_hispanic |>
    filter(state == x)

  create_infographic(df_state$rri)

  # Save the infographic
  ggsave(paste0(config$sp_data_path, "/data/analysis/app/rri_infographic_hispanic_", x, ".png"), plot = last_plot(), width = 8, height = 8, dpi = 300)
})















# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity
# ---------------------------------------------------------------------------- #

# Filter and prepare the data
all_parole_release_disparities <- filter_population_criteria(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(#time_between_ped_release_category != "Missing Parole Eligibility Year" &
         #  time_between_ped_release_category != "Released before Parole Eligibility Year" &???? FIX THIS
           !is.na(time_between_ped_rptyear) &
           !is.na(parelig_year) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0
           #reltype == "Conditional release"????????????????????????????????????
  ) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))

# df1 <- all_parole_release_disparities |>
#   filter(state == "Georgia") |>
#   select(time_between_ped_release, race) |>
#   group_by(race) |>
#   summarise(total_years = sum(time_between_ped_release, na.rm = TRUE))

all_pe_release_total_years_race <- all_parole_release_disparities |>
  filter(!is.na(race)) |>
  group_by(state, race) |>
  summarise(total_years = sum(time_between_ped_release, na.rm = TRUE))

# Median years between release and PE
# Not much of a difference?
all_pe_release_median_years_race <- all_parole_release_disparities |>
  filter(!is.na(race)) |>
  group_by(state, race) |>
  summarise(median_years = median(time_between_ped_release, na.rm = TRUE))









# ---------------------------------------------------------------------------- #
# Timing of Release by Offense Type, Race, and Ethnicity
# ---------------------------------------------------------------------------- #





# ---------------------------------------------------------------------------- #
# Time Served by Offense Type
# ---------------------------------------------------------------------------- #

# Calculate average length of stay by race and state
ncrp_race_los <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(race != "Unknown") |>
  group_by(state, race, rptyear) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE)) |>
  pivot_longer(cols = c(average_los), names_to = "type", values_to = "average_los") |>
  select(-type)

df1 <- ncrp_race_los |> filter(state == "Georgia")


# Calculate the average length of stay by race, state, and by offense type
ncrp_race_los_by_offense_type <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(race != "Unknown") |>
  group_by(state, race, fbi_index, rptyear) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE),
    people_released = n()) |>
  pivot_longer(cols = c(average_los), names_to = "type", values_to = "average_los") |>
  select(-type) |>
  mutate(race = factor(race, levels = c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic")))

# Get unique states
states <- unique(ncrp_race_los_by_offense_type$state)
all_scatter_los_race_offense <- map(.x = states, .f = function(x) {

  df1 <- ncrp_race_los_by_offense_type |>
    ungroup() |>
    filter(state == x)|>
    mutate(fbi_index_num = as.numeric(as.factor(fbi_index)))

  # Create a named vector for y-axis labels
  y_labels <- setNames(unique(as.factor(df1$fbi_index)), unique(as.numeric(as.factor(df1$fbi_index))))

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(fbi_index_num, start_x, end_x, race, fbi_index)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  highcharts <- highchart() |>
    # hc_add_series(
    #   df_lines,
    #   type = 'line',
    #   hcaes(x = value, y = fbi_index_num, group = fbi_index),
    #   lineWidth = 1,
    #   color = "black",
    #   dashStyle = "solid",
    #   opacity = 1,
    #   marker = list(enabled = FALSE),
    #   enableMouseTracking = FALSE,
    #   showInLegend = FALSE
    # ) |>
    hc_add_series(
      df1,
      type = 'scatter',
      marker = list(symbol = "circle", radius = 5),
      hcaes(x = average_los, y = fbi_index_num, group = race, name = fbi_index)
    ) |>
    hc_yAxis(
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      lineColor = "black",
      tickColor = "black",
      title = list(text = "Average Length of Stay (Years)",
                   style = list(color = "black")),
      labels = list(style = list(color = "black")),
      gridLineDashStyle = "Dash",  # Add dashed grid lines
      gridLineWidth = 1,           # Ensure grid lines are visible
      gridLineColor = lightgray       # Set grid line color
    ) |>
    hc_title(text = "Average Length of Stay by Offense and Race and Ethnicity") |>
    hc_colors(c(color1, color2, color4, color3)) |>
    hc_exporting(enabled = TRUE) |>
    hc_add_theme(base_hc_theme) |>
    hc_tooltip(
      headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
      pointFormat = paste0(
        '<span style="color:{point.color}">\u25CF</span> {series.name}:<br/>',
        'Offense: {point.name}<br/>',
        'Average LOS: {point.x: .1f} years<br/>',
        'People Released: {point.people_released}<br/>'
      )
    ) |>
    hc_legend(verticalAlign = "top",
              layout = "horizontal")

  return(highcharts)
})

# Name the list of charts by state
all_scatter_los_race_offense <- setNames(all_scatter_los_race_offense, states)

# Display the chart for Georgia as an example
all_scatter_los_race_offense$Georgia






# Calculate average length of stay by race and state
ncrp_race_los <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(race != "Unknown") |>
  group_by(state, race, rptyear) |>
  summarise(
    average_los = mean(time_between_admisson_release, na.rm = TRUE)) |>
  pivot_longer(cols = c(average_los), names_to = "type", values_to = "average_los") |>
  select(-type)  |>
  droplevels()




states <- unique(ncrp_race_los$state)

all_lollipop_los_race <- map(.x = states, .f = function(x) {

  df1 <- ncrp_race_los |>
    ungroup() |>
    filter(state == x) |>
    arrange(desc(average_los)) |>
    mutate(race_num = row_number(),
           color = case_when(
             race == "White, non-Hispanic" ~ color2,
             race == "Black, non-Hispanic" ~ color1,
             race == "Hispanic, any race" ~ color4,
             race == "Other race(s), non-Hispanic" ~ color3
           ))

  max_los <- max(df1$average_los, na.rm = TRUE)

  # Create a named vector for y-axis labels
  y_labels <- setNames(as.character(df1$race), df1$race_num)

  # Create the df_lines dataframe
  df_lines <- df1 |>
    mutate(start_x = 0, end_x = average_los) |>
    select(race_num, start_x, end_x, race)

  # Reshape df_lines for highcharter
  df_lines <- df_lines |>
    gather(key = "point", value = "value", start_x, end_x)

  highcharts <- highchart() |>
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = race_num, group = race),
      lineWidth = 1,
      color = "black",
      dashStyle = "solid",
      opacity = 1,
      marker = list(enabled = FALSE),
      enableMouseTracking = FALSE,
      showInLegend = FALSE
    ) |>
    hc_add_series(
      df1,
      type = 'scatter',
      marker = list(symbol = "circle", radius = 5),
      hcaes(x = average_los, y = race_num, group = race, name = race, color = color),
      dataLabels = list(
        enabled = TRUE,
        format = '{point.x:.1f} Years',
        align = "left",
        y = 9,
        x = 8,
        style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
      )
    ) |>
    hc_add_theme(base_hc_theme)|>
    hc_yAxis(
      labels = list(
        style = list(
          color = 'black',
          fontWeight = "regular",
          fontSize = "12px"
        )
      ),
      title = list(text = ""),
      majorGridLineColor = "transparent",
      gridLineColor = "transparent",
      lineColor = "transparent",
      majorGridLineColor = "transparent",
      minorGridLineColor = "transparent",
      tickColor = "black",
      categories = y_labels
    ) |>
    hc_xAxis(
      title = list(text = ""),
      labels = list(enabled = FALSE),
      lineColor = "transparent",
      minorGridLineColor = "transparent",
      tickLength = 0,
      gridLineColor = "transparent",
      tickColor = "transparent",
      max = max_los*1.5
    ) |>
    hc_exporting(enabled = FALSE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = 150)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_los_race <- setNames(all_lollipop_los_race, states)

# Display the chart for Georgia as an example
all_lollipop_los_race$Georgia




# Generate sentence for each state
states <- unique(ncrp_race_los$state)
all_sentence_los_race <- map(.x = states, .f = function(x) {

  df1 <- ncrp_race_los |>
    ungroup() |>
    mutate(race = case_when(
      race == "White, non-Hispanic" ~ "White",
      race == "Black, non-Hispanic" ~ "Black",
      race == "Hispanic, any race" ~ "Hispanic",
      race == "Other race(s), non-Hispanic" ~ "Other races"
    )) |>
    filter(state == x)

  # Handling missing data
  if (nrow(df1) == 0) {
    return(paste0("No data available for ", x))
  }

  # Focus on comparisons with White people
  df_white <- df1 |> filter(race == "White")

  # Generate sentences for Black and Hispanic comparisons
  sentence <- ""
  for (race_group in c("Black", "Hispanic")) {
    df_race <- df1 |> filter(race == race_group)
    if (nrow(df_race) > 0 && nrow(df_white) > 0) {
      los_diff <- df_race$average_los - df_white$average_los
      if (los_diff > 0) {
        sentence <- paste0(sentence,
                           race_group, " people faced ", round(los_diff, 1),
                           " more years on average compared to White people in ", df_race$rptyear[1], ". "
        )
      }
    }
  }

  # If no disparities are found, return a different message
  if (sentence == "") {
    sentence <- paste0("No significant disparities compared to White people found for ", x)
  }

  return(sentence)
})

# Set names for the list elements
all_sentence_los_race <- setNames(all_sentence_los_race, states)

# Check the sentence for Georgia
all_sentence_los_race$Georgia











# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_pe_release_total_years_race, file = file.path(folder, "all_pe_release_total_years_race.rds"))
  save(all_scatter_race_ped_release,   file = file.path(folder, "all_scatter_race_ped_release.rds"))
  save(all_bubble_race_ped_release,    file = file.path(folder, "all_bubble_race_ped_release.rds"))

  save(all_sentence_rri,               file = file.path(folder, "all_sentence_rri.rds"))

  save(all_sentence_los_race,          file = file.path(folder, "all_sentence_los_race.rds"))
  save(all_lollipop_los_race,          file = file.path(folder, "all_lollipop_los_race.rds"))

  save(all_sentence_los_race_offense,  file = file.path(folder, "all_sentence_los_race_offense.rds"))
  save(all_scatter_los_race_offense,   file = file.path(folder, "all_scatter_los_race_offense.rds"))

  save(all_rri_data ,                  file = file.path(folder, "all_rri_data.rds"))
}

