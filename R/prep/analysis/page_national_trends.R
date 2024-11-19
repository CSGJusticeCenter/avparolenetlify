#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: November 4, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################

#------------------------------------------------------------------------------#
# Parole Eligibility Table
#------------------------------------------------------------------------------#

# Select projection_year from NCRP projections created by Seba Guzman in Stata
parole_eligibility_table_projection_year <- ncrp_projections |>
  filter(year == projection_year) |>
  filter(!state %in% states_to_exclude$state) |>
  mutate(proj_pop_past_pey_rounded = fnc_round_to_power(proj_pop_past_pey),
         proj_pcnt_ppey_rounded = round(proj_pcnt_ppey, 0)) |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded)

# Get total past PE
proj_past_pe_count_rounded <- ncrp_projections |>
  filter(year == projection_year) |>
  filter(!state %in% states_to_exclude$state) |>
  group_by() |>
  summarise(n = sum(proj_pop_past_pey, na.rm = TRUE)) |>
  mutate(n_rounded = fnc_round_to_power(n)) |>
  pull(n_rounded)

# Filter out missing values from proj_pcnt_ppey
ncrp_projections_no_nas <- ncrp_projections |>
  filter(year == projection_year) |>
  filter(!is.na(proj_pcnt_ppey))

# Calculate the average percentage of people past parole eligibility
average_percent_past_pey <- mean(ncrp_projections_no_nas$proj_pcnt_ppey)

# Convert this percentage to a "1 in X" estimate
proj_past_pe_1_in_x <- round(100 / average_percent_past_pey, 1)


#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

fnc_blankitout_homepage <- function(){
  list(
    theme_void(),  # Removes background and gridlines for a clean appearance.
    scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
    scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
    theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
  )
}

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

# General setup
wd <- getwd()
whichimage <- "person-2745706-bw"

# Set up colors
light_color  <- "white"
empty_color   <- "#FFFFFF"
default_ncols <- ceiling(proj_past_pe_1_in_x)

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

# Create 1 in X infographic
fnc_create_icons_homepage(proj_past_pe_1_in_x, emptyhumans = TRUE)

# Save the infographic with the formatted state name
file_path <- file.path("img/pe_1_in_x.png")
ggsave(file_path, plot = last_plot(), width = 8, height = 6, dpi = 300)

# Load, crop, and save the image
img <- image_read(file_path)
img_cropped <- image_trim(img)
image_write(img_cropped, file_path)

#------------------------------------------------------------------------------#
# Parole Board Members by State
#------------------------------------------------------------------------------#

# Get parole status information by state
# Get number of parole board members
states_parole <- state_notes |>
  select(state, abolished_parole, members)


#------------------------------------------------------------------------------#
# Parole Eligibility Table
#------------------------------------------------------------------------------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- parole_eligibility_table_projection_year |>
  left_join(states_parole, by = "state") |>
  filter(abolished_parole == "N" | state == "Louisiana") |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded, members)

# Rename variables for downloadable table
parole_eligibility_table_download <- parole_eligibility_table |>
  select(State = state,
         `2023 Projection: In Prison Past Parole Eligibility (N)` = proj_pop_past_pey_rounded,
         `2023 Projection: In Prison Past Parole Eligibility (%)` = proj_pcnt_ppey_rounded,
         `Parole Board Members` = members)


#------------------------------------------------------------------------------#
# Parole Eligibility Map
#------------------------------------------------------------------------------#

# Create a vector of all state names
all_states <- state.name

# Define the gradient colors for categories
gradient_colors <- c(green1, green2, green3, green4, blue)

# Prepare tooltips and map data
# Prepare data for national maps
map_data <- parole_eligibility_table_projection_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(states_parole, by = "state") |>

  # Format data and create tooltip
  mutate(
    state_abb = state.abb[match(state, state.name)],

    all_na = ifelse(is.na(proj_pop_past_pey_rounded)
                    , TRUE, FALSE),

    # Create tooltips
    tooltip = case_when(

      state == "Louisiana" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(proj_pop_past_pey_rounded, 0),
               "<br>Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>",
               "Percentage of People: "),

      all_na == TRUE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Parole eligibility data is not available.<br>"),

      all_na == TRUE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Percentage of People: ",
               paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(proj_pop_past_pey_rounded, 0))
    ),

    tooltip = str_replace_all(tooltip, "NA%", "No Data"),
    tooltip = str_replace_all(tooltip, "NA", "No Data")
  ) |>

  # create data labels
  mutate(change_label = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         # change_label = str_replace_all(change_label, "NA%", "-"),
         change_label = str_replace_all(change_label, "NA%", " "),

         currentperclabel = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))


# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$proj_pcnt_ppey_rounded, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    gradient_color = findInterval(proj_pcnt_ppey_rounded, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(proj_pcnt_ppey_rounded), NA, gradient_colors[gradient_color]),
    proj_pcnt_ppey_rounded = round(proj_pcnt_ppey_rounded, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) |>
  group_by(gradient_color) |>
  mutate(
    data_category = case_when(
      # state == "Louisiana" ~ "Abolished Discretionary Parole",
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
      is.na(data_category) & abolished_parole == "Y" ~ "Abolished Discretionary Parole",
      # state == "Louisiana" ~ "Abolished Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,
      is.na(gradient_color) & data_category == "Abolished Discretionary Parole" ~ "white",
      # state == "Louisiana" ~ "white",
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "Abolished Discretionary Parole" ~ 5,
      # state == "Louisiana" ~ 5,
      TRUE ~ data_category_num
    )
  )

# create hex map
map_percent <- highchart(height = 625) |>

  hc_chart(marginTop = 50,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      align = "center",
                      formatter = JS("function() {
                          return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
                      }"),
                      style = list(fontSize = "16px",
                                   fontWeight = "regular",
                                   align = "center",
                                   fontFamily = "Graphik",
                                   textOutline = 0)),

    borderColor = darkgray,
    borderWidth = 0.5,
    nullColor = lightgray) |>

  hc_colorAxis(dataClassColor = "category",
               dataClasses = list(
                 list(from = 1, to = 1, color = green1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = green2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = green3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = green4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "Abolished Disretionary<br>Parole",
                      marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
                 list(from = 6, to = 6, color = darkgray, name = "Missing Data")
               )
  ) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_add_theme(base_hc_theme) |>

  hc_plotOptions(series = list(
    animation = FALSE,
    cursor = "pointer",
    borderWidth = 3,
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      pointDescriptionFormatter = JS("function(point) {
        return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
      }")
    )
  ),
  accessibility = list(
    enabled = TRUE,
    keyboardNavigation = list(enabled = TRUE),
    linkedDescription =
      paste0("This hexagonal map visualizes the projected proportion of people in prison past their parole eligibility across different U.S. states in 2023. ",
             "States are represented as hexagons, with color gradients indicating different percentage ranges of prison populations past parole eligibility. ",
             "The map also includes a category for states that have abolished discretionary parole and those with missing data."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description =
                                     paste0("This chart visually compares parole eligibility status across U.S. states, using colors to denote different percentage ranges.")))
  ) |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 1em;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(enabled = FALSE, filename = "proj_past_parole_eligibility_2023") |>

  hc_caption(text = ncrp_csg_source,
             y = 0) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 0,
            y = -30,
            itemMarginTop = 2,
            itemMarginBottom = 2)

# Add JavaScript to apply a gray border to the "Abolished Discretionary Parole" legend item
map_percent <- onRender(map_percent, "
  function(el, x) {
    // Add CSS to target the circle symbol of the second legend item
    var style = document.createElement('style');
    style.innerHTML = `
      .highcharts-legend-item:nth-child(5) .highcharts-point {
        stroke: gray;
        stroke-width: 1px;
      }
    `;
    document.head.appendChild(style);
  }
")

# View map
map_percent

# KEEP THIS CODE FOR NOW
# DOWNLOAD MAP OPTION - NO BUTTON?
map_percent_download <- highchart(height = 625,
                                  width = 1000) |>

  hc_chart(marginTop = 50,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      align = "center",
                      formatter = JS("function() {
                          return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
                      }"),
                      style = list(fontSize = "16px",
                                   fontWeight = "regular",
                                   align = "center",
                                   fontFamily = "Graphik",
                                   textOutline = 0)),

    borderColor = darkgray,
    borderWidth = 0.5,
    nullColor = lightgray) |>

  hc_colorAxis(dataClassColor = "category",
               dataClasses = list(
                 list(from = 1, to = 1, color = green1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = green2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = green3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = green4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "Abolished Disretionary<br>Parole",
                      marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
                 list(from = 6, to = 6, color = darkgray, name = "Missing Data")
               )
  ) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 0,
            y = -40,
            itemMarginTop = 2,
            itemMarginBottom = 2) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_add_theme(base_hc_theme) |>

  hc_plotOptions(series = list(
    animation = FALSE,
    cursor = "pointer",
    borderWidth = 3,
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      pointDescriptionFormatter = JS("function(point) {
        return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
      }")
    )
  ),
  accessibility = list(
    enabled = TRUE,
    keyboardNavigation = list(enabled = TRUE),
    linkedDescription =
      paste0("This hexagonal map visualizes the projected proportion of people in prison past their parole eligibility across different U.S. states in 2023. ",
             "States are represented as hexagons, with color gradients indicating different percentage ranges of prison populations past parole eligibility. ",
             "The map also includes a category for states that have abolished discretionary parole and those with missing data."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description =
                                     paste0("This chart visually compares parole eligibility status across U.S. states, using colors to denote different percentage ranges.")))
  ) |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 1em;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(
    enabled = FALSE) |>

  hc_caption(text = ncrp_csg_source,
             y = 0)

# Add JavaScript to apply a gray border to the "Abolished Discretionary Parole" legend item
map_proj_past_parole_eligibility_2023 <- onRender(map_percent_download, "
  function(el, x) {
    // Add CSS to target the circle symbol of the second legend item
    var style = document.createElement('style');
    style.innerHTML = `
      .highcharts-legend-item:nth-child(5) .highcharts-point {
        stroke: gray;
        stroke-width: 1px;
      }
    `;
    document.head.appendChild(style);
  }
")

# Save map_proj_past_parole_eligibility_2023 as a temporary HTML file
saveWidget(map_proj_past_parole_eligibility_2023, file = "temp.html", selfcontained = TRUE)

# Use webshot to take a screenshot and save it as a PNG
webshot2::webshot(
  url = "temp.html",
  file = file.path("img/map_proj_past_parole_eligibility_2023.png"),
  delay = 1,
  vwidth = 1200,
  vheight = 500,
  cliprect = c(0, 0, 1000, 625)
)

#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  map_percent                       = "map_percent.rds",
  proj_past_pe_count_rounded        = "proj_past_pe_count_rounded.rds",
  average_percent_past_pey          = "average_percent_past_pey.rds",
  parole_eligibility_table          = "parole_eligibility_table.rds",
  parole_eligibility_table_download = "parole_eligibility_table_download.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

# Define file name
file_name <- "parole_eligibility_by_state_2023_estimates.csv"

# Construct the full path
file_path <- file.path(app_folder, file_name)

# Example of writing to this path
write.csv(parole_eligibility_table_download, file_path, row.names = FALSE)




