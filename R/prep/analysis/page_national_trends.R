#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: December 5, 2024 (MAR)
# Description:
#    This script generates a parole eligibility map, tables, and other visualizations
#    for the national trends page of the AV Parole project. It includes:
#    - Filtering and processing NCRP projection data
#    - Calculating total populations and ratios
#    - Creating infographics to visualize data
#    - Generating tooltips and preparing data for a national map visualization
#######################################

#------------------------------------------------------------------------------#
# Parole Eligibility Table
#------------------------------------------------------------------------------#

# Filter NCRP projections for the specified year
# Exclude states listed in `states_to_exclude` and process data for rounded values
# This table forms the basis for parole eligibility statistics by state
parole_eligibility_table_projection_year <- ncrp_projections |>
  # Filter by projection year
  filter(year == projection_year & !is.na(proj_pop_past_pey)) |>
  # Exclude specified states
  filter(!state %in% states_abolished_parole$state) |>
  mutate(
    # Round population to nearest power
    # Round percentage past PEY
    proj_pop_past_pey_rounded = fnc_round_to_power(proj_pop_past_pey),
    proj_pcnt_ppey_rounded = round(proj_pcnt_ppey, 0)
  ) |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded)

# Calculate the total projected population past parole eligibility (PE)
# This calculation aggregates the population across all states
proj_past_pe <- ncrp_projections |>
  # Filter by projection year
  filter(year == projection_year) |>
  summarise(
    # Sum population past parole eligibility
    past_pe_pop = sum(proj_pop_past_pey, na.rm = TRUE)
  ) |>
  pull(past_pe_pop)

# Rounded value
proj_past_pe_count_rounded <- fnc_round_to_power(proj_past_pe)

# Calculate the total prison population and the ratio "1 in X" individuals past PEY
proj_prison_pop <- ncrp_population_projections |>
  # Exclude specified states
  # filter(!state %in% states_abolished_parole$state) |> # we may want this but not now
  # Filter by projection year
  filter(year == projection_year) |>
  summarise(
    # Total prison population
    total_prison_pop = sum(total_prison_population, na.rm = TRUE)
  ) |>
  # Extract the total prison population
  pull(total_prison_pop)

# Calculate the "1 in X" ratio: total prison population / population past parole eligibility
proj_past_pe_1_in_x <- round(proj_prison_pop / proj_past_pe, 0)

#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

# Configure image and visualization settings for the "1 in X" infographic
# Image height and width in pixels
px_h <- 521
px_w <- 323

# Adjustments for additional spacing
ex_h <- 0.005
ex_w <- 0.02

# Calculate aspect ratios for height-to-width and width-to-height
img_ar_hw <- (px_h * (1 + ex_h)) / (px_w * (1 + ex_w))
img_ar_wh <- (px_w * (1 + ex_w)) / (px_h * (1 + ex_h))

# Load the raw image and invert pixel values (convert black to white and vice versa)
rawimg <- readPNG(file.path(getwd(), glue("img/person-2745706-bw.png")))
img <- ifelse(rawimg == 0, 1, 0)

# Generate the infographic showing "1 in X" individuals past parole eligibility
default_ncols <- proj_past_pe_1_in_x
fnc_create_icons_homepage(proj_past_pe_1_in_x, emptyhumans = TRUE)

# Save the infographic as a PNG file
file_path <- file.path("img/pe_1_in_x.png")
ggsave(file_path, plot = last_plot(), width = 8, height = 6, dpi = 300)

# Load the saved image, crop white space, and save the processed image
img <- image_read(file_path)                              # Load the saved infographic
img_cropped <- image_trim(img)                            # Crop white space
image_write(img_cropped, file_path)                       # Save the cropped image

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

# Merge parole eligibility data with state-level parole status and board member information
# Filter to include states with parole systems and select key columns for download
parole_eligibility_table <- parole_eligibility_table_projection_year |>
  left_join(states_parole, by = "state") |>               # Join with state-level parole data
  filter(
    abolished_parole == "N" | state == "Louisiana"        # Include states with parole systems or Louisiana
  ) |>
  select(
    state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded, members  # Select relevant columns
  )

# Create a downloadable version of the table with formatted column names
parole_eligibility_table_download <- parole_eligibility_table |>
  select(
    State = state,
    `2023 Projection: In Prison Past Parole Eligibility (N)` = proj_pop_past_pey_rounded,
    `2023 Projection: In Prison Past Parole Eligibility (%)` = proj_pcnt_ppey_rounded,
    `Parole Board Members` = members
  )

#------------------------------------------------------------------------------#
# Parole Eligibility Map
#------------------------------------------------------------------------------#

# Create a vector of all state names
all_states <- state.name

# Define the gradient colors for categories
gradient_colors <- c(gradient1, gradient2, gradient3, gradient4, blue)

# Generate a complete dataset for national map
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

    # Generate tooltips for each state based on parole eligibility status
    tooltip = case_when(

      # Special tooltip for Louisiana
      state == "Louisiana" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Percentage of People: ", paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ", formattable::comma(proj_pop_past_pey_rounded, 0),
               "<br>Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>"),

      # Missing data and has a parole system
      all_na == TRUE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Parole eligibility data is not available.<br>"),

      # Missing data and does not have a parole system
      all_na == TRUE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      # Has data and does not have a parole system
      all_na == FALSE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      # Has data and has a parole system
      all_na == FALSE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Percentage of People: ",
               paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(proj_pop_past_pey_rounded, 0))
    ),

    # Reformat NA info in tooltip
    tooltip = str_replace_all(tooltip, "NA%", "No Data"),
    tooltip = str_replace_all(tooltip, "NA", "No Data")
  ) |>

  # create data labels
  mutate(change_label = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         change_label = str_replace_all(change_label, "NA%", " "),

         currentperclabel = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$proj_pcnt_ppey_rounded, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)

# Round breaks, ensuring no duplicates and strictly increasing order
breaks <- unique(round(breaks, 0))
breaks <- cummax(breaks)

map_data_breaks <- map_data |>
  mutate(
    gradient_color = findInterval(proj_pcnt_ppey_rounded, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(proj_pcnt_ppey_rounded), NA, gradient_colors[gradient_color]),
    proj_pcnt_ppey_rounded = round(proj_pcnt_ppey_rounded, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) |>
  group_by(gradient_color) |>
  # First, define the data category ranges based on breaks
  mutate(
    data_category = case_when(
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
    )
  ) |>
  # Then handle special categories like missing data and no discretionary parole
  mutate(
    data_category = case_when(
      is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
      is.na(data_category) & abolished_parole == "Y" ~ "No Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,
      is.na(gradient_color) & data_category == "No Discretionary Parole" ~ "white",
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "No Discretionary Parole" ~ 5,
      TRUE ~ data_category_num
    )
  )

#------------------------------------------------------------------------------#
# Generate Hexagonal Map for Projected Parole Eligibility Percentages
#------------------------------------------------------------------------------#

# Create the main map visualization
map_percent <- highchart(height = 625) |>

  # Configure the chart margins
  hc_chart(marginTop = 50, marginBottom = 50, marginRight = 50) |>

  # Add the hexagonal map data
  hc_add_series_map(
    map = hex_gj,                           # GeoJSON hex grid for states
    df = map_data_breaks,                   # Data to populate the map
    joinBy = "state_abb",                   # Join key for state abbreviations
    value = "data_category_num",            # Numeric category for color mapping
    dataLabels = list(
      enabled = TRUE,
      useHTML = TRUE,
      align = "center",
      formatter = JS("
        function() {
          return '<div style=\"text-align:center; font-weight:regular;\">' +
                 this.point.state_abb + '<br>' + this.point.change_label + '</div>';
        }"),
      style = list(
        fontSize = "16px",
        fontWeight = "regular",
        align = "center",
        fontFamily = "Graphik",
        textOutline = 0
      )
    ),
    borderColor = darkgray,                 # Border color for hexagons
    borderWidth = 0.5,                      # Border width for hexagons
    nullColor = lightgray                   # Color for missing data
  ) |>

  # Define the color axis for the map
  hc_colorAxis(
    dataClassColor = "category",            # Use categories for color classification
    dataClasses = list(
      list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
      list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
      list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
      list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
      list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
           marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Add a white legend marker for "No Discretionary Parole"
      list(from = 6, to = 6, color = darkgray, name = "Missing Data")      # Color for missing data
    )
  ) |>

  # Configure the X and Y axes (empty for maps)
  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  # Apply a base theme for consistent styling
  hc_add_theme(base_hc_theme) |>

  # Configure accessibility and interactivity for the map
  hc_plotOptions(
    series = list(
      animation = FALSE,                    # Disable animation for static maps
      cursor = "pointer",                   # Enable pointer cursor for interactivity
      borderWidth = 3,                      # Set border width
      accessibility = list(
        enabled = TRUE,                     # Enable accessibility
        keyboardNavigation = list(enabled = TRUE),
        pointDescriptionFormatter = JS("
          function(point) {
            return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
          }")
      )
    ),
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      linkedDescription = paste0(
        "This hexagonal map visualizes the projected proportion of people in prison ",
        "past their parole eligibility across U.S. states in 2023. Colors indicate ",
        "percentage ranges, with special categories for states with abolished parole ",
        "and missing data."
      ),
      landmarkVerbosity = "one"
    )
  ) |>

  # Configure tooltips for interactivity
  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF',           # White background for tooltips
    outside = TRUE,                        # Render tooltip outside the map
    useHTML = TRUE,
    formatter = JS("
      function() {
        return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
               '<div style=\"text-align:left;\">' +
               '<span style=\"font-weight:normal; font-size: 1em;\">' + this.point.tooltip + '</span>' +
               '</div></div>';
      }")
  ) |>

  # Add the title and caption for the map
  hc_title(
    text = "Percentage of People in Prison Past Parole Eligibility",
    align = "center",
    style = list(fontSize = "1.75em", fontWeight = "bold")
  ) |>
  hc_caption(
    text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
    y = 0
  ) |>

  # Add a legend to explain the color categories
  hc_legend(
    align = "right",
    verticalAlign = "bottom",
    layout = "vertical",
    symbolHeight = 15,
    symbolWidth = 15,
    x = 0,
    y = -30,
    itemMarginTop = 2,
    itemMarginBottom = 2
  )

# Add custom JavaScript to style the "No Discretionary Parole" legend item
# White circle needs a gray circle - can't do this in highcharter legend
map_percent <- onRender(map_percent, "
  function(el, x) {
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

# Render the map for preview
map_percent

# Save map_percent as a temporary HTML file
saveWidget(map_percent, file = "temp.html", selfcontained = TRUE)

# Use webshot to take a screenshot and save it as a PNG
webshot2::webshot(
  url = "temp.html",
  file = file.path("img/map_proj_past_parole_eligibility_2023.png"),
  delay = 1,
  vwidth = 1200,
  vheight = 500
  # cliprect = c(0, 0, 1000, 625)
)

#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  map_percent                       = "map_percent.rds",
  proj_past_pe_count_rounded        = "proj_past_pe_count_rounded.rds",
  proj_past_pe_1_in_x               = "proj_past_pe_1_in_x.rds",
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

# Save csv
write.csv(parole_eligibility_table_download, file_path, row.names = FALSE)
















# # KEEP CODE FOR NOW
# # Calculate the breaks for the percent of people eligible for parole
# num_breaks <- length(gradient_colors) - 1
# breaks <- quantile(map_data$proj_pcnt_ppey_rounded, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
# breaks[1] <- 0  # Set the first break to 0
# breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
# breaks <- cummax(breaks)  # Ensure breaks are strictly increasing
#
# # Add Color Gradients and Data Categories to the Map Data
# map_data_breaks <- map_data |>
#   mutate(
#     # Assign gradient colors based on the rounded percentage of people past PEY
#     # - `findInterval` maps each percentage to a break range defined in `breaks`
#     # - `rightmost.closed` ensures the upper bound is inclusive
#     # - `all.inside` forces values outside breaks to be assigned to the closest range
#     gradient_color = findInterval(proj_pcnt_ppey_rounded, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
#
#     # Map the numeric gradient category to the corresponding color, handling NA values
#     gradient_color = ifelse(is.na(proj_pcnt_ppey_rounded), NA, gradient_colors[gradient_color]),
#
#     # Round the projected percentage past PEY to the nearest whole number for clarity
#     proj_pcnt_ppey_rounded = round(proj_pcnt_ppey_rounded, 0),
#
#     # Convert gradient colors to numeric categories for use in visualizations
#     data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
#   ) |>
#
#   # Group by gradient color for calculating categories
#   group_by(gradient_color) |>
#
#   # Define the data category labels based on gradient ranges
#   mutate(
#     data_category = case_when(
#       # Assign labels corresponding to each break range
#       gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
#       gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
#       gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
#       gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
#       gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
#     ),
#
#     # Handle NA values by assigning specific categories for missing data and no discretionary parole
#     data_category = case_when(
#       is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
#       is.na(data_category) & abolished_parole == "Y" ~ "No Discretionary Parole",
#       TRUE ~ data_category
#     ),
#
#     # Assign default gradient colors for special categories (e.g., missing data and no parole)
#     gradient_color = case_when(
#       is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,  # Dark gray for missing data
#       is.na(gradient_color) & data_category == "No Discretionary Parole" ~ "white",  # White for no parole
#       TRUE ~ gradient_color  # Retain original gradient color for valid categories
#     ),
#
#     # Assign numeric identifiers for each data category
#     data_category_num = case_when(
#       is.na(data_category_num) & data_category == "Missing Data" ~ 6,  # Assign 6 for missing data
#       is.na(data_category_num) & data_category == "No Discretionary Parole" ~ 5,  # Assign 5 for no parole
#       TRUE ~ data_category_num  # Retain original category number for valid data
#     )
#   )
