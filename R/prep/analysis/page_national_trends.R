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

# Filter NCRP projections for the specified projection year and calculate rounded values
# - Exclude states listed in `states_to_exclude`
# - Calculate projected population past parole eligibility year (PEY) rounded to nearest power
# - Round percentage past PEY to the nearest whole number
# - Select only relevant columns for output
parole_eligibility_table_projection_year <- ncrp_projections |>
  filter(year == projection_year) |>
  # filter(!state %in% states_abolished_parole$state) |>
  filter(!state %in% states_to_exclude$state) |>
  mutate(proj_pop_past_pey_rounded = fnc_round_to_power(proj_pop_past_pey),
         proj_pcnt_ppey_rounded = round(proj_pcnt_ppey, 0)) |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded)

# OPTION 1)
# Calculate the total projected population past parole eligibility (PE) across all states
proj_past_pe <- ncrp_projections |>
  filter(year == projection_year) |>
  summarise(past_pe_pop = sum(proj_pop_past_pey, na.rm = TRUE))

# Round the total projected population past PE to the nearest power
proj_past_pe_count_rounded <- proj_past_pe |>
  mutate(past_pe_pop_rounded = fnc_round_to_power(past_pe_pop)) |>
  pull(past_pe_pop_rounded)

# Extract the unrounded total projected population past PE for further calculations
proj_past_pe <- proj_past_pe |>
  pull(past_pe_pop)

# Calculate the total projected prison population for the specified projection year
proj_prison_pop <- ncrp_population_projections |>
  filter(year == projection_year) |>
  summarise(total_prison_pop = sum(total_prison_population, na.rm = TRUE)) |>
  pull(total_prison_pop)

# Calculate the ratio of total prison population to population past PE (1 in X individuals)
proj_past_pe_1_in_x <- round(proj_prison_pop/proj_past_pe, 0)

#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

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
gradient_colors <- c(gradient1, gradient2, gradient3, gradient4, blue)

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
               "Percentage of People: ", paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ", formattable::comma(proj_pop_past_pey_rounded, 0),
               "<br>Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>"),

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
      # state == "Louisiana" ~ "No Discretionary Parole",
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
      is.na(data_category) & abolished_parole == "Y" ~ "No Discretionary Parole",
      # state == "Louisiana" ~ "No Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,
      is.na(gradient_color) & data_category == "No Discretionary Parole" ~ "white",
      # state == "Louisiana" ~ "white",
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "No Discretionary Parole" ~ 5,
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
                 list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
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

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(enabled = FALSE, filename = "proj_past_parole_eligibility_2023") |>

  hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
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

# Add JavaScript to apply a gray border to the "No Discretionary Parole" legend item
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
                 list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
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

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(
    enabled = FALSE) |>

  hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
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

# Example of writing to this path
write.csv(parole_eligibility_table_download, file_path, row.names = FALSE)




