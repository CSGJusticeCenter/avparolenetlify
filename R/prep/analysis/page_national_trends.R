#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################

hex_gj <- read_sf(file.path(sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(3857) |>
  sf_geojson() |>
  fromJSON(simplifyVector = FALSE)


#-----Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop_consolidated |>
  group_by(state, rptyear) |>
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment with sentences 1+ years but not life
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- fnc_filter_pe_population_criteria(ncrp_yearendpop_consolidated)

# Get total prison population for new court commitments and with sentences 1+ years but not life
filtered_pop_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(filtered_total_pop = n(), .groups = 'drop')

# Get number of people in prison by parole eligibility status for the specified criteria
# Get proportion of parole eligibility statuses out of everyone in the filtered population
filtered_parole_status_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear, parelig_status) |>
  summarise(count = n(), .groups = 'drop') |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  mutate(proportion = count / filtered_total_pop)

# Reshape data for table
filtered_parole_elig_table_by_year <- filtered_parole_status_by_year |>
  pivot_longer(cols = c(count, proportion), names_to = "metric", values_to = "value") |>
  mutate(metric_name = case_when(
    metric == "count" ~ paste(parelig_status, "count"),
    metric == "proportion" ~ paste(parelig_status, "perc.")
  )) |>
  select(state, rptyear, metric_name, value) |>
  pivot_wider(names_from = metric_name, values_from = value) |>
  clean_names()

# Filter to select analysis year specified in the config file
filtered_parole_elig_table_analysis_year_with_missing_states <- filtered_parole_elig_table_by_year |>
  filter(rptyear == select_year)

# Find missing states and combine with the original dataframe
missing_states <- tibble(state = setdiff(state.name, filtered_parole_elig_table_analysis_year_with_missing_states$state),
                         rptyear = select_year)

# Add missing states to table so we have a complete table of 50 states
filtered_parole_elig_table_analysis_year <- filtered_parole_elig_table_analysis_year_with_missing_states |>
  bind_rows(missing_states) |>
  full_join(total_pop_by_year, by = c("state", "rptyear")) |>
  full_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  arrange(state) |>
  select(state, rptyear, total_pop, filtered_total_pop,
         contains("current"), contains("future"), contains("missing")) |>
  filter(rptyear == select_year) |>
  mutate(current_perc           = current_perc * 100,
         # future_perc            = future_perc * 100,
         # missing_perc           = missing_perc * 100,
         current_count_rounded = fnc_round_to_power(current_count))



#-----Parole Board Members by State ------#

# Get parole status information by state
# Get number of parole board members
states_parole <- state_notes |>
  select(state, abolished_parole, members)



#-----Parole Eligibility Table ------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- filtered_parole_elig_table_analysis_year |>
  left_join(states_parole, by = "state") |>
  filter(abolished_parole == "N" | state == "Louisiana") |>
  mutate(current_perc = round(current_perc, 1)) |>
  select(state, current_perc, current_count_rounded, filtered_total_pop, members)


# Rename variables for downloadable table
parole_eligibility_table_download <- parole_eligibility_table |>
  select(State = state,
         `In Prison Past Parole Eligibility (N)` = current_count_rounded,
         `In Prison Past Parole Eligibility (%)` = current_perc,
         `Prison Population` = filtered_total_pop,
         `Parole Board Members` = members)





#-----Parole Eligibility Maps Data ------#

# Create a vector of all state names
all_states <- state.name

# Define the gradient colors for categories
gradient_colors <- c(green1, green2, green3, green4, blue)

# Prepare tooltips and map data
# Prepare data for national maps
map_data <- filtered_parole_elig_table_analysis_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(states_parole, by = "state") |>

  # Format data and create tooltip
  mutate(
    state_abb = state.abb[match(state, state.name)],

    all_na = ifelse(is.na(current_count)# & is.na(future_count) & is.na(missing_count)
                    , TRUE, FALSE),

    # Create tooltips
    tooltip = case_when(

      state == "Louisiana" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>",
               "Percentage of People: ",
               paste0(round(current_perc, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(current_count_rounded, 0)),

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
               paste0(round(current_perc, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(current_count_rounded, 0))
    ),

    tooltip = str_replace_all(tooltip, "NA%", "No Data"),
    tooltip = str_replace_all(tooltip, "NA", "No Data")
  ) |>

  # create data labels
  mutate(change_label = paste0(round(current_perc, 0), "%"),
         # change_label = str_replace_all(change_label, "NA%", "-"),
         change_label = str_replace_all(change_label, "NA%", " "),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))


# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0),
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
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
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
map_percent <- highchart(height = 600) |>

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
            x = 15,
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
            x = 15,
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

  # hc_exporting(
  #   enabled = TRUE,
  #   allowHTML = TRUE,
  #   filename = paste0(gsub(" ", "_", tolower("Map Past Parole Eligibility by State 2023"))),
  #   scale = 1,
  #   sourceWidth = 800,
  #   sourceHeight = 600) |>

  hc_exporting(
    enabled = FALSE) |>

  hc_caption(text = ncrp_csg_source,
             y = 0)

# Add JavaScript to apply a gray border to the "Abolished Discretionary Parole" legend item
map_percent_download <- onRender(map_percent_download, "
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

# Render the map
map_percent_download
map_percent

# Save map_percent_download as a temporary HTML file
saveWidget(map_percent_download, file = "temp.html", selfcontained = TRUE)

# Use webshot to take a screenshot and save it as a PNG
webshot2::webshot(
  url = "temp.html",
  file = file.path(app_folder, "map_percent_download.png"),
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
