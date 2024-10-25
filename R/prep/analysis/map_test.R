#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################


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
  select(state, abolished_discretionary_parole = abolished_parole, members)



#-----Parole Eligibility Table ------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- filtered_parole_elig_table_analysis_year |>
  left_join(states_parole, by = "state") |>
  filter(abolished_discretionary_parole == "N" | state == "Louisiana") |>
  mutate(current_perc = round(current_perc, 1)) |>
  select(state, current_perc, current_count_rounded, filtered_total_pop, members)


# Rename variables for downloadable table
parole_eligibility_table_download <- parole_eligibility_table |>
  select(State = state,
         `In Prison Past Parole Eligibility (N)` = current_count_rounded,
         `In Prison Past Parole Eligibility (%)` = current_perc,
         `Prison Population` = filtered_total_pop,
         `Parole Board Members` = members)


#------ Parole Eligibility Maps Data ------#

# Create a vector of all state names
all_states <- state.name

# Get parole status information by state
parole_info_by_state_clean <- parole_info_by_state |>
  select(state, abolished_discretionary_parole)

# Define the gradient colors for categories
gradient_colors <- c(colors$green1, colors$green2, colors$green3, colors$green4)

# Prepare data for national maps
map_data <- filtered_parole_elig_table_analysis_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(parole_info_by_state_clean, by = "state") |>

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

      all_na == TRUE & abolished_discretionary_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Parole eligibility data is not available.<br>"),

      all_na == TRUE & abolished_discretionary_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_discretionary_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_discretionary_parole == "N" ~
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
         change_label = str_replace_all(change_label, "NA%", "-"),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))

# Define the gradient colors for categories
gradient_colors <- c(colors$green1, colors$green2, colors$green3, colors$green4)  # Adjusted colors to match the example gradient

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    all_na = ifelse(is.na(current_count) & is.na(missing_count), TRUE, FALSE),
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0)
  )

map_percent <- highchart() |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "current_perc",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.change_label + '</span>' + '</div>';}")),
    nullColor = colors$lightgray,
    borderColor = "#FFFFFF",  # Set the outline color to white
    borderWidth = 2) |>

  hc_add_theme(hc_theme_map) |>
  hc_colorAxis(min = 0, max = max(map_data_breaks$current_perc)*1.2,
               stops = color_stops(n = 5, colors = gradient_colors),
               labels = list(
                 formatter = JS("function() { return this.value + '%'; }")
               )) |>

  hc_legend(align = "left",
            verticalAlign = "top",
            layout = "horizontal",
            symbolWidth = 250,
            x = -7,
            title = list(text = "Pct. of People in Prison Past Their Parole Eligibility Date",
                         style = list(fontWeight = "regular",
                                      fontSize = "12px"))
  ) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

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
      paste0("This map shows the proportion of people in prison who are past their parole eligibility date."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description = paste0("TEXT")))
  ) |>
  hc_title(text = paste0("People in Prison Past Their Parole Eligibility Date in ", analysis_year),
           align = "left")

map_percent


##################################################################################


library(sf)
library(highcharter)
library(jsonlite)

states_abolished_parole <- states_parole |>
  filter(abolished_discretionary_parole == "Y") |>
  mutate(state_abb = state.abb[match(state, state.name)])

# Read and preprocess the shapefile
hex_gj <- read_sf(file.path(sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  # Exclude states with abolished discretionary parole
  filter(!state_abb %in% states_abolished_parole$state_abb) |>
  st_transform(3857) |>
  sf_geojson() |>
  fromJSON(simplifyVector = FALSE)

# Create the map with the filtered data
map_percent <- highchart() |>
  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "current_perc",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.change_label + '</span>' + '</div>';}")),
    nullColor = darkgray,
    borderColor = "#FFFFFF",  # Set the outline color to white
    borderWidth = 2) |>

  hc_add_theme(hc_theme_map) |>
  hc_colorAxis(min = 0, max = max(map_data_breaks$current_perc)*1.2,
               stops = color_stops(n = 5, colors = gradient_colors),
               labels = list(
                 formatter = JS("function() { return this.value + '%'; }")
               )) |>

  hc_legend(align = "left",
            verticalAlign = "top",
            layout = "horizontal",
            symbolWidth = 250,
            x = -7,
            title = list(text = "Pct. of People in Prison Past Their Parole Eligibility Date",
                         style = list(fontWeight = "regular",
                                      fontSize = "12px"))
  ) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

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
      paste0("This map shows the proportion of people in prison who are past their parole eligibility date."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description = paste0("TEXT")))
  ) |>
  hc_title(text = paste0("People in Prison Past Their Parole Eligibility Date in ", analysis_year),
           align = "left")

# Display the map
map_percent
