#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizatons for national trends page
#######################################


#------ Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment 1-25 year sentence lengths
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- ncrp_yearendpop |>
  filter(admtype == "New court commitment",
         sentlgth %in% c("1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years"))

# Get total prison population for new court commitments and sentence length 1-25 years
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
  filter(rptyear == analysis_year)

# Find missing states and combine with the original dataframe
missing_states <- tibble(state = setdiff(state.name, filtered_parole_elig_table_analysis_year_with_missing_states$state),
                         rptyear = analysis_year)

# Add missing states to table so we have a complete table of 50 states
filtered_parole_elig_table_analysis_year <- filtered_parole_elig_table_analysis_year_with_missing_states |>
  bind_rows(missing_states) |>
  left_join(total_pop_by_year, by = c("state", "rptyear")) |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  arrange(state) |>
  select(state, rptyear, total_pop, filtered_total_pop,
         contains("current"), contains("future_1_5_years"), contains("future_6_years"), contains("missing"))



#------ Parole Eligibility Maps (%) ------#

# Create a vector of all state names
all_states <- state.name

# Get parole status information by state
parole_info_by_state_clean <- parole_info_by_state |>
  select(state, abolished_discretionary_parole)

# Prepare data for national maps
map_data <- filtered_parole_elig_table_analysis_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(parole_info_by_state_clean, by = "state") |>

  # format data and create tooltip
  mutate(current_perc           = current_perc*100,
         future_1_5_years_perc  = future_1_5_years_perc*100,
         missing_perc           = missing_perc*100,

         state_abb = state.abb[match(state, state.name)],

         all_na = ifelse(is.na(current_count) &
                           is.na(future_1_5_years_count) & is.na(missing_count), TRUE, FALSE),

         # create tooltips
         tooltip = case_when(all_na == TRUE & abolished_discretionary_parole == "No" ~
                               paste0("<b>", state, "</b><br><br>",
                                      "Parole eligibility data is not available.<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(total_pop, digits = 0),
                                      "<br><br><i>Click on the state to view the state report.</i></b>"),

                             all_na == TRUE & abolished_discretionary_parole == "Yes" ~
                               paste0("<b>", state, "</b><br><br>",
                                      state, " abolished discretionary parole.<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(total_pop, digits = 0),
                                      "<br><br><i>Click on the state to view the state report.</i></b>"),

                             all_na == FALSE & abolished_discretionary_parole == "Yes" ~
                               paste0("<b>", state, "</b><br><br>",
                                      state, " abolished discretionary parole.<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(total_pop, digits = 0),
                                      "<br><br><i>Click on the state to view the state report.</i></b>"),

                             all_na == FALSE & abolished_discretionary_parole == "No" ~
                               paste0("<b>", state, "</b><br><br>",
                                      "Number of People in Prison<br> Currently Eligible for Parole Release:<br><b>",
                                      paste(formattable::comma(current_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Number of People in Prison<br> with Missing Parole Eligibility Data:<br><b>",
                                      paste(formattable::comma(missing_count, 0), "</b>", sep = ""), "<br><br>",
                                      "Total Prison Population:<br><b>",
                                      formattable::comma(total_pop, digits = 0),
                                      "<br><br><i>Click on the state to view the state report.</i></b>")
         ),

         tooltip = str_replace_all(tooltip, "NA%", "No Data"),
         tooltip = str_replace_all(tooltip, "NA", "No Data")) |>

  # create data labels
  mutate(change_label = paste0(round(current_perc, 0), "%"),
         change_label = str_replace_all(change_label, "NA%", "-"),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))

# Define the gradient colors for categories
gradient_colors <- c(colors$green1, colors$green2, colors$green3, colors$green4)

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    all_na = ifelse(is.na(current_count) &
                      is.na(future_1_5_years_count) & is.na(missing_count), TRUE, FALSE),
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) |>
  group_by(gradient_color) |>
  mutate(
    data_category = case_when(
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & all_na & abolished_discretionary_parole == "No" ~ "Missing Data",
      is.na(data_category) & abolished_discretionary_parole == "Yes" ~ "Abolished Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ colors$lightgray,
      is.na(gradient_color) & data_category == "Abolished Discretionary Parole" ~ colors$brown,
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "Abolished Discretionary Parole" ~ 5,
      abolished_discretionary_parole == "Yes" ~ 5,
      TRUE ~ data_category_num
    )
  )


map_data_breaks$url <- paste0("https://avparoleproject.netlify.app/state_report_", tolower(gsub(" ", "_", map_data_breaks$state)))


map_percent <- highchart(#height = 600
) |>

  hc_chart(marginTop = 60,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.change_label + '</span>' + '</div>';}")),
    nullColor = colors$lightgray,
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}")),
    point = list(events = list(
      click = JS("function() { window.location.assign(this.url); }")
    ))
  ) |>

  hc_colorAxis(dataClassColor="category",
               dataClasses = list(list(from = 1, to = 1, color=colors$green1, name = "0 - 3%"),
                                  list(from = 2, to = 2, color=colors$green2, name = "4 - 12%"),
                                  list(from = 3, to = 3, color=colors$green3, name = "13 - 20%"),
                                  list(from = 4, to = 4, color=colors$green4, name = "21 - 39%"),
                                  list(from = 5, to = 5, color=colors$yellow, name = "Abolished Discretionary Parole"),
                                  list(from = 6, to = 6, color=colors$lightgray, name = "Missing Data")
               )) |>

  hc_legend(align = "center",
            verticalAlign = "top",
            layout = "horizontal",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 10,
            y = -10
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
          '<div style=\"text-align:center;\">' +
          '<span style=\"font-weight:bold; font-size: 16px;\">' + this.point.state_abb + '</span><br>' +
          '<span style=\"font-weight:normal; font-size: 12px;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

  hc_add_theme(hc_theme_map) |>

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
  )

map_percent

#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(map_percent, file = file.path(folder, "map_percent.rds"))
}





# Load the shapefile
hex_gj <- read_sf(paste0(config$sp_data_path, "/data/raw/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(3857)

# Convert to a data frame for manipulation
hex_gj_df <- st_as_sf(hex_gj) |>
  mutate(state = state.name[match(state_abb, state.abb)])

# Convert parole_info_by_state_clean to a data frame
parole_info_by_state_clean <- as.data.frame(parole_info_by_state_clean)

# Join with hex data
hex_data <- hex_gj_df |>
  left_join(parole_info_by_state_clean, by = "state")

# Define colors
highlight_colors <- c("Yes" = colors$yellow, "No" = colors$green2)
other_color <- "white"

# Base plot with gray tiles
base_plot <- ggplot() +
  geom_sf(data = hex_data, aes(geometry = geometry), fill = other_color, color = "black") +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "Graphik")
  )

# Map for states that have not abolished discretionary parole
map1 <- base_plot +
  geom_sf(data = hex_data |>
            filter(abolished_discretionary_parole == "No"),
          aes(fill = abolished_discretionary_parole), color = "black") +
  scale_fill_manual(values = highlight_colors) +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )

# Map for states that have abolished discretionary parole
map2 <- base_plot +
  geom_sf(data = hex_data |>
            filter(abolished_discretionary_parole == "Yes"),
          aes(fill = abolished_discretionary_parole), color = "black") +
  scale_fill_manual(values = highlight_colors) +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )

# Arrange the two maps side by side
combined_map <- cowplot::plot_grid(map1, map2, ncol = 2)

# Save the combined map
ggsave(filename =  "combined_map_high_res.png", plot = combined_map, width = 20, height = 10, dpi = 300)



