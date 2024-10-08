#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################


#------ Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment with sentences 1+ years but not life
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- fnc_filter_pe_population_criteria(ncrp_yearendpop)

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
  filter(rptyear == select_year)



#------ Parole Board Members by State ------#

# Get parole status information by state
state_notes_clean <- state_notes |>
  select(state, abolished_parole)

# Get number of parole board members
# members_select_states <- state_notes |>
#   filter(abolished_parole == "N") |>
#   select(state, members)
members <- state_notes |>
  select(state, members)


#------ Parole Eligibility Table ------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- filtered_parole_elig_table_analysis_year |>
  left_join(state_notes_clean, by = "state") |>
  left_join(members, by = "state") |>
  filter(abolished_parole == "N" | state == "Louisiana") |>
  select(state, current_perc, current_count, filtered_total_pop, members)

# Rename variables for downloadable table
parole_eligibility_table_download <- parole_eligibility_table |>
  mutate(current_count_rounded = fnc_round_to_power(current_count),
         current_perc = round(current_perc*100, 1)) |>
  select(State = state,
         `In Prison Past Parole Eligibility (N)` = current_count_rounded,
         `In Prison Past Parole Eligibility (%)` = current_perc,
         `Prison Population` = filtered_total_pop,
         `Parole Board Members` = members)





#------ Parole Eligibility Maps Data ------#

# Create a vector of all state names
all_states <- state.name

# Define the gradient colors for categories
gradient_colors <- c(green1, green2, green3, green4, blue)

# Prepare data for national maps
map_data <- filtered_parole_elig_table_analysis_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(state_notes_clean, by = "state") |>

  # Format data and create tooltip
  mutate(
    current_count_rounded  = fnc_round_to_power(current_count),
    current_perc           = current_perc * 100,
    future_perc            = future_perc * 100,
    missing_perc           = missing_perc * 100,

    state_abb = state.abb[match(state, state.name)],

    all_na = ifelse(is.na(current_count) & is.na(future_count) & is.na(missing_count), TRUE, FALSE),

    # Create tooltips
    tooltip = case_when(

      state == "Louisiana" ~
        paste0("<b>", state, "</b><br>",
               "Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>",
               "<b>People in Prison Past Their Parole Eligibility</b><br>",
               "<span style='color: gray; font-weight: bold;'>2023 Projection</span>",
               "<table style='border-collapse: collapse; margin: 0; padding: 0;'>",
               "<tr><td style='padding-right: 5px; border: 1px solid white; margin: 0; padding: 0;'>- Percentage of the Prison Population:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
               paste0(round(current_perc, 0), "%</b></td></tr>",
                      "<tr><td style='border: 1px solid white; margin: 0; padding: 0;'>- Number of People:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
                      paste(formattable::comma(current_count_rounded, 0), "</b></td></tr></table>",
                            "<span style='color: gray; font-style: italic;'>Click on the state to view the state report.</span>"))),

      all_na == TRUE & abolished_parole == "N" ~
        paste0("<b>", state, "</b><br>",
               "Parole eligibility data is not available.<br>"),

      all_na == TRUE & abolished_parole == "Y" ~
        paste0("<b>", state, "</b><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "Y" ~
        paste0("<b>", state, "</b><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "N" ~
        paste0("<b>", state, "</b><br>",
               "<b>People in Prison Past Their Parole Eligibility</b><br>",
               "<span style='color: gray; font-weight: bold;'>2023 Projection</span>",
               "<table style='border-collapse: collapse; margin: 0; padding: 0;'>",
               "<tr><td style='padding-right: 5px; border: 1px solid white; margin: 0; padding: 0;'>- Percentage of the Prison Population:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
               paste0(round(current_perc, 0), "%</b></td></tr>",
                      "<tr><td style='border: 1px solid white; margin: 0; padding: 0;'>- Number of People:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
                      paste(formattable::comma(current_count_rounded, 0), "</b></td></tr></table>",
                            "<span style='color: gray; font-style: italic;'>Click on the state to view the state report.</span>")))
    ),

    tooltip = str_replace_all(tooltip, "NA%", "No Data"),
    tooltip = str_replace_all(tooltip, "NA", "No Data")
  ) |>

  # create data labels
  mutate(change_label = paste0(round(current_perc, 0), "%"),
         change_label = str_replace_all(change_label, "NA%", "-"),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))

# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# # Process map_data to include gradient color and data category
# map_data_breaks <- map_data |>
#   mutate(
#     all_na = ifelse(is.na(current_count) &
#                       is.na(future_count) & is.na(missing_count), TRUE, FALSE),
#     gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
#     gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
#     current_perc = round(current_perc, 0)
#   ) |>
#   mutate(color = case_when(abolished_parole == "Y" ~ yellow),
#          url = ifelse(abolished_parole == "Y" | !is.na(current_perc), NA,
#                       paste0("https://avparoleproject.netlify.app/state_report_",
#                              tolower(gsub(" ", "_", map_data_breaks$state)))),
#          dummy_value = ifelse(abolished_parole == "Y", 1, NA))
#
# # Select data
# map_data <- map_data_breaks |>
#   select(state, state_abb, current_perc, change_label, abolished_parole, tooltip)
#
# # Define the gradient colors for categories
# gradient_colors <- c(green1, green2, green3, green4, blue)
#
# # Calculate the breaks for the percent of people eligible for parole
# num_breaks <- length(gradient_colors) - 1
# breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
# breaks[1] <- 0  # Set the first break to 0
# breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
# breaks <- cummax(breaks)  # Ensure breaks are strictly increasing
#
# # Process map_data to include gradient color and data category
# map_data_breaks <- map_data |>
#   mutate(
#     gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
#     gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
#     current_perc = round(current_perc, 0),
#     data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
#   ) |>
#   group_by(gradient_color) |>
#   mutate(
#     data_category = case_when(
#       state == "Louisiana" ~ "Abolished Parole",
#       gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
#       gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
#       gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
#       gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
#       gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
#     ),
#     data_category = case_when(
#       is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
#       is.na(data_category) & abolished_parole == "Y" ~ "Abolished Parole",
#       state == "Louisiana" ~ "Abolished Parole",
#       TRUE ~ data_category
#     ),
#     gradient_color = case_when(
#       is.na(gradient_color) & data_category == "Missing Data" ~ lightgray,
#       is.na(gradient_color) & data_category == "Abolished Parole" ~ yellow,
#       state == "Louisiana" ~ yellow,
#       TRUE ~ gradient_color
#     ),
#     data_category_num = case_when(
#       is.na(data_category_num) & data_category == "Missing Data" ~ 6,
#       is.na(data_category_num) & data_category == "Abolished Parole" ~ 5,
#       state == "Louisiana" ~ 5,
#       TRUE ~ data_category_num
#     )
#   )
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
      state == "Louisiana" ~ "Abolished Parole",
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$current_perc, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
      is.na(data_category) & abolished_parole == "Y" ~ "Abolished Parole",
      state == "Louisiana" ~ "Abolished Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ lightgray,
      is.na(gradient_color) & data_category == "Abolished Parole" ~ yellow,
      state == "Louisiana" ~ yellow,
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "Abolished Parole" ~ 5,
      state == "Louisiana" ~ 5,
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
                      formatter = JS("function() {return '<div style=\"text-align:center;\">' +
                            '<span style=\"font-weight:bold; font-size: 14px; text-align:center;\">' + this.point.state_abb + '</span><br>' +
                            '<span style=\"font-weight:normal; font-size: 14px; text-align:center;\">' + this.point.change_label + '</span>' + '</div>';}"),
                      textOutline = "none",
                      y = 0),
    nullColor = lightgray,
    borderColor = "#FFFFFF",
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}"))) |>

  hc_colorAxis(dataClassColor="category",
               dataClasses = list(
                 list(from = 1, to = 1, color = green1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = green2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = green3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = green4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = yellow, name = "Abolished Parole"),
                 list(from = 6, to = 6, color = lightgray, name = "Missing Data")
               )) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 10,
            y = -40) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = 'rgba(255, 255, 255, 1)',
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          this.point.tooltip +
          '</div>';}"
    ),
    useHTML = TRUE
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
      paste0("This map shows the proportion of people in prison who are past their parole eligibility year."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description = paste0("TEXT")))
  ) |>

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

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center") |>

  hc_exporting(
    enabled = TRUE,
    scale = 1,  # Ensure the exported image matches screen size exactly
    sourceWidth = 800,  # Set the width same as screen
    sourceHeight = 600,  # Set the height same as screen
    chartOptions = list(
      plotOptions = list(
        series = list(
          dataLabels = list(
            align = "center",
            verticalAlign = "middle",
            style = list(
              fontWeight = "bold",
              fontSize = "14px",
              textOutline = "none",
              align = "center"
            )
          )
        )
      )
    )
  ) |>
  hc_caption(text = ncrp_source,
             y = -10)
map_percent


#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(map_percent,                       file = file.path(folder, "map_percent.rds"))
  save(parole_eligibility_table,          file = file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_download, file = file.path(folder, "parole_eligibility_table_download.rds"))
}

