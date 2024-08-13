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



#------ Parole Board Members by State ------#

# Get parole status information by state
parole_info_by_state_clean <- parole_info_by_state |>
  select(state, abolished_discretionary_parole)

# Get number of parole board members
parole_board_members_select_states <- parole_info_by_state |>
  filter(abolished_discretionary_parole == "No") |>
  select(state, parole_board_members)

parole_board_members <- parole_info_by_state |>
  select(state, parole_board_members)

# Average number of parole board members
avg_parole_board_members_select_states <- mean(parole_board_members_select_states$parole_board_members)
parole_board_member_per_person <- sum(filtered_parole_elig_table_analysis_year$current_count, na.rm = TRUE)/sum(parole_board_members_select_states$parole_board_members)

# Set the number of surrounding dots
n <- 500#round(parole_board_member_per_person, 0)

# Calculate the number of rows and columns for the grid
grid_size <- ceiling(sqrt(n))

# Generate data for the surrounding dots using a grid pattern
x <- rep(seq(-grid_size, grid_size, length.out = grid_size), grid_size)
y <- rep(seq(-grid_size, grid_size, length.out = grid_size), each = grid_size)

# Select the first n points
x <- x[1:n]
y <- y[1:n]

# Normalize the coordinates to fit within a unit circle
max_r <- max(sqrt(x^2 + y^2))
x <- x / max_r
y <- y / max_r

# Create a data frame with the coordinates
data <- data.frame(x = c(0, x), y = c(0, y), size = c(2, rep(1, n)),
                   color = c(colors$red, rep(colors$darkgray, n)),
                   alpha = c(1, rep(0.5, n)))

# Adjust the alpha column to apply transparency only to gray circles
data$alpha[data$color == colors$red] <- 1

# Plot the graphic
square_dot_parole_graphic <- ggplot(data, aes(x, y, color = color, size = size, alpha = alpha)) +
  geom_point() +
  scale_color_identity() +  # Use the color column directly
  scale_size_identity() +   # Use the size column directly
  scale_alpha_identity() +  # Use the alpha column directly
  theme_void() +  # Remove axis and background
  theme(
    aspect.ratio = 1,  # Ensure the plot is square
    plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5)  # Center and format the title with ggtext
  ) +
  coord_fixed() +
  labs(title = "<span style='color:#d97d68;'>1 parole board member</span> per<br>500 people in prison<br>and eligible for parole")

square_dot_parole_graphic

# Save the combined map
ggsave(filename =  "square_dot_parole_graphic.png", plot = square_dot_parole_graphic,
       width  = 5, height = 5, dpi = 600)








#------ Parole Eligibility Table ------#

parole_eligibility_table <- filtered_parole_elig_table_analysis_year |>
  left_join(parole_info_by_state_clean, by = "state") |>
  left_join(parole_board_members, by = "state") |>
  mutate(ratio = paste0("1:", round(current_count/parole_board_members, 0))) |>
  mutate(ratio = ifelse(ratio == "1:NA", NA, ratio)) |>
  select(state, current_perc, current_count, filtered_total_pop, abolished_discretionary_parole, parole_board_members, ratio)












#------ Parole Eligibility Maps Data ------#

# Create a vector of all state names
all_states <- state.name

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
    current_perc           = current_perc * 100,
    future_1_5_years_perc  = future_1_5_years_perc * 100,
    missing_perc           = missing_perc * 100,

    state_abb = state.abb[match(state, state.name)],

    all_na = ifelse(is.na(current_count) & is.na(future_1_5_years_count) & is.na(missing_count), TRUE, FALSE),

    # Create tooltips
    tooltip = case_when(
      all_na == TRUE & abolished_discretionary_parole == "No" ~
        paste0("<b>", state, "<br><br>",
               "Parole eligibility data is not available.</b><br><br>",
               "Click on the state to view the state report."),

      all_na == TRUE & abolished_discretionary_parole == "Yes" ~
        paste0("<b>", state, "<br><br>",
               state, " abolished discretionary parole.</b><br><br>",
               "Click on the state to view the state report."),

      all_na == FALSE & abolished_discretionary_parole == "Yes" ~
        paste0("<b>", state, "<br><br>",
               state, " abolished discretionary parole.</b><br><br>",
               "Click on the state to view the state report."),

      all_na == FALSE & abolished_discretionary_parole == "No" ~
        paste0("<b>", state, "</b><br>",
               "<b>People in Prison Past Their Parole Eligibility Year</b><br>",
               "<table style='border-collapse: collapse; margin: 0; padding: 0;'>",
               "<tr><td style='padding-right: 5px; border: 1px solid white; margin: 0; padding: 0;'>- Proportion of the Prison Population:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
               paste0(round(current_perc, 0), "%</b></td></tr>",
                      "<tr><td style='border: 1px solid white; margin: 0; padding: 0;'>- Number of People in Prison:</td><td style='border: 1px solid white; margin: 0; padding: 0;'><b>",
                      paste(formattable::comma(current_count, 0), "</b></td></tr></table>",
                            "<span style='color: gray; font-weight: bold;'>Click on the state to view the state report.</span>")))
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
    all_na = ifelse(is.na(current_count) &
                      is.na(future_1_5_years_count) & is.na(missing_count), TRUE, FALSE),
    gradient_color = findInterval(current_perc, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(current_perc), NA, gradient_colors[gradient_color]),
    current_perc = round(current_perc, 0)
  )|>
  mutate(color = case_when(abolished_discretionary_parole == "Yes" ~ colors$yellow))

map_data_breaks$url <- paste0("https://avparoleproject.netlify.app/state_report_", tolower(gsub(" ", "_", map_data_breaks$state)))

# Adding a dummy column for value in the abolished discretionary parole series
map_data_breaks <- map_data_breaks |>
  mutate(dummy_value = ifelse(abolished_discretionary_parole == "Yes", 1, NA))

map_percent <- highchart() |>

  # Series for states with abolished discretionary parole
  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks |> filter(abolished_discretionary_parole == "Yes"),
    joinBy = "state_abb",
    value = "dummy_value",  # Using the dummy column as the value
    color = colors$yellow,
    borderColor = "#FFFFFF",  # Ensuring the outline is white
    borderWidth = 2,  # Outline width
    showInLegend = TRUE,
    name = "Abolished Discretionary Parole",
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state} has abolished discretionary parole.")
    )
  ) |>

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
    borderWidth = 2,  # Set the outline width
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      point = list(valueDescriptionFormat = "{point.state}, {point.currentperclabel}")),
    point = list(events = list(
      click = JS("function() { window.location.assign(this.url); }")
    )
    )
  ) |>

  hc_add_theme(hc_theme_map) |>

  hc_colorAxis(min = 0, max = max(map_data_breaks$current_perc)*1.2,
               stops = color_stops(n = 5, colors = gradient_colors),
               labels = list(
                 formatter = JS("function() { return this.value + '%'; }")
               )) |>

  # hc_legend(align = "left",
  #           verticalAlign = "top",
  #           layout = "horizontal",
  #           symbolWidth = 250,
  #           x = -7,
  #           title = list(text = "Pct. of People in Prison Past Their Parole Eligibility Year",
  #                        style = list(fontWeight = "regular",
  #                          fontSize = "12px"))
  # ) |>
  hc_legend(align = "left",
            x = -8,
            verticalAlign = "top",
            layout = "horizontal",
            itemStyle = list(
              fontWeight = "normal",
              fontSize = "12px"
            ),
            # Customizing the title for the gradient legend
            title = list(
              text = "Pct. of People in Prison Past Their Parole Eligibility Year",
              style = list(fontWeight = "normal", fontSize = "14px")
            ),
            # Customizing the legend for abolished discretionary parole
            useHTML = TRUE,
            labelFormatter = JS(paste0("
              function() {
                if (this.name === 'Abolished Discretionary Parole') {
                  return '<span style=\"background-color: white; font-weight: normal;", "; padding: 0 0px; border-radius: 3px;\">' + this.name + '</span>';
                } else {
                  return this.name;
                }
              }
            "))
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
      paste0("This map shows the proportion of people in prison who are past their parole eligibility year."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description = paste0("TEXT")))
  ) |>
  hc_title(text = paste0("People in Prison Past Their Parole Eligibility Year in ", analysis_year),
           align = "left")
map_percent



#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(map_percent,              file = file.path(folder, "map_percent.rds"))
  save(parole_eligibility_table, file = file.path(folder, "parole_eligibility_table.rds"))
}

