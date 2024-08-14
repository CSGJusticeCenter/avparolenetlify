

# Retrieve and process census data for a given state
fnc_get_census_data <- function(state) {
  df <-
    tidycensus::get_decennial(
      geography = "state",
      state = state,
      variables = race_vars,
      summary_var = "P3_001N",
      year = select_year,
      geometry = FALSE) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race = case_when(
        variable %in% c("estimate_american_indian",
                        "estimate_asian",
                        "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    )
  return(df)
}

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

rri_data <- merged_data %>%
  inner_join(reference_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate) %>%
  select(state, race, rri, incarceration_rate) |>
  mutate(incarceration_rate_10 = incarceration_rate/10)


# Get list of states
states <- unique(rri_data$state)

# Generate sentences dynamically
all_sentence_rri <- map(.x = states, .f = function(x) {

  df1 <- rri_data %>%
    filter(state == x)

  higher_rates <- df1 %>%
    filter(rri > 1 & race != "White, non-Hispanic") %>%
    mutate(sentence = paste0(race, " people are incarcerated in state prison at a rate <b>",
                             round(rri, 1), " times</b> higher")) %>%
    pull(sentence)

  if (length(higher_rates) > 0) {
    final_sentence <- paste0("In ", x, ", ", paste(higher_rates, collapse = " and "),
                             " than White, non-Hispanic people, when accounting for population sizes in the community.")
  } else {
    final_sentence <- paste0("In ", x, ", there are no disparities in prison incarceration rates compared to White, non-Hispanic individuals.")
  }

  return(final_sentence)
})

# Set names of the list to states
all_sentence_rri <- setNames(all_sentence_rri, states)

# Example output for Georgia
all_sentence_rri$Georgia


# Common circle size in pixels
circle_radius <- 4  # Adjust this value to make circles larger or smaller
num_columns <- 50   # Fixed number of columns for layout

calc_height <- function(num_items, columns, circle_radius) {
  num_rows <- ceiling(num_items / columns)
  height <- num_rows * (circle_radius*2)  # Adjust 4 as needed to increase spacing
  return(ifelse(height < 100, 100, height))  # Ensure minimum height of 100
}
# Get unique states
states <- unique(rri_data$state)

# Create Highcharts visualizations for each state
all_hc_waffle_rri_black <- map(.x = states, .f = function(x) {

  df2 <- rri_data |>
    ungroup() |>
    filter(state == x) |>
    select(-state, -rri) |>
    mutate(incarceration_rate = round(incarceration_rate, 1)) |>
    mutate(color = case_when(
      race == "Black, non-Hispanic" ~ color1,
      race == "Hispanic, any race" ~ color2,
      race == "Other race(s), non-Hispanic" ~ color3,
      race == "White, non-Hispanic" ~ color4
    ))

  # Black, non-Hispanic
  df1 <- df2 |> filter(race == "Black, non-Hispanic")
  rate_black <- df1$incarceration_rate[1]  # Adjust if there's more than one value
  height_black <- calc_height(rate_black, num_columns, circle_radius)

  highcharts <- highchart() |>
    hc_chart(type = "item") |>
    hc_title(text = glue("For every 100,000 Black people in the community, {rate_black} are in prison."),
             align = "left") |>
    hc_xAxis(categories = df1$race) |>
    hc_yAxis(title = list(text = "")) |>
    hc_series(
      list(
        name = "",
        data = lapply(1:round(rate_black), function(i) {
          list(
            y = 1,
            marker = list(symbol = "circle", radius = circle_radius)
          )
        }),
        type = "item",
        marker = list(radius = circle_radius),
        layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_black / num_columns), columns = num_columns)
      )
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_add_theme(base_hc_theme) |>
    hc_exporting(enabled = TRUE) |>
    hc_size(height = height_black) |>
    hc_tooltip(enabled = FALSE) |>
    hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
    hc_colors(c(color1))

  return(highcharts)
})

# Name the list of charts by state
all_hc_waffle_rri_black <- setNames(all_hc_waffle_rri_black, states)
all_hc_waffle_rri_black$Georgia

# Create Highcharts visualizations for each state
all_hc_waffle_rri_hispanic <- map(.x = states, .f = function(x) {

  df2 <- rri_data |>
    ungroup() |>
    filter(state == x) |>
    select(-state, -rri) |>
    mutate(incarceration_rate = round(incarceration_rate, 1)) |>
    mutate(color = case_when(
      race == "Black, non-Hispanic" ~ color1,
      race == "Hispanic, any race" ~ color2,
      race == "Other race(s), non-Hispanic" ~ color3,
      race == "White, non-Hispanic" ~ color4
    ))

  # Black, non-Hispanic
  df1 <- df2 |> filter(race == "Hispanic, any race")
  rate_hispanic <- df1$incarceration_rate[1]  # Adjust if there's more than one value
  height_hispanic <- calc_height(rate_hispanic, num_columns, circle_radius)

  highcharts <- highchart() |>
    hc_chart(type = "item") |>
    hc_title(text = glue("For every 100,000 Hispanic people in the community, {rate_hispanic} are in prison."),
             align = "left") |>
    hc_xAxis(categories = df1$race) |>
    hc_yAxis(title = list(text = "")) |>
    hc_series(
      list(
        name = "",
        data = lapply(1:round(rate_hispanic), function(i) {
          list(
            y = 1,
            marker = list(symbol = "circle", radius = circle_radius)
          )
        }),
        type = "item",
        marker = list(radius = circle_radius),
        layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_hispanic / num_columns), columns = num_columns)
      )
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_add_theme(base_hc_theme) |>
    hc_exporting(enabled = TRUE) |>
    hc_size(height = height_hispanic) |>
    hc_tooltip(enabled = FALSE) |>
    hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
    hc_colors(c(color4))

  return(highcharts)
})

# Name the list of charts by state
all_hc_waffle_rri_hispanic <- setNames(all_hc_waffle_rri_hispanic, states)
all_hc_waffle_rri_hispanic$Georgia

# Create Highcharts visualizations for each state
all_hc_waffle_rri_white <- map(.x = states, .f = function(x) {

  df2 <- rri_data |>
    ungroup() |>
    filter(state == x) |>
    select(-state, -rri) |>
    mutate(incarceration_rate = round(incarceration_rate, 1)) |>
    mutate(color = case_when(
      race == "Black, non-Hispanic" ~ color1,
      race == "Hispanic, any race" ~ color2,
      race == "Other race(s), non-Hispanic" ~ color3,
      race == "White, non-Hispanic" ~ color4
    ))

  # Black, non-Hispanic
  df1 <- df2 |> filter(race == "White, non-Hispanic")
  rate_white <- df1$incarceration_rate[1]  # Adjust if there's more than one value
  height_white <- calc_height(rate_white, num_columns, circle_radius)

  highcharts <- highchart() |>
    hc_chart(type = "item") |>
    hc_title(text = glue("For every 100,000 White people in the community, {rate_white} are in prison."),
             align = "left") |>
    hc_xAxis(categories = df1$race) |>
    hc_yAxis(title = list(text = "")) |>
    hc_series(
      list(
        name = "",
        data = lapply(1:round(rate_white), function(i) {
          list(
            y = 1,
            marker = list(symbol = "circle", radius = circle_radius)
          )
        }),
        type = "item",
        marker = list(radius = circle_radius),
        layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_white / num_columns), columns = num_columns)
      )
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_add_theme(base_hc_theme) |>
    hc_exporting(enabled = TRUE) |>
    hc_size(height = height_white) |>
    hc_tooltip(enabled = FALSE) |>
    hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
    hc_colors(c(color2))

  return(highcharts)
})

# Name the list of charts by state
all_hc_waffle_rri_white <- setNames(all_hc_waffle_rri_white, states)
all_hc_waffle_rri_white$Georgia

# Create Highcharts visualizations for each state
all_hc_waffle_rri_other <- map(.x = states, .f = function(x) {

  df2 <- rri_data |>
    ungroup() |>
    filter(state == x) |>
    select(-state, -rri) |>
    mutate(incarceration_rate = round(incarceration_rate, 1)) |>
    mutate(color = case_when(
      race == "Black, non-Hispanic" ~ color1,
      race == "Hispanic, any race" ~ color2,
      race == "Other race(s), non-Hispanic" ~ color3,
      race == "White, non-Hispanic" ~ color4
    ))

  # Black, non-Hispanic
  df1 <- df2 |> filter(race == "Other race(s), non-Hispanic")
  rate_other <- df1$incarceration_rate[1]  # Adjust if there's more than one value
  height_other <- calc_height(rate_other, num_columns, circle_radius)

  highcharts <- highchart() |>
    hc_chart(type = "item") |>
    hc_title(text = glue("For every 100,000 non-Hispanic people of other* races in the community, {rate_other} are in prison."),
             align = "left") |>
    hc_xAxis(categories = df1$race) |>
    hc_yAxis(title = list(text = "")) |>
    hc_series(
      list(
        name = "",
        data = lapply(1:round(rate_other), function(i) {
          list(
            y = 1,
            marker = list(symbol = "circle", radius = circle_radius)
          )
        }),
        type = "item",
        marker = list(radius = circle_radius),
        layoutAlgorithm = list(type = 'grid', rows = ceiling(rate_other / num_columns), columns = num_columns)
      )
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_add_theme(base_hc_theme) |>
    hc_exporting(enabled = TRUE) |>
    hc_size(height = height_other) |>
    hc_tooltip(enabled = FALSE) |>
    hc_plotOptions(series = list(marker = list(radius = circle_radius))) |>
    hc_colors(c(color3))

  return(highcharts)
})

# Name the list of charts by state
all_hc_waffle_rri_other <- setNames(all_hc_waffle_rri_other, states)
all_hc_waffle_rri_other$Georgia







#------ Years Spent in Prison After Parole Eligibility by Race and Ethnicity ------#

# Filter and prepare the data
all_parole_release_disparities <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0 &
           reltype == "Conditional release"
  ) |>
  filter(admtype == "New court commitment") |>
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))

# Get unique states
states <- unique(all_parole_release_disparities$state)

# Create Highcharts visualizations for each state
all_scatter_race_ped_release <- map(.x = states, .f = function(x) {

  df1 <- all_parole_release_disparities |>
    filter(state == x) |>
    select(time_between_ped_release, race)

  hc_accessibility_text <- paste0("Text TBD")

  highcharts <- hchart(df1, "scatter",
                       hcaes(x = race, y = time_between_ped_release, group = race)) |>
    hc_chart(type = "scatter") |>
    hc_colors(colors) |>
    hc_title(text = "Years Spent in Prison After Parole Eligibility Year by Race and Ethnicity") |>
    hc_xAxis(title = list(text = "")) |>
    hc_yAxis(title = list(text = ""), tickInterval = 1) |>
    hc_plotOptions(
      scatter = list(
        jitter = list(
          x = .25,
          y = .25
        ),
        marker = list(
          radius = 2,
          symbol = 'circle'
        ),
        tooltip = list(
          pointFormat = 'Time Between Parole Eligibility Year<br>and Release Year: {point.y:.0f}'
        ),
        showInLegend = FALSE
      )
    ) |>
    hc_colors(c(color1, color2, color4, color3)) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_exporting(enabled = TRUE)
  return(highcharts)
})

# Name the list of charts by state
all_scatter_race_ped_release <- setNames(all_scatter_race_ped_release, states)

# Display the chart for Georgia as an example
all_scatter_race_ped_release$Georgia



df1 <- all_parole_release_disparities |>
  filter(state == "Georgia") |>
  select(time_between_ped_release, race) |>
  group_by(race) |>
  summarise(total_years = sum(time_between_ped_release, na.rm = TRUE))













#------ Timing of Release by Offense Type, Race, and Ethnicity ------#

# Determine the timing of release categories: 1st year, 2nd year, etc
# Remove people with missing parole eligibility data and those released
# before their parole eligibility year
ncrp_time_between_ped_release <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr)) |>
  mutate(time_between_ped_release_category2 = case_when(
    time_between_ped_release == 0 ~ "First Year",
    time_between_ped_release == 1 ~ "Second Year",
    time_between_ped_release >= 2 ~ "Third Year or More"
  )) |>
  mutate(time_between_ped_release_category2 =
           factor(time_between_ped_release_category2,
                  levels = c("Third Year or More",
                             "Second Year",
                             "First Year")))


# Expand the data to include all combinations of categories, race, and year
races <- c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race")
levels <- c("Murder and Non-negligent Manslaughter", "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
            "Other Violent Offenses", "Property", "Public order", "Drugs", "Other or Unknown")
years <- c("First Year", "Second Year", "Third Year or More")

# Get unique states
states <- unique(ncrp_time_between_ped_release$state)

all_bubble_race_ped_release <- map(.x = states, .f = function(x) {

  df1 <- ncrp_time_between_ped_release |>
    filter(race != "Unknown" & race != "Other race(s), non-Hispanic") |>
    filter(state == x) |>
    group_by(state, fbi_index, race) |>
    count(time_between_ped_release_category2) |>
    mutate(prop = (n / sum(n)),
           prop = round(prop*100, 0)) |>
    arrange(desc(race)) |>
    ungroup() |>
    select(-state) |>
    rename(Level = fbi_index,
           Race = race,
           Year = time_between_ped_release_category2,
           Value = prop) |>
    droplevels() |>
    mutate(Year = factor(Year,
                         levels = c(
                           "First Year",
                           "Second Year",
                           "Third Year or More"
                         )))

  # Reverse the order of levels (crimes)
  y_levels <- rev(levels)

  # Create a new mapping of categories to numerical values for y-axis
  y_mapping <- setNames(seq_along(y_levels) - 1, y_levels)

  # Create a complete grid of all possible combinations of Level, Race, and Year
  complete_grid <- expand.grid(
    Level = y_levels,
    Race = races,
    Year = years
  )

  # Convert to a tibble
  complete_grid <- as_tibble(complete_grid)

  # Merge the complete grid with the original data
  df_complete <- complete_grid %>%
    left_join(df1, by = c("Level", "Race", "Year"))

  # Replace NA values in the Value column with zero
  df_complete <- df_complete %>%
    mutate(Value = replace_na(Value, 0))

  # Update y_value after merging
  df_complete$y_value <- y_mapping[df_complete$Level]

  # Create a mapping of years to numerical values for x-axis
  year_mapping <- setNames(seq_along(years) - 1, years)
  df_complete$x_value <- year_mapping[df_complete$Year] * length(races) + as.numeric(factor(df_complete$Race, levels = races)) - 1

  # Filter out entries with Value 0
  df_complete <- df_complete |> filter(Value > 0)

  # Create plot bands for years
  plot_bands <- list(
    list(from = -0.5, to = 2.5, color = "#EFEFEF", label = list(text = "First Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
    list(from = 2.5, to = 5.5, color = "white", label = list(text = "Second Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "regular"))),
    list(from = 5.5, to = 8.5, color = "#EFEFEF", label = list(text = "Third Year or More", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "regular")))
  )

  # Define colors for races
  color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color2, "Hispanic, any race" = color3)
  df_complete$color <- color_mapping[df_complete$Race]

  # Create the chart
  highcharts <- highchart() |>
    hc_chart(type = "bubble", marginTop = 70) |>
    # hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(
    #   formatter = JS("function() {
    #     return '<span style=\"font-weight: ' + (this.pos < 3 ? 'bold' : 'regular') + '\">' + this.value + '</span>';
    #   }")
    # )) |>
    hc_xAxis(
      categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(
        formatter = JS("function() {
      if (this.pos < 3) return '<span style=\"font-weight: bold;\">' + this.value + '</span>';
      return '';
    }"),
        style = list(
          fontWeight = "bold"
        )
      )
    ) |>
    hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
    hc_add_series(name = "", data = list_parse(data.frame(x = df_complete$x_value, y = df_complete$y_value, z = df_complete$Value, n = df_complete$n, color = df_complete$color, Race = df_complete$Race, Level = df_complete$Level, Year = df_complete$Year))) |>
    hc_title(text = "When are people released from prison after their parole eligibility year?") |>
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = '<b>Race:</b> {point.Race}<br><b>Level:</b> {point.Level}<br><b>Year:</b> {point.Year}<br><b>Percentage:</b> {point.z}%<br><b>Count:</b> {point.n}<br>') |>
    hc_plotOptions(bubble = list(
      maxSize = 50,
      sizeBy = "area",
      dataLabels = list(
        enabled = TRUE,
        format = '{point.z}%',
        style = list(
          color = "black",
          textOutline = "none",
          fontWeight = "regular"
        )
      )
    )) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_add_theme(base_hc_theme)

  return(highcharts)
})

# Name the list of charts by state
all_bubble_race_ped_release <- setNames(all_bubble_race_ped_release, states)

# Display the chart for Georgia as an example
all_bubble_race_ped_release$Georgia







#------ Time Served by Offense Type ------#

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
    hc_add_series(
      df_lines,
      type = 'line',
      hcaes(x = value, y = fbi_index_num, group = fbi_index),
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
              layout = "horizontal") |>
    hc_add_theme(base_hc_theme)

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
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(enabled = FALSE) |>
    hc_legend(enabled = FALSE) |>
    hc_size(height = 150)

  return(highcharts)
})

# Name the list of charts by state
all_lollipop_los_race <- setNames(all_lollipop_los_race, states)

# Display the chart for Georgia as an example
all_lollipop_los_race$Georgia





#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_parole_release_disparities, file = file.path(folder, "all_parole_release_disparities.rds"))
  save(all_scatter_race_ped_release,   file = file.path(folder, "all_scatter_race_ped_release.rds"))
  save(all_bubble_race_ped_release,    file = file.path(folder, "all_bubble_race_ped_release.rds"))

  save(all_sentence_rri,               file = file.path(folder, "all_sentence_rri.rds"))
  save(all_hc_waffle_rri_black,        file = file.path(folder, "all_hc_waffle_rri_black.rds"))
  save(all_hc_waffle_rri_hispanic,     file = file.path(folder, "all_hc_waffle_rri_hispanic.rds"))
  save(all_hc_waffle_rri_white,        file = file.path(folder, "all_hc_waffle_rri_white.rds"))
  save(all_hc_waffle_rri_other,        file = file.path(folder, "all_hc_waffle_rri_other.rds"))

  save(all_lollipop_los_race,          file = file.path(folder, "all_lollipop_los_race.rds"))
  save(all_scatter_los_race_offense,   file = file.path(folder, "all_scatter_los_race_offense.rds"))
}

