


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

# Create Highcharts visualizations for each state
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

  # Create plot bands for years
  plot_bands <- list(
    list(from = -0.5, to = 2.5, color = "lightgray", label = list(text = "First Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
    list(from = 2.5, to = 5.5, color = "white", label = list(text = "Second Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
    list(from = 5.5, to = 8.5, color = "lightgray", label = list(text = "Third Year or More", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold")))
  )

  # Define colors for races
  color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color4, "Hispanic, any race" = color3)
  df_complete$color <- color_mapping[df_complete$Race]

  # Create the chart
  highcharts <- highchart() |>
    hc_chart(type = "bubble", marginTop = 70) |>
    hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(style = list(fontWeight = "bold"))) |>
    hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
    hc_add_series(name = "", data = list_parse(data.frame(x = df_complete$x_value, y = df_complete$y_value, z = df_complete$Value, n = df_complete$n, color = df_complete$color, Race = df_complete$Race, Level = df_complete$Level, Year = df_complete$Year))) |>
    hc_title(text = "Proportion of People Released by Year of Parole Eligibility and Offense Type") |>
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
          fontWeight = "bold"
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














#------ Years Spent in Prison After Parole Eligibility by Race and Ethnicity ------#

# Filter and prepare the data
parole_release_disparities <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0 &
           reltype == "Conditional release") |>
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
states <- unique(parole_release_disparities$state)

# Create Highcharts visualizations for each state
all_scatter_race_ped_release <- map(.x = states, .f = function(x) {

  df1 <- parole_release_disparities |>
    filter(state == x) |>
    select(time_between_ped_release, race)

  hc_accessibility_text <- paste0("Text TBD")

  highcharts <- hchart(df1, "scatter",
                       hcaes(x = race, y = time_between_ped_release, group = race)) |>
      hc_chart(type = "scatter") |>
      hc_colors(colors) |>
      hc_title(text = "Years Spent in Prison After Parole Eligibility by Race and Ethnicity") |>
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
      hc_colors(c(color1, color2, color3, color4)) |>
      hc_add_theme(hc_theme_with_line) |>
    hc_exporting(enabled = TRUE)
    return(highcharts)
})

# Name the list of charts by state
all_scatter_race_ped_release <- setNames(all_scatter_race_ped_release, states)

# Display the chart for Georgia as an example
all_scatter_race_ped_release$Georgia



#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_scatter_race_ped_release, file = file.path(folder, "all_scatter_race_ped_release.rds"))
  save(all_bubble_race_ped_release,  file = file.path(folder, "all_bubble_race_ped_release.rds"))
}

