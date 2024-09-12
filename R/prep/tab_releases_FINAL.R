#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#######################################


#------ Prison Releases by Year ------#

# Prison releases by year
ncrp_releases_by_year <- ncrp_releases |>
  filter(rptyear >= 2010) |>
  group_by(state, rptyear) |>
  summarise(total = n())

states <- unique(ncrp_releases_by_year$state)

# Generate sentence for each state
all_sentence_releases <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- ncrp_releases_by_year %>% filter(state == x)

  # Find the earliest and latest year prison releases
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)
  earliest_year_release <- df1$total[df1$rptyear == earliest_year]
  latest_year_release <- df1$total[df1$rptyear == latest_year]

  # Calculate the percent change
  percent_change <- (latest_year_release - earliest_year_release) / earliest_year_release * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", prison releases ",
                      change_type, " ", percent_change_abs, "%, dropping from ",
                      format(earliest_year_release, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_release, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Set names for the list elements
all_sentence_releases <- setNames(all_sentence_releases, states)

# Check the sentence for Georgia
all_sentence_releases$Georgia



# Highchart by state since 2010
states <- unique(ncrp_releases_by_year$state)
all_line_releases_by_year <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_releases_by_year |>
    ungroup() |>
    filter(state == x) |>
    distinct()

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$total)*1.1
  min_value <- min(df1$total)/1.5

  hc_accessibility_text <- paste0("This graph shows the number of releases in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Releases by Year") |>
    hc_yAxis(title = list(text = ""),
             min = min_value,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "Releases",
        data = df1$total,
        # tooltip = list(
        #   pointFormat = "Year: {point.category}<br>Prison Releases: {point.y}"
        # )
        tooltip = list(
          pointFormat = "<b>Prison Releases:</b> {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color1))

  return(highcharts)
})
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
all_line_releases_by_year$Georgia





#------ Proportion of Parole-Eligible Population Released ------#


# We have the year-end population of those who were parole-eligible but were not released,
#   and we have the number of parole-eligible individuals who were released but
#   we don't have the total initial population of parole-eligible individuals for each year,
#   so, determine this below.

# Calculate the number of parole eligible people released by state and year
ncrp_pe_releases_by_year <- ncrp_releases |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_releases = n())

# Calculate the number of parole eligible people in prison by state and year
ncrp_pe_population_not_released_by_year <- ncrp_yearendpop |>
  filter(rptyear >= 2010) |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(total_parole_eligible_population_not_released = n())

# Merge data together
ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year |>
  left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) |>
  mutate(total_parole_eligible_population =
           total_parole_eligible_releases + total_parole_eligible_population_not_released,
         prop_parole_elgible_released =
           total_parole_eligible_releases/total_parole_eligible_population) |>
  select(state, rptyear,
         total_parole_eligible_population_not_released,
         total_parole_eligible_releases) |>
  pivot_longer(
    cols = c(total_parole_eligible_population_not_released, total_parole_eligible_releases),
    names_to = "status",
    values_to = "n"
  ) |>
  mutate(status = case_when(
    status == "total_parole_eligible_population_not_released" ~ "Not Released",
    status == "total_parole_eligible_releases" ~ "Released"
  ))



# Highchart stacked bar chart
states <- unique(ncrp_pe_proportion_released$state)
all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_proportion_released |>
    filter(state == x)
  jsFormatter <- JS("function() {
                   var total = this.point.stackTotal;
                   var percentage = Math.round((this.y / total) * 100);
                   return percentage + '%';
                  }")
  highcharts <- df1 |>
    hchart(
      type = "column",
      hcaes(x = rptyear, y = n, group = status)
    ) |>
    hc_yAxis(title = list(text = "")) |>
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 # dataLabels = list(enabled = TRUE,
                                 #                   style = list(textOutline = "none",
                                 #                                color = "white"),
                                 #                   formatter = jsFormatter),
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD accessibility text",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD accessibility text"))) |>
    hc_title(text = "Proportion of Parole-Eligible People Released from Prison by Year") |>
    hc_colors(c(color2, color1))
  return(highcharts)
})
all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
all_stackedbar_parole_eligibility_release$Georgia



# SENTENCE: In 2020, X% of people eligible for parole were released during their eligibility
#           year. This represents a X% decrease/increase compared YEAR.

states <- unique(ncrp_pe_proportion_released$state)
all_sentence_pe_proportion_released <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_proportion_released %>%
    filter(state == x) %>%
    group_by(rptyear, status) %>%
    summarize(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = status, values_from = total, values_fill = list(total = 0)) %>%
    mutate(proportion_released = Released / (Released + `Not Released`) * 100) %>%
    arrange(rptyear)

  # Handling case with only one year of data
  if (nrow(df1) == 1) {
    latest_year <- df1$rptyear[1]
    latest_value <- df1$proportion_released[1]
    sentence <- paste0(
      "In ", latest_year, ", ", round(latest_value, 1),
      "% of people eligible for parole were released during their eligibility year."
    )
    return(sentence)
  }

  # Handling missing data
  if (nrow(df1) < 2) {
    return(paste0("Insufficient data for ", x))
  }

  # Get the latest and earliest years
  latest_year <- tail(df1$rptyear, 1)
  earliest_year <- head(df1$rptyear, 1)

  # Get the values for the latest and earliest years
  latest_value <- tail(df1$proportion_released, 1)
  earliest_value <- head(df1$proportion_released, 1)

  # Calculate the percentage change
  percentage_change <- latest_value - earliest_value

  # Determine if the change is an increase or decrease
  change_type <- ifelse(percentage_change >= 0, "increase", "decrease")

  # Construct the sentence
  sentence <- paste0(
    "In ", latest_year, ", ", round(latest_value, 0),
    "% of people eligible for parole were released during their eligibility year. ",
    "This represents a ", round(abs(percentage_change), 0), "% ", change_type,
    " compared to ", earliest_year, "."
  )

  return(sentence)
})

all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
all_sentence_pe_proportion_released$Arizona
all_sentence_pe_proportion_released$Georgia



#------ Releases by Release Type ------#

# Filter to people with release type information
# Remove "Other releases" - although Alabama has 30% other releases
ncrp_release_type <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(reltype == "Conditional release" | reltype == "Unconditional release") |>
  mutate(reltype = case_when(reltype == "Conditional release" ~ "Conditional Release",
                             reltype == "Unconditional release" ~ "Unconditional Release",
                             TRUE ~ reltype)) |>
  group_by(state) |>
  count(reltype)

# Highchart pie chart showing releases by release type
states <- unique(ncrp_release_type$state)
all_pie_release_type <- map(.x = states, .f = function(x) {
  df1 <- ncrp_release_type |>
    ungroup() |>
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by release type (unconditional release vs. conditional
                                  release) in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- # Create a pie chart
    highchart() |>
    hc_chart(type = "pie") |>
    hc_title(text = "Proportion of Conditional vs. Unconditional Releases") |>
    hc_plotOptions(pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:1em">{point.name}: </span><br><span style="font-size:2em"><b>{point.percentage:.0f}%</b></span>'
      )
    )) |>
    hc_series(list(
      name = "Release Type",
      colorByPoint = TRUE,
      data = list_parse2(df1 |>
                           mutate(y = n) |>
                           select(name = reltype, y))
    )) |>
    hc_add_theme(base_hc_theme) |>
    hc_colors(c(color2, color3)) |>
    hc_exporting(enabled = TRUE) |>
    hc_tooltip(pointFormat = 'Number of Releases: {point.y}<br>Percentage of Releases: {point.percentage:.0f}%')
  return(highcharts)
})
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia





#------ Releases by Race, Ethnicity, Age, and Gender ------#






















#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_sentence_releases,                     file = file.path(folder, "all_sentence_releases.rds"))
  save(all_line_releases_by_year,                 file = file.path(folder, "all_line_releases_by_year.rds"))
  save(all_sentence_pe_proportion_released,       file = file.path(folder, "all_sentence_pe_proportion_released.rds"))
  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))
  save(all_pie_release_type,                      file = file.path(folder, "all_pie_release_type.rds"))
}

