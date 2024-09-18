#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#######################################


# ---------------------------------------------------------------------------- #
# Prison Release Trends
# ---------------------------------------------------------------------------- #

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
                      change_type, " ", percent_change_abs, " percent, dropping from ",
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
    hc_colors(c(color5))

  return(highcharts)
})
all_line_releases_by_year <- setNames(all_line_releases_by_year, states)
all_line_releases_by_year$Georgia

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

# # Merge data together
# ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year |>
#   left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) |>
#   mutate(total_parole_eligible_population =
#            total_parole_eligible_releases + total_parole_eligible_population_not_released,
#          prop_parole_elgible_released =
#            total_parole_eligible_releases/total_parole_eligible_population) |>
#   select(state, rptyear,
#          total_parole_eligible_population_not_released,
#          total_parole_eligible_releases) |>
#   pivot_longer(
#     cols = c(total_parole_eligible_population_not_released, total_parole_eligible_releases),
#     names_to = "status",
#     values_to = "n"
#   ) |>
#   mutate(status = case_when(
#     status == "total_parole_eligible_population_not_released" ~ "Not Released",
#     status == "total_parole_eligible_releases" ~ "Released"
#   ))
#
#
#
# # Highchart stacked bar chart
# states <- unique(ncrp_pe_proportion_released$state)
# all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
#   df1 <- ncrp_pe_proportion_released |>
#     filter(state == x)
#   jsFormatter <- JS("function() {
#                    var total = this.point.stackTotal;
#                    var percentage = Math.round((this.y / total) * 100);
#                    return percentage + '%';
#                   }")
#   highcharts <- df1 |>
#     hchart(
#       type = "column",
#       hcaes(x = rptyear, y = n, group = status)
#     ) |>
#     hc_yAxis(title = list(text = "")) |>
#     hc_xAxis(categories = unique(df1$rptyear),
#              title = "") |>
#     hc_add_theme(hc_theme_with_line) |>
#     hc_legend(enabled = TRUE) |>
#     hc_exporting(enabled = TRUE) |>
#     hc_plotOptions(series = list(stacking = "normal",
#                                  animation = FALSE,
#                                  cursor = "pointer",
#                                  # dataLabels = list(enabled = TRUE,
#                                  #                   style = list(textOutline = "none",
#                                  #                                color = "white"),
#                                  #                   formatter = jsFormatter),
#                                  borderWidth = 3,
#                                  minPointLength = 4),
#                    accessibility = list(enabled = TRUE,
#                                         keyboardNavigation = list(enabled = TRUE),
#                                         linkedDescription = "TBD accessibility text",
#                                         landmarkVerbosity = "one"),
#                    area = list(accessibility = list(description = "TBD accessibility text"))) |>
#     hc_title(text = "Proportion of Parole-Eligible People Released from Prison by Year") |>
#     hc_colors(c(color3, color5))
#   return(highcharts)
# })
# all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
# all_stackedbar_parole_eligibility_release$Georgia

# Merge data together
ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year |>
  left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) |>
  mutate(total_parole_eligible_population =
           total_parole_eligible_releases + total_parole_eligible_population_not_released,
         prop_parole_eligible_released =
           total_parole_eligible_releases / total_parole_eligible_population,
         prop_parole_eligible_not_released =
           total_parole_eligible_population_not_released / total_parole_eligible_population) |>
  select(state, rptyear,
         prop_parole_eligible_not_released,
         prop_parole_eligible_released) |>
  pivot_longer(
    cols = c(prop_parole_eligible_not_released, prop_parole_eligible_released),
    names_to = "status",
    values_to = "proportion"
  ) |>
  mutate(status = case_when(
    status == "prop_parole_eligible_not_released" ~ "Not Released",
    status == "prop_parole_eligible_released" ~ "Released"
  ))

# Highchart stacked bar chart
states <- unique(ncrp_pe_proportion_released$state)
all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_proportion_released |>
    filter(state == x)
  highcharts <- df1 |>
    hchart(
      type = "column",
      hcaes(x = rptyear, y = proportion, group = status)
    ) |>
    hc_yAxis(title = list(text = ""),
             max = 1,
             labels = list(formatter = JS("function() { return (this.value * 100) + '%'; }"))) |>
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD accessibility text",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD accessibility text"))) |>
    hc_title(text = "Proportion of Parole-Eligible People Released from Prison by Year") |>
    hc_colors(c(color3, color5))
  return(highcharts)
})
all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
all_stackedbar_parole_eligibility_release$Georgia


# SENTENCE: In 2020, X% of people eligible for parole were released during their eligibility
#           year. This represents a X% decrease/increase compared YEAR.

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
      " percent of people eligible for parole were released during their eligibility year."
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
    " percent of people eligible for parole were released during their eligibility year. ",
    "This represents a ", round(abs(percentage_change), 0), " percent ", change_type,
    " compared to ", earliest_year, "."
  )

  return(sentence)
})

all_sentence_pe_proportion_released <- setNames(all_sentence_pe_proportion_released, states)
all_sentence_pe_proportion_released$Arizona
all_sentence_pe_proportion_released$Georgia



# ---------------------------------------------------------------------------- #
# RELEASE TYPE
# ---------------------------------------------------------------------------- #

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





# ---------------------------------------------------------------------------- #
# DEMOGRAPHICS
# ---------------------------------------------------------------------------- #


prison_releases_race <- ncrp_releases |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(race)& race != "Unknown") |>
  count(race) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

prison_releases_sex <- ncrp_releases |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(sex)& sex != "Unknown") |>
  count(sex) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

prison_releases_agerlse <- ncrp_releases |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(agerlse) & agerlse != "Unknown") |>
  count(agerlse) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate graph for each state
states <- unique(prison_releases_race$state)
all_bar_releases_race <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_race |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Race and Ethnicity:</b> ", race, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by race and ethnicity in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "race", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5))
  return(highcharts)
})
all_bar_releases_race <- setNames(all_bar_releases_race, states)
all_bar_releases_race$Georgia


# Generate sentence for each state
states <- unique(prison_releases_race$state)
all_sentence_releases_race <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_race |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", most people released from prison were ",
                      df1$race, " people, representing ", round(df1$prop*100, 0), " percent of prison releases.")
  return(sentences)
})

all_sentence_releases_race <- setNames(all_sentence_releases_race, states)
all_sentence_releases_race$Georgia

# Generate graph for each state
states <- unique(prison_releases_sex$state)
all_bar_releases_sex <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_sex |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Sex:</b> ", sex, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by sex in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sex", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Sex") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE)|>
    hc_colors(c(color5))
  return(highcharts)
})
all_bar_releases_sex <- setNames(all_bar_releases_sex, states)
all_bar_releases_sex$Georgia


# Generate sentence for each state
states <- unique(prison_releases_sex$state)
all_sentence_releases_sex <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_sex |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", most people released from prison were ",
                      tolower(df1$sex), "s, representing ", round(df1$prop*100, 0), " percent of prison releases.")
  return(sentences)
})

all_sentence_releases_sex <- setNames(all_sentence_releases_sex, states)
all_sentence_releases_sex$Georgia

# Generate graph for each state
states <- unique(prison_releases_agerlse$state)
all_bar_releases_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_agerlse |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Age:</b> ", agerlse, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(agerlse))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "agerlse", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Age") |>
    hc_subtitle(text = paste0("Prison Releases, ", select_year)) |>
    hc_exporting(enabled = TRUE)|>
    hc_colors(c(color5))
  return(highcharts)
})
all_bar_releases_agerlse <- setNames(all_bar_releases_agerlse, states)
all_bar_releases_agerlse$Georgia


# Generate sentence for each state
states <- unique(prison_releases_agerlse$state)
all_sentence_releases_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- prison_releases_agerlse |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$agerlse <- gsub("-", " to ", df1$agerlse)
  sentences <- paste0("In ", select_year, ", most releases from prison were people between ",
                      tolower(df1$agerlse), " old, representing ", round(df1$prop*100, 0), " percent of prison releases.")
  return(sentences)
})

all_sentence_releases_agerlse <- setNames(all_sentence_releases_agerlse, states)
all_sentence_releases_agerlse$Georgia








# ---------------------------------------------------------------------------- #
# OFFENSE TYPE
# ---------------------------------------------------------------------------- #

releases_fbi_index <- ncrp_releases |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  filter(!is.na(fbi_index) & fbi_index != "Unknown") |>
  count(fbi_index) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup()

# Generate graph for each state
states <- unique(releases_fbi_index$state)
all_bar_releases_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- releases_fbi_index |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Offense Type:</b> ", fbi_index, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  hc_accessibility_text <- paste0("This graph shows the proportion of prison releases by offense type in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = paste0("Prison Population by Offense Type, ", select_year)) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color5))
  return(highcharts)
})
all_bar_releases_fbi_index <- setNames(all_bar_releases_fbi_index, states)
all_bar_releases_fbi_index$Georgia

# Generate sentence for each state
states <- unique(releases_fbi_index$state)
all_sentence_releases_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- releases_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", most people released from prison were incarcerated for ",
                      tolower(df1$fbi_index), " offenses, representing ", round(df1$prop*100, 0), " percent of people.")
  return(sentences)
})

all_sentence_releases_fbi_index <- setNames(all_sentence_releases_fbi_index, states)
all_sentence_releases_fbi_index$Georgia











# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_sentence_releases,                     file = file.path(folder, "all_sentence_releases.rds"))
  save(all_line_releases_by_year,                 file = file.path(folder, "all_line_releases_by_year.rds"))
  save(all_sentence_pe_proportion_released,       file = file.path(folder, "all_sentence_pe_proportion_released.rds"))
  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))
  save(all_pie_release_type,                      file = file.path(folder, "all_pie_release_type.rds"))

  save(all_sentence_releases_race,       file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_releases_race.rds"))
  save(all_bar_releases_race,            file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_releases_race.rds"))

  save(all_sentence_releases_sex,        file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_releases_sex.rds"))
  save(all_bar_releases_sex,             file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_releases_sex.rds"))

  save(all_sentence_releases_agerlse,    file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_releases_agerlse.rds"))
  save(all_bar_releases_agerlse,         file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_releases_agerlse.rds"))

  save(all_sentence_releases_fbi_index,  file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_releases_fbi_index.rds"))
  save(all_bar_releases_fbi_index,       file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_releases_fbi_index.rds"))
}

