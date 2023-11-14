#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: October 31, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for app
#######################################

################################################################################

# Section: Release Trends

# (1) # and % of parole-eligible people released each year,
# (2) out of all releases: % of people released each year at parole eligibility year,
#                          % 1-5 years after parole eligibility,
#                          % of people released more than 5 years after parole eligibility

################################################################################

# 1) # and % of parole-eligible people released each year

# We have the year-end population of those who were parole-eligible but were not released,
#   and we have the number of parole-eligible individuals who were released but
#   we don't have the total initial population of parole-eligible individuals for each year,
#   so, determine this below.

# Calculate the number of parole eligible people released by state and year
ncrp_pe_releases_by_year <- ncrp_releases %>%
  filter(rptyear >= 2010) %>%
  filter(parelig_status == "Current") %>%
  group_by(state, rptyear) %>%
  summarise(total_parole_eligible_releases = n())

# Calculate the number of parole eligible people in prison by state and year
ncrp_pe_population_not_released_by_year <- ncrp_yearendpop %>%
  filter(rptyear >= 2010) %>%
  filter(parelig_status == "Current") %>%
  group_by(state, rptyear) %>%
  summarise(total_parole_eligible_population_not_released = n())

# Merge data together
ncrp_pe_proportion_released <- ncrp_pe_population_not_released_by_year %>%
  left_join(ncrp_pe_releases_by_year, by = c("state", "rptyear")) %>%
  mutate(total_parole_eligible_population =
           total_parole_eligible_releases + total_parole_eligible_population_not_released,
         prop_parole_elgible_released =
           total_parole_eligible_releases/total_parole_eligible_population) %>%
  select(state, rptyear,
         total_parole_eligible_population_not_released,
         total_parole_eligible_releases) %>%
  pivot_longer(
    cols = c(total_parole_eligible_population_not_released, total_parole_eligible_releases),
    names_to = "status",
    values_to = "n"
  ) %>%
  mutate(status = case_when(
    status == "total_parole_eligible_population_not_released" ~ "Not Released",
    status == "total_parole_eligible_releases" ~ "Released"
  ))

# Highchart stacked bar chart
states <- unique(ncrp_pe_proportion_released$state)
all_stackedbar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_proportion_released %>%
    filter(state == x)
  jsFormatter <- JS("function() {
                   var total = this.point.stackTotal;
                   var percentage = Math.round((this.y / total) * 100);
                   return percentage + '%';
                  }")
  highcharts <- df1 %>%
    hchart(
      type = "column",
      hcaes(x = rptyear, y = n, group = status)
    ) %>%
    hc_yAxis(title = list(text = " Parole Eligible Population")) %>%
    hc_xAxis(categories = unique(df1$rptyear),
             title = "") %>%
    hc_add_theme(hc_theme_with_line) %>%
    hc_legend(enabled = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(stacking = "normal",
                                 animation = FALSE,
                                 cursor = "pointer",
                                 dataLabels = list(enabled = TRUE, formatter = jsFormatter),
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD accessibility text",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD accessibility text")))
  return(highcharts)
})
all_stackedbar_parole_eligibility_release <- setNames(all_stackedbar_parole_eligibility_release, states)
all_stackedbar_parole_eligibility_release$Georgia



# 2) Out of all releases: % of people released each year at parole eligibility year,
#                         % 1-5 years after parole eligibility,
#                         % of people released more than 5 years after parole eligibility


# Highchart bar chart showing relationship between release year and parole eligibility
#     year by release type (unconditional vs conditional release)
# Prepare data
ncrp_release_by_reltype <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  group_by(state, reltype) %>%
  fnc_values_labels(time_between_ped_release_category) %>%
  fnc_tooltip(time_between_ped_release_category, prop_label,
              paste0("Timing of Release: ", select_year)) %>%
  mutate(time_between_ped_release_category =
           factor(time_between_ped_release_category,
                  levels = c("Missing Parole Eligibility Year",
                             "Released more than 5 Years After Parole Eligibility Year",
                             "Released 1 to 5 Years After Parole Eligibility Year",
                             "Released on Parole Eligibility Year",
                             "Released before Parole Eligibility Year")))

# Highchart stacked bar chart showing release timing by reltype type
states <- unique(ncrp_release_by_reltype$state)
all_groupedbar_release_timing_reltype <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_release_by_reltype %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released before their parole eligibility year,
                                  at their parole eligibility year, 1 to 5 years after their parole eligibility year,
                                  5 years after their parole eligibility year, or missing their parole eligibility year
                                  by release type (unconditional release vs. conditional release) in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "reltype", "time_between_ped_release_category", hc_accessibility_text)
  return(highcharts)
})
all_groupedbar_release_timing_reltype <- setNames(all_groupedbar_release_timing_reltype, states)
all_groupedbar_release_timing_reltype$Georgia








################################################################################

# Section: Demographics

################################################################################

# Releases by Race and Ethnicity
ncrp_releases_race <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_labels(race) %>%
  fnc_tooltip(race, prop_label,
              paste0("Race and Ethnicity: "))%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of releases by race and ethnicity
states <- unique(ncrp_releases_race$state)
all_bar_release_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_releases_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by race and ethnicity in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "race", hc_accessibility_text)
  return(highcharts)
})
all_bar_release_race <- setNames(all_bar_release_race, states)
all_bar_release_race$Georgia



# Releases by Gender
ncrp_releases_gender <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_labels(sex) %>%
  fnc_tooltip(sex, prop_label,
              paste0("Gender: "))%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )


# Create highcharts showing breakdown of releases by gender
states <- unique(ncrp_releases_gender$state)
all_bar_release_gender <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_releases_gender %>%
    filter(state == x) %>%
    arrange(desc(n))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by gender in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "sex", hc_accessibility_text)
  return(highcharts)
})
all_bar_release_gender <- setNames(all_bar_release_gender, states)
all_bar_release_gender$Georgia



# Releases by Age
ncrp_releases_agerlse <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_labels(agerlse) %>%
  fnc_tooltip(agerlse, prop_label,
              paste0("Age: "))%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of releases by age
states <- unique(ncrp_releases_agerlse$state)
all_bar_release_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_releases_agerlse %>%
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "agerlse", hc_accessibility_text)
  return(highcharts)
})
all_bar_release_agerlse <- setNames(all_bar_release_agerlse, states)
all_bar_release_agerlse$Georgia








################################################################################

# Section: Type of Releases

# (1) Unconditional vs. Conditional release, 2020,

################################################################################

# Filter to people with release type information
# Remove "Other releases" - although Alabama has 30% other releases #######################?
ncrp_release_type <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(reltype == "Conditional release" | reltype == "Unconditional release") %>%
  group_by(state) %>%
  fnc_values_labels(reltype) %>%
  fnc_tooltip(reltype, prop_label,
              paste0("Release Type: "))


# Highchart pie chart showing releases by release type
states <- unique(ncrp_release_type$state)
all_pie_release_type <- map(.x = states, .f = function(x) {
  df1 <- ncrp_release_type %>%
    ungroup() %>%
    filter(state == x) %>%
    select(reltype, prop, prop_label)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by release type (unconditional release vs. conditional
                                  release) in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_piechart(df1, "reltype", hc_accessibility_text)
  return(highcharts)
})
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia








################################################################################

# Section: Release Timing by Offense Type

################################################################################

# Prepare data
ncrp_release_by_offense <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state, fbi_index) %>%
  fnc_values_labels(time_between_ped_release_category) %>%
  fnc_tooltip(time_between_ped_release_category, prop_label,
              paste0("Timing of Release: ")) %>%
  mutate(time_between_ped_release_category =
           factor(time_between_ped_release_category,
                  levels = c("Missing Parole Eligibility Year",
                             "Released more than 5 Years After Parole Eligibility Year",
                             "Released 1 to 5 Years After Parole Eligibility Year",
                             "Released on Parole Eligibility Year",
                             "Released before Parole Eligibility Year")))


# Highchart stacked bar chart showing release timing by offense type
states <- unique(ncrp_release_by_offense$state)
all_groupedbar_release_timing_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_release_by_offense %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  released by most serious sentenced offense in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "fbi_index", "time_between_ped_release_category", hc_accessibility_text)
  return(highcharts)
})
all_groupedbar_release_timing_fbi_index <- setNames(all_groupedbar_release_timing_fbi_index, states)
all_groupedbar_release_timing_fbi_index$Georgia









################################################################################

# Section: Time Served by Offense Type

################################################################################

# Calculate the average and median length of stay by state and by offense type
ncrp_los_by_offense_type <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state, fbi_index) %>%
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE),
    Median  = median(time_between_admisson_release, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(Average, Median),
    names_to = "type",
    values_to = "value") %>%
  mutate(data_label = round(value, 1))

# Highchart
states <- unique(ncrp_los_by_offense_type$state)
all_groupedbar_los_by_offense <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_los_by_offense_type %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  max_y_value <- max(df1$value) + 5

  hc_accessibility_text <- paste0("This graph shows the average and median length of stay by offense type in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- hchart(df1, "bar",
                       hcaes(x = fbi_index,
                             y = value,
                             group = type
                       ),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.data_label}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = "Number of Years"),
             max = max_y_value) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = hc_accessibility_text, landmarkVerbosity = "one"),
      area = list(accessibility = list(description = hc_accessibility_text)))
  return(highcharts)
})

all_groupedbar_los_by_offense <- setNames(all_groupedbar_los_by_offense, states)
all_groupedbar_los_by_offense$Georgia



# Calculate the average length of stay by state and by offense type
ncrp_los_by_offense_type <- ncrp_releases %>%
  group_by(state, fbi_index, rptyear) %>%
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE)) %>%
  pivot_longer(cols = Average, names_to = "type", values_to = "value") %>%
  group_by(state) %>%
  mutate(max_rptyear = max(rptyear),
         min_rptyear = max_rptyear - 10) %>%
  filter(rptyear == min_rptyear | rptyear == max_rptyear) %>%
  group_by(state, fbi_index) %>%
  mutate(change_value_10_years = last(value) - first(value),
         prop = (last(value) - first(value)) / first(value) * 100,
         change_sentence = ifelse(prop >= 0,
                                  paste0(round(value, 1), "<br><b>", "\u2191", "</b> ", round(prop, 0), "% from 10 years ago"),
                                  paste0(round(value, 1), "<br><b>", "\u2193", "</b> ", round(abs(prop), 0), "% from 10 years ago")))


# Highchart
states <- unique(ncrp_los_by_offense_type$state)
all_bar_los_by_offense <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_los_by_offense_type %>%
    ungroup() %>%
    filter(state == x & type == "Average") %>%
    distinct()
  max_y_value <- max(df1$value) + 10

  hc_accessibility_text <- paste0("This graph shows the average length of stay by offense type in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- hchart(df1, "bar",
                       hcaes(x = fbi_index,
                             y = value
                       ),
                       dataLabels = list(enabled = TRUE,
                                         align = 'left',
                                         useHTML = TRUE,
                                         inside = FALSE,
                                         format = "{point.change_sentence}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = "Average Number of Years"),
             max = max_y_value) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE) %>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = hc_accessibility_text,
        landmarkVerbosity = "one"),
      area = list(accessibility = list(description = hc_accessibility_text)))
  return(highcharts)
})

all_bar_los_by_offense <- setNames(all_bar_los_by_offense, states)
all_bar_los_by_offense$Georgia









################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))

  save(all_groupedbar_release_timing_reltype,     file = file.path(folder, "all_groupedbar_release_timing_reltype.rds"))

  save(all_bar_release_agerlse,                   file = file.path(folder, "all_bar_release_agerlse.rds"))
  save(all_bar_release_gender,                    file = file.path(folder, "all_bar_release_gender.rds"))
  save(all_bar_release_race,                      file = file.path(folder, "all_bar_release_race.rds"))


  save(all_pie_release_type,                      file = file.path(folder, "all_pie_release_type.rds"))
  save(all_groupedbar_release_timing_fbi_index,   file = file.path(folder, "all_groupedbar_release_timing_fbi_index.rds"))

  save(all_groupedbar_los_by_offense,             file = file.path(folder, "all_groupedbar_los_by_offense.rds"))
  save(all_bar_los_by_offense,                    file = file.path(folder, "all_bar_los_by_offense.rds"))

}

