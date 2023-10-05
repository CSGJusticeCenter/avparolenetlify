#######################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: October 2, 2023 (MAR)
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
#   which makes direct calculation of the proportion released a bit tricky.

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
    hc_add_theme(hc_theme_jc_line) %>%
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

# Prepare data
ncrp_time_between_ped_release_category <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_tooltip(time_between_ped_release_category)

# Highchart bar chart showing relationship between release year and parole eligibility year
states <- unique(ncrp_time_between_ped_release_category$state)
all_bar_parole_eligibility_release <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_time_between_ped_release_category %>%
    filter(state == x)
  highcharts <- fnc_basic_columnchart(df1, "time_between_ped_release_category", "TBD accessibility text")
  return(highcharts)
})
all_bar_parole_eligibility_release <- setNames(all_bar_parole_eligibility_release, states)
all_bar_parole_eligibility_release$Georgia









################################################################################

# Section: Demographics

################################################################################






################################################################################

# Section: Type of Releases

# (1) Unconditional vs. Conditional release, 2020,

################################################################################

# Filter to people with release type information
# Remove "Other releases" - although Alabama has 30% other releases #######################?
ncrp_release_type_select_year <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(reltype == "Conditional release" | reltype == "Unconditional release") %>%
  group_by(state) %>%
  fnc_values_tooltip(reltype)

# Highchart pie chart showing releases by release type
states <- unique(ncrp_release_type_select_year$state)
all_pie_release_type <- map(.x = states, .f = function(x) {
  df1 <- ncrp_release_type_select_year %>%
    ungroup() %>%
    filter(state == x) %>%
    select(reltype, prop, prop_label)
  highcharts <- fnc_basic_piechart(df1, "reltype", "TBD accessibility text")
  return(highcharts)
})
all_pie_release_type <- setNames(all_pie_release_type, states)
all_pie_release_type$Georgia






################################################################################

# Section: Release Timing by Offense Type

################################################################################

# Prepare data
ncrp_release_by_offense_select_year <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  group_by(state, fbi_index) %>%
  fnc_values_tooltip(time_between_ped_release_category) %>%
  mutate(time_between_ped_release_category =
           factor(time_between_ped_release_category,
                  levels = c("Missing Parole Eligibility Year",
                             "Released 5 Years After Parole Eligibility Year",
                             "Released 1-5 Years After Parole Eligibility Year",
                             "Released at Parole Eligibility Year",
                             "Released Before Parole Eligibility Year")))


# Highchart stacked bar chart showing release timing by offense type
states <- unique(ncrp_release_by_offense_select_year$state)
all_groupedbar_release_timing_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_release_by_offense_select_year %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_grouped_stacked_barchart(df1, "fbi_index", "time_between_ped_release_category", "TBD accessibility text")
  return(highcharts)
})
all_groupedbar_release_timing_fbi_index <- setNames(all_groupedbar_release_timing_fbi_index, states)
all_groupedbar_release_timing_fbi_index$Georgia





################################################################################

# Section: Time Served by Offense Type

################################################################################







################################################################################

# Section: Maxout

################################################################################

# Determine if people were released on or after their mandatory
# Remove people who were released after their mandatory - this is a mistake according to Carl
ncrp_releases_maxout_select_year <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(!is.na(mand_prisrel_year) &
        !is.na(relyr) &
        mand_prisrel_year >= relyr) %>%
  mutate(maxout = case_when(
    mand_prisrel_year > relyr  ~ "Released Before Mandatory Release Year",
    mand_prisrel_year == relyr ~ "Released On Mandatory Release Year")
  ) %>%
  group_by(state) %>%
  fnc_values_tooltip(maxout)

# Highchart pie chart showing releases by release type
states <- unique(ncrp_releases_maxout_select_year$state)
all_pie_maxout <- map(.x = states, .f = function(x) {
  df1 <- ncrp_releases_maxout_select_year %>%
    ungroup() %>%
    filter(state == x)
  highcharts <- fnc_basic_piechart(df1, "maxout", "TBD accessibility text")
  return(highcharts)
})
all_pie_maxout <- setNames(all_pie_maxout, states)
all_pie_maxout$Georgia


################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_stackedbar_parole_eligibility_release, file = file.path(folder, "all_stackedbar_parole_eligibility_release.rds"))
  save(all_bar_parole_eligibility_release,        file = file.path(folder, "all_bar_parole_eligibility_release.rds"))

  save(all_pie_release_type,                      file = file.path(folder, "all_pie_release_type.rds"))
  save(all_groupedbar_release_timing_fbi_index,   file = file.path(folder, "all_groupedbar_release_timing_fbi_index.rds"))

  save(all_pie_maxout,                            file = file.path(folder, "all_pie_maxout.rds"))

}

