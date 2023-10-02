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
# (2) Conditional vs. unconditional release, 2020

################################################################################







################################################################################

# Section: Release Timing by Offense Type

################################################################################








################################################################################

# Section: Time Served by Offense Type

################################################################################








################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_bar_parole_eligibility_release, file = file.path(folder, "all_bar_parole_eligibility_release.rds"))

}

