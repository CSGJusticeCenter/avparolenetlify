#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: January 15, 2024 (MAR)
# Description:
#    Disparities tables and graphics for app for Disparities Tab
#######################################

################################################################################

# Section: Race and Ethnicity

# (1) barcharts for 1. in community vs in prison by race abd ethnicity and
#                   2. in community vs in prison and parole-eligible by race abd ethnicity
# (2) RRIs for each of those populations,
# (3) release date compared to first eligibility date (e.g., 50% released first year,
#                                                            22% year after first eligibility,
#                                                            15% 2 years after eligible, etc.)

################################################################################

# (1) bar charts including community, prison pop, parole-eligible pop, paroled at first opportunity pop

# First, get census information
# Population by race and ethnicity
# Weighted estimate of percentage of race from select_year census
# Pulled estimated counts and construct percent estimate

# These are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P4_005N",
               estimate_black              = "P4_006N",
               estimate_asian              = "P4_008N",
               estimate_native_hawaiian_pi = "P4_009N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P4_007N",
               estimate_more_than_one_race = "P4_011N")

# Use lapply to retrieve and process data for each state
# Make race and ethnicity consistent with NCRP dat(Black, White, Hispanic, Other)
states <- state.name
census_state_race_population_list <- lapply(states, fnc_get_census_data)

# Convert list into a dataframe
census_state_race_population <- bind_rows(census_state_race_population_list)

# Add "state" column
census_state_race_population$state <- rep(states, each = nrow(census_state_race_population) / length(states))

##########
# Community by Race and Ethnicity
##########

# Calculate proportion of people in community by race and ethnicity
# Combine some races into other category
census_state_race_population <- census_state_race_population %>%
  rename(total_state_population = summary_value) %>%
  group_by(state, total_state_population,
           race = ifelse(race %in% c("White, non-Hispanic",
                                     "Black, non-Hispanic",
                                     "Hispanic, any race"),
                         race, "Other race(s), non-Hispanic")) %>%
  summarise(n = sum(value)) %>%
  ungroup() %>%
  mutate(
    prop = (n/total_state_population),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)) %>%
  select(-total_state_population) %>%
  mutate(population_type = "In the Community")

##########
# Parole-Eligible Race and Ethnicity
##########

# Calculate proportion of people parole-eligible in prison by race and ethnicity
ncrp_parole_eligible_population <- ncrp_yearendpop %>%

  filter(rptyear == select_year) %>%

  filter(race != "Unknown") %>%
  filter(parelig_status == "Current") %>%
  fnc_parameters() %>%
  group_by(state) %>%
  fnc_values_tooltip(race) %>%
  select(-tooltip) %>%
  mutate(population_type = "In Prison Past Their Parole Eligibility Date")

##########
# Paroled at First Opportunity by Race and Ethnicity
##########

# Calculate proportion of people paroled at first opportunity by race and ethnicity
ncrp_released_at_parole_eligibility_year <- ncrp_releases %>%

  filter(rptyear == select_year) %>%
  fnc_parameters() %>%
  filter(race != "Unknown") %>%

  mutate(released_at_parole_eligibility = case_when(
    relyr < parelig_year  ~ "Released before Parole Eligibility Year",
    relyr == parelig_year ~ "Released on Parole Eligibility Year",
    relyr > parelig_year  ~ "Released after Parole Eligibility Year",
    is.na(relyr) | is.na(parelig_year) ~ "Unknown"
  )) %>%

  filter(released_at_parole_eligibility == "Released on Parole Eligibility Year") %>%

  group_by(state, released_at_parole_eligibility) %>%
  count(race) %>%
  mutate(
    prop = (n / sum(n)),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)) %>%
  select(population_type = released_at_parole_eligibility, everything())


##########
# Combine All Data Together
##########

# Add all data together
merged_population_data <- rbind(census_state_race_population,
                                bjs_prison_pop_by_race,
                                ncrp_parole_eligible_population,
                                ncrp_released_at_parole_eligibility_year)
merged_population_data <- merged_population_data %>%
  mutate(race = factor(race,
                       levels = c("Other race(s), non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Black, non-Hispanic")),
         population_type = factor(population_type,
                                  levels = c("In the Community",
                                             "Released on Parole Eligibility Year",
                                             "In Prison",
                                             "In Prison Past Their Parole Eligibility Date"))) %>%
  arrange(state, population_type, desc(race)) %>%
  mutate(tooltip = paste0("<b>", state, "</b><br><br>",
                          "<b>", race, "</b><br><br>",
                          "<b>", population_type, "</b><br><br>",
                          "Percentage of People: <b>", prop_label, "</b>", sep = "")
  )

# Highchart showing race and ethnicity population in the community and in prison
states <- unique(merged_population_data$state)
all_groupedbar_disparities_inprison_race <- map(.x = states,  .f = function(x) {
  df1 <- merged_population_data %>%
    ungroup() %>%
    filter(state == x) %>%
    filter(population_type == "In Prison" |
             population_type == "In the Community") %>%
    distinct()
  highcharts <- fnc_grouped_barchart(df1, "race", "population_type", "TBD accessibility text") %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE)
  return(highcharts)
})
all_groupedbar_disparities_inprison_race <- setNames(all_groupedbar_disparities_inprison_race, states)
all_groupedbar_disparities_inprison_race$Georgia

# Highchart showing race and ethnicity population in the community and currently eligible for parole
all_groupedbar_disparities_inprisonpe_race <- map(.x = states,  .f = function(x) {
  df1 <- merged_population_data %>%
    ungroup() %>%
    filter(state == x) %>%
    filter(population_type == "In Prison Past Their Parole Eligibility Date" |
             population_type == "In the Community") %>%
    distinct()
  highcharts <- fnc_grouped_barchart(df1, "race", "population_type", "TBD accessibility text") %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE)
  return(highcharts)
})
all_groupedbar_disparities_inprisonpe_race <- setNames(all_groupedbar_disparities_inprisonpe_race, states)
all_groupedbar_disparities_inprisonpe_race$Georgia



################################################################################

# (2) RRIs for each of those populations 1. in prison and 2. in prison past their parole eligibility date

################################################################################

# Select columns and add data together
# Combine data
# Calculate rate and RRIs
census_race <- census_state_race_population %>%
  select(state, race, n_census = n)
bjs_race <- bjs_prison_pop_by_race %>%
  select(state, race, n_prison = n)
merged_race_data <- census_race %>%
  left_join(bjs_race, by = c("state", "race"))

white_rate <- merged_race_data %>%
  filter(race == "White, non-Hispanic") %>%
  mutate(white_rate = (n_prison / n_census) * 100000) %>%
  select(state, white_rate)

nonwhite_rate <- merged_race_data %>%
  filter(race != "White, non-Hispanic") %>%
  mutate(nonwhite_rate = (n_prison / n_census) * 100000) %>%
  select(state, race, nonwhite_rate)

rri_in_prison_data <- nonwhite_rate %>%
  left_join(white_rate, by = "state") %>%
  mutate(rri = nonwhite_rate / white_rate)

# #states <- unique(rri_in_prison_data$state)
# states <- "Georgia" ############################################################
#
# # create infographics - takes ~10 minutes to run
# map(states, fnc_create_and_save_infograph)


################################################################################

# (3) release date compared to first eligibility date (e.g., 50% released first year,
#                                                            22% year after first eligibility,
#                                                            15% 2 years after eligible, etc.)

################################################################################

ncrp_time_between_ped_release <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr)) %>%
  mutate(time_between_ped_release_category2 = case_when(
    time_between_ped_release == 0 ~ "First Year",
    time_between_ped_release == 1 ~ "Second Year",
    time_between_ped_release == 2 ~ "Third Year",
    time_between_ped_release == 3 ~ "Fourth Year",
    time_between_ped_release == 4 ~ "Fifth Year",
    time_between_ped_release >= 5  ~ "More than Fifth Year"
  )) %>%
  mutate(time_between_ped_release_category2 = factor(time_between_ped_release_category2,
                                                     levels = c("First Year",
                                                                "Second Year",
                                                                "Third Year",
                                                                "Fourth Year",
                                                                "Fifth Year",
                                                                "More than Fifth Year")))

##########
# Race and Ethnicity
##########

states <- unique(ncrp_time_between_ped_release$state)
all_stackedcolumn_disparities_release_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_time_between_ped_release %>%
    group_by(state, race) %>%
    fnc_values_tooltip2(time_between_ped_release_category2, race) %>%
    ungroup() %>%
    filter(state == x) %>%
    arrange(desc(race)) %>%
    mutate(time_between_ped_release_category2 =
             factor(time_between_ped_release_category2,
                    levels = c("More than Fifth Year",
                               "Fifth Year",
                               "Fourth Year",
                               "Third Year",
                               "Second Year",
                               "First Year")))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by race
                                  released in their first year, second year, third year, fourth year,
                                  fifth year, and more than fifth year in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "race", "time_between_ped_release_category2", hc_accessibility_text)
  return(highcharts)
})
all_stackedcolumn_disparities_release_race <- setNames(all_stackedcolumn_disparities_release_race, states)
all_stackedcolumn_disparities_release_race$Georgia

##########
# Gender
##########

all_stackedcolumn_disparities_release_gender <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_time_between_ped_release %>%
    group_by(state, sex) %>%
    fnc_values_tooltip2(time_between_ped_release_category2, sex) %>%
    ungroup() %>%
    filter(state == x) %>%
    arrange(desc(sex)) %>%
    mutate(time_between_ped_release_category2 =
             factor(time_between_ped_release_category2,
                    levels = c("More than Fifth Year",
                               "Fifth Year",
                               "Fourth Year",
                               "Third Year",
                               "Second Year",
                               "First Year")))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by gender
                                  released in their first year, second year, third year, fourth year,
                                  fifth year, and more than fifth year in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "sex", "time_between_ped_release_category2", hc_accessibility_text)
  return(highcharts)
})
all_stackedcolumn_disparities_release_gender <- setNames(all_stackedcolumn_disparities_release_gender, states)
all_stackedcolumn_disparities_release_gender$Georgia

##########
# Age Category (age at release)
##########

all_stackedcolumn_disparities_release_agerlse <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_time_between_ped_release %>%
    group_by(state, agerlse) %>%
    fnc_values_tooltip2(time_between_ped_release_category2, agerlse) %>%
    ungroup() %>%
    filter(state == x) %>%
    arrange(desc(agerlse)) %>%
    mutate(time_between_ped_release_category2 =
             factor(time_between_ped_release_category2,
                    levels = c("More than Fifth Year",
                               "Fifth Year",
                               "Fourth Year",
                               "Third Year",
                               "Second Year",
                               "First Year")))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population by age
                                  released in their first year, second year, third year, fourth year,
                                  fifth year, and more than fifth year in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "agerlse", "time_between_ped_release_category2", hc_accessibility_text)
  return(highcharts)
})
all_stackedcolumn_disparities_release_agerlse <- setNames(all_stackedcolumn_disparities_release_agerlse, states)
all_stackedcolumn_disparities_release_agerlse$Georgia








################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_groupedbar_disparities_inprison_race,   file = file.path(folder, "all_groupedbar_disparities_inprison_race.rds"))
  save(all_groupedbar_disparities_inprisonpe_race, file = file.path(folder, "all_groupedbar_disparities_inprisonpe_race.rds"))

  save(all_stackedcolumn_disparities_release_race, file = file.path(folder, "all_stackedcolumn_disparities_release_race.rds"))
  save(rri_in_prison_data,                         file = file.path(folder, "rri_in_prison_data.rds"))

}

