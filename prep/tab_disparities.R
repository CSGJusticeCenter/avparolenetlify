#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: October 5, 2023 (MAR)
# Description:
#    Disparities tables and graphics for app
#######################################

################################################################################

# Section: Gender

# (1) bar charts including community, prison pop, parole-eligible pop, paroled at first opportunity pop
# (2) RRIs for each of those populations,
# (3) release date compared to first eligibility date (e.g., 50% released first year,
#                                                            22% year after first eligibility,
#                                                            15% 2 years after eligible, etc.)

################################################################################




















################################################################################

# Section: Race and Ethnicity

# (1) bar charts including community, prison pop, parole-eligible pop, paroled at first opportunity pop
# (2) RRIs for each of those populations,
# (3) release date compared to first eligibility date (e.g., 50% released first year,
#                                                            22% year after first eligibility,
#                                                            15% 2 years after eligible, etc.)

################################################################################

# (1) bar charts including community, prison pop, parole-eligible pop, paroled at first opportunity pop

# Population by race and ethnicity
# Weighted estimate of percentage of race from select_year census
# Pulled estimated counts and construct percent estimate
# These are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P3_003N",
               estimate_black              = "P3_004N",
               estimate_asian              = "P3_006N",
               estimate_native_hawaiian_pi = "P3_007N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P1_005N")

# Use lapply to retrieve and process data for each state
# Make race and ethnicity consistent with NCRP dat(Black, White, Hispanic, Other)
states <- state.name
census_state_population_list <- lapply(states, fnc_get_census_data)

# Convert list into a dataframe
census_state_population <- bind_rows(census_state_population_list)

# Add "state" column
census_state_population$state <- rep(states, each = nrow(census_state_population) / length(states))

# Calculate proportion of people in community by race and ethnicity
# Combine some races into other category
census_state_population <- census_state_population %>%
  rename(total_state_population = summary_value) %>%
  group_by(state, total_state_population,
           race = ifelse(race %in% c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
                         race, "Other race(s), non-Hispanic")) %>%
  summarise(n = sum(value)) %>%
  ungroup() %>%
  mutate(
    prop = (n/total_state_population),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)) %>%
  select(-total_state_population) %>%
  mutate(population_type = "In the Community")

# Calculate proportion of people parole-eligible in prison by race and ethnicity
ncrp_parole_eligible_population <- ncrp_yearendpop %>%

  filter(rptyear == select_year) %>%

  filter(race != "Unknown") %>%
  filter(parelig_status == "Current") %>%
  fnc_parameters() %>%
  group_by(state) %>%
  fnc_values_tooltip(race) %>%
  select(-tooltip) %>%
  mutate(population_type = "In Prison but Parole-Eligible")

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


# Add all data together
merged_population_data <- rbind(census_state_population,
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
                                  "In Prison but Parole-Eligible"))) %>%
  arrange(state, population_type, desc(race)) %>%
  mutate(tooltip = paste0("<b>", state, "</b><br><br>",
                          "<b>", race, "</b><br><br>",
                          "<b>", population_type, "</b><br><br>",
                          "Percentage of People: <b>", prop_label, "</b>", sep = "")
         )
# merged_population_data <- rbind(census_state_population,
#                                 ncrp_parole_eligible_population)
# merged_population_data <- merged_population_data %>%
#   mutate(race = factor(race,
#                        levels = c("Other race(s), non-Hispanic",
#                                   "White, non-Hispanic",
#                                   "Hispanic, any race",
#                                   "Black, non-Hispanic")),
#          population_type = factor(population_type,
#                                   levels = c("In the Community",
#                                              "In Prison but Parole-Eligible"))) %>%
#   arrange(state, population_type, desc(race))


# Highchart showing race population in the community and in prison but parole-eligible
states <- unique(merged_population_data$state)
all_groupedbar_disparities_race <- map(.x = states,  .f = function(x) {
  df1 <- merged_population_data %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_grouped_barchart(df1, "race", "population_type", "TBD accessibility text") %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE)
  return(highcharts)
})
all_groupedbar_disparities_race <- setNames(all_groupedbar_disparities_race, states)
all_groupedbar_disparities_race$Georgia




ncrp_time_between_ped_release <- ncrp_releases %>%
  filter(rptyear == select_year) %>%
  filter(time_between_ped_release_category != "Missing" &
         time_between_ped_release_category != "Released Before Parole Eligibility Year" &
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
                                                                "More than Fifth Year"))) %>%
  group_by(state, race) %>%
  fnc_values_tooltip2(time_between_ped_release_category2, race) %>%
  mutate(race = factor(race,
                       levels = c("Other race(s), non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Black, non-Hispanic")))

states <- unique(ncrp_time_between_ped_release$state)
all_groupedcolumn_disparities_release_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_time_between_ped_release %>%
    ungroup() %>%
    filter(state == x) %>%
    arrange(desc(race))
  highcharts <- fnc_grouped_columnchart(df1, "race", "time_between_ped_release_category2", "TBD accessibility text") %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE)
  return(highcharts)
})
all_groupedcolumn_disparities_release_race <- setNames(all_groupedcolumn_disparities_release_race, states)
all_groupedcolumn_disparities_release_race$Georgia







################################################################################

# Section: Predicted Probabilities

# Predicted probabilities: Controlling for offense and sentence length,
#      predicted probabilities of release before/at eligibility date. Something like
#      “After controlling for differences in ages, offenses, and sentence length,
#      66% of White people, 40% of Black people, and 55% of Hispanic people are
#      released before/on parole eligibility date. Similarly, 80% of women and 74%
#      of men were released before/on parole eligibility date

################################################################################








################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_groupedbar_disparities_race,         file = file.path(folder, "all_groupedbar_disparities_race.rds"))
  save(all_groupedcolumn_disparities_release_race, file = file.path(folder, "all_groupedcolumn_disparities_release_race.rds"))


}
