#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: OCtober 23, 2023 (MAR)
# Description:
#    Parole eligibility table and data for national trends page
#######################################

################################################################################

# Reactable table on "National Trends" page
# Parole eligibility by state in select year

# Obtained from NCRP year end population

################################################################################

# Get total prison population by state and year
ncrp_prison_population <- ncrp_yearendpop %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  summarise(yearendpop = sum(n, na.rm = FALSE))

# Get total prison population by state and year
# Just for people in prison for a new court commitment and sentence length 1-25 years
ncrp_prison_population_125years_new_crime <- ncrp_yearendpop %>%
  fnc_parameters() %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  summarise(yearendpop_125years_new_crime = sum(n, na.rm = FALSE))

# Get number of people in prison by parole eligibility status
# Just for people in prison for a new court commitment and sentence length 1-25 years
# Merge prison population numbers to get proportions
ncrp_parole_eligible_125years_new_crime <- ncrp_yearendpop %>%
  fnc_parameters() %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  left_join(ncrp_prison_population,
            by = c("state", "rptyear")) %>%
  mutate(prop = n / yearendpop)

# Reshape data
parole_eligibility_table <- ncrp_parole_eligible_125years_new_crime %>%
  group_by(state, rptyear, parelig_status) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    prop = sum(prop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, rptyear, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()

# Filter to select year
parole_eligibility_table_select_year <- parole_eligibility_table %>%
  filter(rptyear == select_year)

# Find missing states
# Arizona, Michigan, New Jersey, New Mexico
missing_data <- tibble(state = setdiff(state.name,
                                       parole_eligibility_table_select_year$state),
                       rptyear = select_year)

# Combine the missing states with the original dataframe to get all 50 states
# This final table shows parole eligibility statuses for people in prison for a
#     new crime, not a parole return/revocation.
parole_eligibility_table_select_year <-
  bind_rows(parole_eligibility_table_select_year, missing_data) %>%
  left_join(ncrp_prison_population,
            by = c("state", "rptyear")) %>%
  left_join(ncrp_prison_population_125years_new_crime,
            by = c("state", "rptyear")) %>%
  arrange(state) %>%
  select(state,
         rptyear,
         yearendpop,
         yearendpop_125years_new_crime,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count,
         current_perc,
         future_1_5_years_perc,
         future_6_years_perc,
         missing_perc)







################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table,             file = file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_select_year, file = file.path(folder, "parole_eligibility_table_select_year.rds"))

}
