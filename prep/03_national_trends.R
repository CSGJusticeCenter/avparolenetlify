#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: August 25, 2023 (MAR)
# Description:
#    Parole eligibility table and data for national trends page
#######################################

################################################################################

# Reactable table on "National Trends" page
# Parole eligibility by state in 2020

# Obtained from NCRP year end population

################################################################################

# Get total prison population by state and year
ncrp_prison_population <- ncrp_yearendpop %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  summarise(yearendpop = sum(n, na.rm = FALSE))

# Merge prison population numbers
# Get number of people by parole eligibility status
# Just for people in prison for a new court commitment
ncrp_parole_eligible_new_court <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  left_join(ncrp_prison_population, by = c("state", "rptyear")) %>%
  mutate(parelig_status = paste0(parelig_status, " (New Crime)"),
         prop = n / yearendpop)

# Merge prison population numbers
# Get number of people by parole eligibility status
# Just for people in prison for a parole return/revocation
ncrp_parole_eligible_other <- ncrp_yearendpop %>%
  filter(admtype != "New court commitment" | is.na(admtype)) %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  left_join(ncrp_prison_population, by = c("state", "rptyear")) %>%
  mutate(parelig_status = paste0(parelig_status, " (Other)"),
         prop = n / yearendpop)

# Add data together
ncrp_parole_eligible_all <- rbind(ncrp_parole_eligible_new_court,
                                  ncrp_parole_eligible_other)


# Combine missing data for people in prison for new crimes and other
parole_eligibility_table <- ncrp_parole_eligible_all %>%
  mutate(parelig_status = case_when(
    parelig_status %in% c("Missing (New Crime)", "Missing (Other)") ~ "Missing",
    TRUE ~ as.character(parelig_status))) %>%
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

# Get population of people who are in prison for a new crime
# Get number and prop of people by eligibility statuses by state and report year
# Reformat for table viewing
# parole_eligibility_table <- ncrp_yearendpop %>%
#   filter(admtype == "New court commitment") %>%
#   group_by(state, rptyear) %>%
#   count(parelig_status) %>%
#   mutate(
#     prop = n/sum(n),
#   ) %>%
#   ungroup() %>%
#   pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
#   mutate(name = case_when(
#     type == "n"    ~ paste(parelig_status, "count"),
#     type == "prop" ~ paste(parelig_status, "perc.")
#   )) %>%
#   select(state, rptyear, yearendpop, name, value) %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   clean_names()

# Filter to 2020
parole_eligibility_table_2020 <- parole_eligibility_table %>%
  filter(rptyear == 2020)

# Find missing states
# Arizona, Michigan, New Jersey, New Mexico
missing_data <- tibble(state = setdiff(state.name,
                                       parole_eligibility_table_2020$state),
                       rptyear = 2020)

# Combine the missing states with the original dataframe to get all 50 states
# This final table shows parole eligibility statuses for people in prison for a
#     new crime, not a parole return/revocation.
parole_eligibility_table_2020 <-
  bind_rows(parole_eligibility_table_2020, missing_data) %>%
  left_join(ncrp_prison_population, by = c("state", "rptyear")) %>%
  arrange(state) %>%
  select(state,
         rptyear,
         yearendpop,
         current_new_crime_count,
         future_1_5_years_new_crime_count,
         future_6_years_new_crime_count,
         missing_count,
         current_new_crime_perc,
         future_1_5_years_new_crime_perc,
         future_6_years_new_crime_perc,
         missing_perc
         )







################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table,      file=file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_2020, file=file.path(folder, "parole_eligibility_table_2020.rds"))

}
