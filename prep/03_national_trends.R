#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: August 22, 2023 (MAR)
# Description:
#    Parole eligibility table and data for national trends page
#######################################

################################################################################

# Reactable table on "National Trends" page
# Parole eligibility by state in 2020

# Obtained from NCRP year end population

################################################################################

# Get population of people who are in prison for a new crime
# Get number and prop of people by eligibility statuses by state and report year
# Reformat for table viewing
parole_eligibility_table <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, rptyear, yearendpop, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()

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
  arrange(state)







################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table,      file=file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_2020, file=file.path(folder, "parole_eligibility_table_2020.rds"))

}
