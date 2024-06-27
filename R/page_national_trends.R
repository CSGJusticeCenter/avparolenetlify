#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizatons for national trends page
#######################################


#------ Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop %>%
  group_by(state, rptyear) %>%
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment 1-25 year sentence lengths
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- ncrp_yearendpop |>
  filter(admtype == "New court commitment",
         sentlgth %in% c("1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years"))

# Get total prison population for new court commitments and sentence length 1-25 years
filtered_pop_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(filtered_total_pop = n(), .groups = 'drop')

# Get number of people in prison by parole eligibility status for the specified criteria
# Get proportion of parole eligibility statuses out of everyone in the filtered population
filtered_parole_status_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear, parelig_status) |>
  summarise(count = n(), .groups = 'drop') |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  mutate(proportion = count / filtered_total_pop)

# Reshape data for table
filtered_parole_elig_table_by_year <- filtered_parole_status_by_year |>
  pivot_longer(cols = c(count, proportion), names_to = "metric", values_to = "value") |>
  mutate(metric_name = case_when(
    metric == "count" ~ paste(parelig_status, "count"),
    metric == "proportion" ~ paste(parelig_status, "perc.")
  )) |>
  select(state, rptyear, metric_name, value) |>
  pivot_wider(names_from = metric_name, values_from = value) |>
  clean_names()

# Filter to select analysis year specified in the config file
filtered_parole_elig_table_analysis_year_with_missing_states <- filtered_parole_elig_table_by_year |>
  filter(rptyear == analysis_year)

# Find missing states and combine with the original dataframe
missing_states <- tibble(state = setdiff(state.name, filtered_parole_elig_table_analysis_year_with_missing_states$state),
                         rptyear = analysis_year)

# Add missing states to table so we have a complete table of 50 states
filtered_parole_elig_table_analysis_year <- filtered_parole_elig_table_analysis_year_with_missing_states |>
  bind_rows(missing_states) |>
  left_join(total_pop_by_year, by = c("state", "rptyear")) |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  arrange(state) |>
  select(state, rptyear, total_pop, filtered_total_pop,
         contains("current"), contains("future_1_5_years"), contains("future_6_years"), contains("missing"))



#------ Parole Eligibility Maps (%) ------#

# Create a vector of all state names
all_states <- state.name

# Get parole status information by state
parole_info_by_state_clean <- parole_info_by_state |>
  select(state, abolished_discretionary_parole)

# Prepare map data for displaying counts
map_percent_data <- filtered_parole_elig_table_analysis_year %>%

  # Add missing states
  complete(state = all_states) %>%

  # Add info about whether state abolished parole release
  left_join(parole_info_by_state_clean, by = "state") %>%

  mutate(state_abb = state.abb[match(state, state.name)],
         all_na = rowSums(is.na(select(.,
                                       current_count,
                                       future_1_5_years_count,
                                       missing_count))) ==
           length(select(.,
                         current_count,
                         future_1_5_years_count,
                         missing_count))
  )






#------ Save Data ------#


theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(filtered_parole_elig_table_analysis_year, file = file.path(folder, "filtered_parole_elig_table_analysis_year.rds"))

}


