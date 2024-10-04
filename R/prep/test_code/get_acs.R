# Import necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)
library(purrr)

race_vars <- c(
  pop_white_total  = "B02001_002E",
  pop_black_total  = "B02001_003E",
  pop_hispanic_total = "B03002_012E"
)

# Set gender variables
gender_vars <- c(
  pop_male_total = "B01001_002E",
  pop_female_total = "B01001_026E"
)

# Get state names, excluding states without parole
states <- state_notes |>
  filter(abolished_parole == "N", state %in% state.name) |>
  pull(state)

# Get ACS population data for race
get_acs_race_pop <- function(year) {
  get_acs(
    geography = "state",
    variables = race_vars,
    year = year,
    survey = "acs1"
  ) %>%
    clean_names() %>%
    mutate(
      race = case_when(
        variable == "B02001_002" ~ "White, non-Hispanic",
        variable == "B02001_003" ~ "Black, non-Hispanic",
        variable == "B03002_012" ~ "Hispanic, any race",
        TRUE ~ "NA"
      )
    )
}

# Get ACS population data for gender
get_acs_gender_pop <- function(year) {
  get_acs(
    geography = "state",
    variables = gender_vars,
    year = year,
    survey = "acs1"
  ) %>%
    clean_names() %>%
    mutate(
      gender = case_when(
        variable == "B01001_002" ~ "Male",
        variable == "B01001_026" ~ "Female",
        TRUE ~ "NA"
      )
    )
}

# Fetch race data for all states and bind rows
census_state_race_population_list <- lapply(states, function(state) get_acs_race_pop(2019))
census_state_race_population <- bind_rows(census_state_race_population_list)
census_state_race_population <- census_state_race_population |> rename(state = name)

# Fetch gender data for all states and bind rows
census_state_gender_population_list <- lapply(states, function(state) get_acs_gender_pop(2019))
census_state_gender_population <- bind_rows(census_state_gender_population_list)
census_state_gender_population <- census_state_gender_population |> rename(state = name)

# Process race data: group and summarize
census_state_race_population <- census_state_race_population |>
  group_by(state, race) |>
  summarise(state_population = sum(estimate, na.rm = TRUE), .groups = "drop")

# Process gender data: group and summarize
census_state_gender_population <- census_state_gender_population |>
  group_by(state, gender) |>
  summarise(state_population = sum(estimate, na.rm = TRUE), .groups = "drop")

# Merge race population data with prison population data
merged_race_data <- census_state_race_population %>%
  inner_join(bjs_prison_pop_by_race_2020, by = c("state", "race")) %>%
  rename(prison_population = n)

# Merge gender population data with prison population data
merged_gender_data <- census_state_gender_population %>%
  inner_join(bjs_prison_pop_by_sex_2022, by = c("state", "gender" = "sex")) %>%   # CHANGE TO 2020 data ???????????????????????
  rename(prison_population = n)

# Calculate incarceration rate per 100,000 for race
merged_race_data <- merged_race_data %>%
  mutate(incarceration_rate = prison_population / state_population * 100000)

# Calculate incarceration rate per 100,000 for gender
merged_gender_data <- merged_gender_data %>%
  mutate(incarceration_rate = prison_population / state_population * 100000)

# Calculate RRI for race
reference_race_rate <- merged_race_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, incarceration_rate) %>%
  rename(reference_rate = incarceration_rate)

rri_race_data <- merged_race_data %>%
  inner_join(reference_race_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate) %>%
  select(state, race, rri)

# Calculate RRI for gender
reference_gender_rate <- merged_gender_data %>%
  filter(gender == "Male") %>%
  select(state, incarceration_rate) %>%
  rename(reference_rate = incarceration_rate)

rri_gender_data <- merged_gender_data %>%
  inner_join(reference_gender_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate) %>%
  select(state, gender, rri)

# View the resulting dataframes
rri_race_data
rri_gender_data
