fips_state_lookup <- fips_codes %>%
  distinct(state_abb = state, state_fips = state_code) %>%
  mutate(
    state = state.name[match(state_abb, state.abb)],
    state = case_when(
      state_abb == "DC" ~ "District of Columbia",
      state_abb == "AS" ~ "American Samoa",
      state_abb == "GU" ~ "Guam",
      state_abb == "MP" ~ "Northern Mariana Islands",
      state_abb == "PR" ~ "Puerto Rico",
      state_abb == "UM" ~ "United States Minor Outlying Islands",
      state_abb == "VI" ~ "United States Virgin Islands",
      TRUE ~ state
    )
  )

age_vars <- paste0("B01001_", str_pad(c(7:25, 31:49), width = 3, pad = "0"))

get_acs_total_pop <- function(year, geography) {
  get_acs(
    geography = geography,
    variables = "B01003_001",
    survey = "acs1",
    year = year
  ) %>%
    transmute(year = year, state_fips = GEOID, pop_total = estimate)
}

get_acs_adult_pop <- function(year) {
  ret <- get_acs(
    geography = "state",
    variables = age_vars,
    survey = "acs1",
    year = year
  ) %>%
    group_by(year = year, state_fips = GEOID) %>%
    summarize(pop_adult = sum(estimate)) %>%
    ungroup()
}

get_acs_total_pop_gender <- function(year) {
  get_acs(
    geography = "state",
    variables = c("B01001_002E", "B01001_026E"),
    survey = "acs1",
    year = year,
    output = "wide"
  ) %>%
    transmute(
      year = year, state_fips = GEOID,
      pop_male_total  = B01001_002E,
      pop_female_total  = B01001_026E
    )
}

get_acs_adult_pop_gender <- function(year) {
  get_acs(
    geography = "state",
    variables = age_vars,
    survey = "acs1",
    year = year
  ) %>%
    mutate(gender = if_else(as.numeric(str_sub(variable, 9)) <= 25, "Male", "Female")) %>%
    group_by(year = year, state_fips = GEOID) %>%
    summarize(
      pop_male_adult = sum(estimate[gender == "Male"]),
      pop_female_adult = sum(estimate[gender == "Female"])
    ) %>%
    ungroup()
}

acs_years <- c(2005:2009, 2011:2019)

acs_total_pop_by_state <- map_dfr(acs_years, get_acs_total_pop)
acs_adult_pop_by_state <- map_dfr(acs_years, get_acs_adult_pop)
acs_total_pop_gender_by_state <- map_dfr(acs_years, get_acs_total_pop_gender)
acs_adult_pop_gender_by_state <- map_dfr(acs_years, get_acs_adult_pop_gender)

acs_pop_by_state <- acs_total_pop_by_state %>%
  left_join(acs_adult_pop_by_state, by = c("year", "state_fips")) %>%
  left_join(acs_total_pop_gender_by_state, by = c("year", "state_fips")) %>%
  left_join(acs_adult_pop_gender_by_state, by = c("year", "state_fips"))

state_pop_2010 <- get_decennial(
  geography = "state",
  variables = c("P010001", "P001001"),
  year = 2010,
  sumfile = "sf1",
  output = "wide"
) %>%
  transmute(year = 2010, state_fips = GEOID, pop_total = P001001, pop_adult = P010001)

state_pop_gender_2010 <- get_decennial(
  geography = "state",
  variables = c("P012002", "P012026"),
  year = 2010,
  sumfile = "sf1",
  output = "wide"
) %>%
  transmute(
    year = 2010,
    state_fips = GEOID,
    pop_male_total  = P012002,
    pop_female_total  = P012026
  )

age_vars_2010 <- paste0("P012", str_pad(c(7:25, 31:49), width = 3, pad = "0"))

state_pop_adult_gender_2010 <- get_decennial(
  geography = "state",
  variables = age_vars_2010,
  year = 2010,
  sumfile = "sf1"
) %>%
  mutate(gender = if_else(as.numeric(str_sub(variable, 5)) <= 25, "Male", "Female")) %>%
  group_by(year = 2010, state_fips = GEOID) %>%
  summarize(
    pop_male_adult = sum(value[gender == "Male"]),
    pop_female_adult = sum(value[gender == "Female"])
  ) %>%
  ungroup()

state_pop_2010_joined <- state_pop_2010 %>%
  left_join(state_pop_gender_2010, by = c("year", "state_fips")) %>%
  left_join(state_pop_adult_gender_2010, by = c("year", "state_fips"))

state_pop_2020 <- get_decennial(
  geography = "state",
  variables = c("P3_001N", "P1_001N"),
  year = 2020,
  sumfile = "pl",
  output = "wide"
) %>%
  transmute(year = 2020, state_fips = GEOID, pop_total = P1_001N, pop_adult = P3_001N)

total_gender_pct <- acs_total_pop_gender_by_state %>%
  filter(year == 2019) %>%
  group_by(state_fips) %>%
  summarize(male_total_pct = pop_male_total / (pop_male_total + pop_female_total))

adult_gender_pct <- acs_adult_pop_gender_by_state %>%
  filter(year == 2019) %>%
  group_by(state_fips) %>%
  summarize(male_adult_pct = pop_male_adult / (pop_male_adult + pop_female_adult))

state_pop_gender_2020 <- state_pop_2020 %>%
  left_join(total_gender_pct, by = "state_fips") %>%
  left_join(adult_gender_pct, by = "state_fips") %>%
  mutate(
    pop_male_total = as.integer(pop_total * male_total_pct),
    pop_female_total = pop_total - pop_male_total,
    pop_male_adult = as.integer(pop_adult * male_adult_pct),
    pop_female_adult = pop_adult - pop_male_adult
  ) %>%
  select(year, state_fips, contains("male"), -contains("pct"))

state_pop_2020_joined <- state_pop_2020 %>%
  left_join(state_pop_gender_2020, by = c("year", "state_fips"))

state_pop_gender_joined <- acs_pop_by_state %>%
  bind_rows(state_pop_2010_joined) %>%
  bind_rows(state_pop_2020_joined) %>%
  arrange(state_fips, year)

get_acs_total_pop_race <- function(year) {
  get_acs(
    geography = "state",
    table = "B02001",
    survey = "acs1",
    year = year,
    output = "wide",
    cache_table = TRUE
  ) %>%
    transmute(
      year = year, state_fips = GEOID,
      pop_white_total  = B02001_002E,
      pop_black_total  = B02001_003E,
      pop_american_indian = B02001_004E,
      pop_asian_total  = B02001_005E + B02001_006E
    )
}

acs_total_pop_race_by_state <- map_dfr(acs_years, get_acs_total_pop_race)

state_pop_race_2010 <- get_decennial(
  geography = "state",
  variables = c("P001003", "P001004", "P001005", "P001006", "P001007"),
  year = 2010,
  sumfile = "pl",
  output = "wide"
) %>%
  transmute(
    year = 2010,
    state_fips = GEOID,
    pop_white_total  = P001003,
    pop_black_total  = P001004,
    pop_american_indian = P001005,
    pop_asian_total  = P001006 + P001007
  )

state_pop_race <- state_pop_race_2010 %>%
  bind_rows(acs_total_pop_race_by_state) %>%
  add_row(year = 2020, state_fips = unique(state_pop_race_2010$state_fips)) %>%
  arrange(state_fips, year) %>%
  group_by(state_fips) %>%
  fill(starts_with("pop"), .direction = "down") %>%
  ungroup()


get_acs_total_pop_race_eth <- function(year) {
  acs_race_eth_pop <- get_acs(
    geography = "state",
    table = "C03002",
    survey = "acs1",
    year = year,
    output = "wide",
    cache_table = TRUE
  ) %>%
    transmute(
      year = year, state_fips = GEOID,
      pop_white_total     = C03002_003E,
      pop_black_total     = C03002_004E,
      pop_hispanic_total  = C03002_012E,
      pop_american_indian = C03002_005E,
      pop_asian_total     = C03002_006E + C03002_007E,
      pop_other_total     = C03002_008E + C03002_009E,
    )
}

acs_total_pop_race_eth_by_state <- map_dfr(2005:2019, get_acs_total_pop_race_eth)

state_pop_joined <- state_pop_gender_joined %>%
  left_join(state_pop_race, by = c("year", "state_fips"))
