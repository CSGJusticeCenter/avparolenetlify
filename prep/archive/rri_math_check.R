
# # load NCRP year end population
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_yearendpop.rds"))

##########################
# Get census data
##########################


# weighted estimate of percentage of race from 2020 census
# pulled estimated counts and construct percent estimate
# these are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P3_003N",
               estimate_black              = "P3_004N",
               estimate_asian              = "P3_006N",
               estimate_native_hawaiian_pi = "P3_007N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P1_005N")

# get list of states
states <- state.name

# define  function to retrieve and process census data for a given state
fnc_get_census_data <- function(state) {
  census_race_data <- tidycensus::get_decennial(
    geography = "state",
    state = state,
    variables = race_vars,
    summary_var = "P3_001N",
    year = 2020,
    geometry = FALSE
  ) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race_eth = case_when(
        variable %in% c("estimate_american_indian", "estimate_asian", "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    ) %>%
    filter(race_eth != "Other race(s), non-Hispanic") %>%
    group_by(race_eth) %>%
    summarise(race_eth_pop = sum(value)) %>%
    ungroup() %>%
    mutate(total_pop = sum(race_eth_pop)
           # estimate = (race_eth_pop / total_pop) * 100
    )

  return(census_race_data)
}

# use lapply to retrieve and process data for each state
census_data_list <- lapply(states, fnc_get_census_data)

# convert the list of tibbles into a dataframe
census_data_df <- bind_rows(census_data_list)

# add the "state" column to the final dataframe
census_data_df$state <- rep(states, each = nrow(census_data_df) / length(states))

# get list of states in NCRP data
states <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  pull(state) %>%
  unique()








# filter to state
census_race_2020 <- census_data_df %>%
  filter(state == x) %>%
  ungroup() %>%
  dplyr::select(race_eth, race_eth_pop)


# WHITE
rri_analytic_white <- ncrp_yearendpop %>%
  filter(rptyear == 2020 &
           state == x &
           race != "Other race(s), non-Hispanic") %>%
  mutate(unique_id = row_number()) %>%
  rename(race_eth = race) %>%
  filter(!is.na(race_eth) == TRUE,
         race_eth == "White, non-Hispanic") %>%
  summarise(people_in_prison =n_distinct(unique_id[sentlgth == "< 1 year"])) %>%
  mutate(race_eth = "White, non-Hispanic") %>%
  inner_join(census_race_2020, by = "race_eth") %>%
  mutate(rate = people_in_prison/race_eth_pop)

# select white rate
white_rate <- rri_analytic_white %>%
  select(white_rate = rate) %>%
  pull(white_rate)

# BLACK, HISPANIC
rri_analytic_black_hispanic <- ncrp_yearendpop %>%
  filter(rptyear == 2020 &
           state == x &
           race != "Other race(s), non-Hispanic") %>%
  mutate(unique_id = row_number()) %>%
  rename(race_eth = race) %>%
  filter(!is.na(race_eth) == TRUE,
         race_eth %in% c("Black, non-Hispanic","Hispanic, any race")) %>%
  group_by(race_eth) %>%
  summarise(people_in_prison =n_distinct(unique_id[sentlgth == "< 1 year"])) %>%
  inner_join(census_race_2020, by = "race_eth") %>%
  mutate(rate = people_in_prison/race_eth_pop) %>%
  mutate(rri = rate/white_rate)





