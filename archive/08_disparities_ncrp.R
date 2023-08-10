#
# library(tidycensus)
# library(tidyverse)
# library(dplyr)
# library(scales)
# options(dplyr.summarise.inform = FALSE)
#
# # Make sure you have a Census API Key
# # http://api.census.gov/data/key_signup.html and then supply the key to the
# # `census_api_key("YOUR KEY HERE", install = TRUE)` which will add it to your Renviron
#
# # pull census/acs API key
# readRenviron("~/.Renviron")
#
#
#
#
# ##########################
# # Get prison population data by race and state
# ##########################
#
# # warning message: NA's will be created for text that isnt numeric
# state_prison_pop <- prison_pop_by_race_state_2020 %>%
#   pivot_longer(cols = c(total:did_not_report), names_to = "race", values_to = "prison_pop") %>%
#   mutate(prison_pop = as.numeric(prison_pop)) %>%
#   filter(race != "total" &
#            race != "did_not_report" &
#            race != "other_a" &
#            race != "unknown" &
#            race != "two_or_more_races_a") %>%
#   mutate(race = case_when(
#     race == "white_a"                                  ~ "White",
#     race == "black_a"                                  ~ "Black",
#     race == "hispanic"                                 ~ "Hispanic",
#     race == "american_indian_alaska_native_a"          ~ "American Indian/Alaska Native",
#     race == "asian_a"                                  ~ "Asian/Pacific Islander",
#     race == "native_hawaiian_other_pacific_islander_a" ~ "Asian/Pacific Islander"
#   )) %>%
#   group_by(state, race) %>%
#   summarise(prison_pop = sum(prison_pop)) %>%
#   mutate(state = case_when(
#     state == "Maryland/d"      ~ "Maryland",
#     state == "Michigan/d"      ~ "Michigan",
#     state == "Montana/e"       ~ "Montana",
#     state == "Nevada/d"        ~ "Nevada",
#     state == "New Mexico/f"    ~ "New Mexico",
#     state == "Ohio/g"          ~ "Ohio",
#     state == "Pennsylvania/d"  ~ "Pennsylvania",
#     state == "Rhode Island/dh" ~ "Rhode Island",
#     state == "Virginia/ci"     ~ "Virginia",
#     TRUE ~ state
#   ))
#
#
#
# ##########################
# # Get census data
# ##########################
#
# # weighted estimate of percentage of race from 2020 census
# # pulled estimated counts and construct percent estimate
# # these are the ids of race variables that we want to pull
# race_vars <- c(estimate_white              = "P3_003N",
#                estimate_black              = "P3_004N",
#                estimate_asian              = "P3_006N",
#                estimate_native_hawaiian_pi = "P3_007N",
#                estimate_hispanic           = "P4_002N",
#                estimate_american_indian    = "P1_005N")
#
# # create empty dataframe
# race_eth_rri_table <- data.frame()
#
# # get states
# state_loop <- unique(state_prison_pop$state)
#
# # get rii by state and add all state rii data together
# for(i in state_loop) {
#
#   state_race_2020 <-
#     tidycensus::get_decennial(geography = "state",
#                               state = i,
#                               variables = race_vars,
#                               # total population for 18+ population from race table
#                               summary_var = "P3_001N",
#                               year = 2020,
#                               geometry = FALSE) %>%
#     clean_names() %>%
#     select(-geoid) %>%
#
#     # rename race levels
#     mutate(race = case_when(
#       variable == "estimate_american_indian"         ~ "American Indian/Alaska Native",
#       variable %in% c("estimate_asian",
#                       "estimate_native_hawaiian_pi") ~ "Asian/Pacific Islander",
#       variable == "estimate_black"                   ~ "Black",
#       variable == "estimate_hispanic"                ~ "Hispanic",
#       variable == "estimate_white"                   ~ "White",
#       TRUE ~ "NA")) %>%
#     group_by(race) %>%
#     mutate(statewide_overall_value = sum(value)) %>%
#     ungroup() %>%
#     group_by(variable) %>%
#
#     # create statewide count estimate
#     mutate(statewide_summary_value = sum(summary_value)) %>%
#     ungroup() %>%
#     distinct(race, .keep_all = TRUE) %>%
#
#     #create percent estimate from estimated counts
#     mutate(estimate = (statewide_overall_value/statewide_summary_value)*100) %>%
#     ungroup() %>%
#     rename(state_race_pop         = statewide_overall_value,
#            total_pop              = statewide_summary_value,
#            state_race_pop_percent = estimate) %>%
#     select(-c(variable,value)) %>%
#     distinct()
#
#   # prep census data for join and rri calculation
#   state_race_2020 <- state_race_2020 %<>%
#     ungroup() %>%
#     dplyr::select(state = name,
#                   race,
#                   state_race_pop)
#
#   state_prison_pop_sub <- state_prison_pop %>%
#     filter(state == i)
#
#   # join two files
#   state_rii <- left_join(state_prison_pop_sub, state_race_2020, by = c("state", "race"))
#
#   # white, non-hispanic only for rri calculation
#   race_eth_rri_white <- state_rii %>%
#     filter(!is.na(race) == TRUE,
#            race == "White") %>%
#     dplyr::summarise(individuals_prison_per_10k = round(prison_pop/mean(state_race_pop)*10000, digits = 1),
#               individuals_prison_rii_to_white = NA) %>%
#     dplyr::mutate(race_ethnicity = "White")
#
#   # now calculate rates and RRIs using above dataframe (for White individuals) as reference
#   race_eth_rri_poc <- state_rii %>%
#     dplyr::filter(!is.na(race)==TRUE,
#                   race %in% c("Asian/Pacific Islander",
#                               "Black",
#                               "Hispanic",
#                               "American Indian/Alaska Native")) %>%
#     dplyr::group_by(race, state) %>%
#     dplyr::summarise(individuals_prison_per_10k =
#                 round(prison_pop/mean(state_race_pop)*10000, digits = 1),
#
#               individuals_prison_rii_to_white =
#                 round(individuals_prison_per_10k/race_eth_rri_white$individuals_prison_per_10k,
#                       digits = 2)) %>%
#     ungroup() %>%
#     dplyr::rename(race_ethnicity = race)
#
#   # bind tables together
#   output <- rbind(race_eth_rri_poc, race_eth_rri_white)
#
#   # add output to previous outputs of states into one dataframe
#   race_eth_rri_table = rbind(race_eth_rri_table, output)
#
# }
#
#
#
#
# ##########
# # Save data
# ##########
#
# theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))
#
# for (folder in theseFOLDERS){
#
#   save(race_eth_rri_table, file=file.path(folder, "race_eth_rri_table.rds"))
#
# }
