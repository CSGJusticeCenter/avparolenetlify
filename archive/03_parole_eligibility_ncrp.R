# #######################################
# # Project: AV Parole
# # File: parole_eligibility_ncrp.R
# # Authors: Mari Roberts
# # Date last updated: May 23, 2023 (MAR)
# # Description:
# #    Parole eligibility tables and graphics for shiny app
# #######################################
#
#
# ##########
# # NCRP - Parole eligibility in 2020
# ##########
#
# # get number and percentage of eligibility statuses
# parole_eligibility_counts <- ncrp_yearendpop %>%
#   group_by(state, rptyear) %>%
#   count(parelig_status) %>%
#   mutate(
#     prop = n/sum(n),
#     yearendpop = sum(n)
#   ) %>%
#   ungroup()
#
# # reformat for table viewing
# parole_eligibility_table <- parole_eligibility_counts %>%
#   pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
#   mutate(name = case_when(
#     type == "n"    ~ paste(parelig_status, "count"),
#     type == "prop" ~ paste(parelig_status, "perc.")
#   )) %>%
#   select(state, rptyear, yearendpop, name, value) %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   clean_names()
#
# # filter to 2020
# parole_eligibility_table_2020 <- parole_eligibility_table %>%
#   filter(rptyear == 2020)
#
# # missing data
# # Arizona, Michigan, New Jersey, New Mexico
# missing_states <- state.name[!state.name %in% parole_eligibility_table_2020$state]
#
# # create a new dataframe with the missing states and NA values
# missing_data <- tibble(state = missing_states)
# missing_data <- missing_data %>% mutate(rptyear = 2020)
#
# # combine the missing data with the original dataframe
# parole_eligibility_table_2020 <- bind_rows(parole_eligibility_table_2020, missing_data) %>%
#   arrange(state)
#
#
#
#
#
#
#
# ##########
# # NCRP - Offenses for those in prison but not released in 2020
# ##########
#
# current_ped_2020_offenses <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status == "Current") %>%
#   filter(!is.na(offgeneral)) %>%
#   mutate(offgeneral = ifelse(
#     offgeneral == "Other/unspecified", "Other or Unspecified", offgeneral
#   )) %>%
#   group_by(state) %>%
#   count(offgeneral) %>%
#   mutate(
#     prop = n/sum(n)
#     , yearendpop_ped = sum(n)
#   ) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
#                   "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
#                   scales::comma(n), "</b><br><br>",
#                   "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
#                   paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
#          chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
#          prop_label = paste0(round(prop*100, 0), "%"))
#
# pop_2020_race <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status != "Missing") %>%
#   filter(!is.na(race)) %>%
#   group_by(state, race) %>%
#   count(race) %>%
#   select(state, race, total_prison_pop_by_race = n)
#
# current_ped_2020_race <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status == "Current") %>%
#   filter(!is.na(race)) %>%
#   group_by(state) %>%
#   count(race) %>%
#   mutate(
#     prop = n/sum(n),
#     yearendpop_ped = sum(n),
#     prop_label = paste0(round(prop*100, 0), "%")
#   ) %>%
#   ungroup() %>%
#   mutate(tooltip = paste0("<b>", state, " - ",
#                           race, "</b><br>",
#                           prop_label, "<br>"))
#
# current_ped_2020_race1 <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status == "Current") %>%
#   filter(!is.na(race)) %>%
#   group_by(state, race) %>%
#   count(race) %>%
#   rename(currently_eligible_for_parole = n) %>%
#   left_join(pop_2020_race, by = c("state", "race")) %>%
#   mutate(
#     prop = currently_eligible_for_parole/total_prison_pop_by_race,
#     prop_label = paste0(round(prop*100, 0), "%")
#   )
#
#
#
#
# ##########
# # NCRP - Parole eligibility by adm type and year
# ##########
#
# parole_eligibility_rate_by_admtype <- ncrp_yearendpop %>%
#   filter(!is.na(parelig_status) & !is.na(admtype) &
#            admtype != "Other admission (including unsentenced, transfer, AWOL/escapee return)") %>%
#   group_by(state, rptyear, parelig_status) %>%
#   count(admtype) %>%
#   mutate(
#     prop = n/sum(n),
#     yearendpop = sum(n),
#     prop = prop*100) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            case_when(admtype == "New court commitment" ~
#                        paste0("<b>", state, "</b><br>",
#                               "New court commitment:<br>",
#                               paste(round(prop, 1), "%</b>", sep = ""), "<br>"),
#                      admtype == "Parole return/revocation" ~
#                        paste0("<b>", state, "</b><br>",
#                               "Parole return/revocation:<br>",
#                               paste(round(prop, 1), "%</b>", sep = ""), "<br>")))
#
# # get number and percentage of eligibility statuses by adm type (new court committment by parole eligibility status)
# parole_eligibility_admtype_counts <- ncrp_yearendpop %>%
#   filter(admtype == "Parole return/revocation" |
#            admtype == "New court commitment") %>%
#   group_by(state, rptyear, admtype) %>%
#   count(parelig_status) %>%
#   mutate(
#     prop = n/sum(n),
#     yearendpop = sum(n)
#   ) %>%
#   ungroup()
#
# # reformat for table viewing
# parole_eligibility_admtype_table <- parole_eligibility_admtype_counts %>%
#   pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
#   mutate(name = case_when(
#     type == "n"    ~ paste(parelig_status, "count"),
#     type == "prop" ~ paste(parelig_status, "perc.")
#   )) %>%
#   select(state, rptyear, admtype, yearendpop, name, value) %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   clean_names()
#
# # filter to 2020
# parole_eligibility_admtype_table_2020 <- parole_eligibility_admtype_table %>%
#   filter(rptyear == 2020)
#
# parole_eligibility_admtype_table_2020 <- parole_eligibility_admtype_table_2020 %>%
#   pivot_wider(names_from = admtype,
#               values_from = c(yearendpop,
#                               missing_count,
#                               missing_perc,
#                               current_count,
#                               current_perc,
#                               future_1_5_years_count,
#                               future_1_5_years_perc,
#                               future_6_years_count,
#                               future_6_years_perc),
#               names_sep = "_")
#
# # missing data
# # Arizona, Michigan, New Jersey, New Mexico
# missing_states <- state.name[!state.name %in% parole_eligibility_admtype_table_2020$state]
#
# # create a new dataframe with the missing states and NA values
# missing_data <- tibble(state = missing_states)
# missing_data <- missing_data %>% mutate(rptyear = 2020)
#
# # combine the missing data with the original dataframe
# parole_eligibility_admtype_table_2020 <- bind_rows(parole_eligibility_admtype_table_2020, missing_data) %>%
#   arrange(state)
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
#   save(parole_eligibility_table,      file=file.path(folder, "parole_eligibility_table.rds"))
#   save(parole_eligibility_table_2020, file=file.path(folder, "parole_eligibility_table_2020.rds"))
#   save(current_ped_2020_offenses,     file=file.path(folder, "current_ped_2020_offenses.rds"))
#   save(current_ped_2020_race,         file=file.path(folder, "current_ped_2020_race.rds"))
#
# }
