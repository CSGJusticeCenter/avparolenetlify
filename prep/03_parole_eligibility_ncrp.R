#######################################
# Project: AV Parole
# File: parole_eligibility_ncrp.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Parole eligibility tables and graphics for shiny app
#######################################

##########
# Parole eligibility in 2020
##########

# filter to 2020 data
parole_elgibility_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020)

# get number and percentage of eligibility statuses
parole_eligibility_counts_2020 <- parole_elgibility_2020 %>%
  group_by(state) %>%
  count(parelig_status) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n)
  ) %>%
  ungroup()

# reformat for table viewing
parole_eligibility_table_2020 <- parole_eligibility_counts_2020 %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, yearendpop, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()
  # select(-c(missing_count, missing_perc))

# missing data
# Arizona, Michigan, New Jersey, New Mexico
parole_eligibility_missing_states_2020 <-
  paste(state.name[!state.name %in% parole_eligibility_table_2020$state], collapse = ", ")





##########
# Offenses for those in prison but not released in 2020
##########

current_ped_2020_offenses <- parole_elgibility_2020 %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(offgeneral)) %>%
  group_by(state) %>%
  count(offgeneral) %>%
  mutate(
    prop = n/sum(n)
    , yearendpop_ped = sum(n)
  ) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
                  "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
                  scales::comma(n), "</b><br><br>",
                  "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"))

pop_2020_race <- parole_elgibility_2020 %>%
  filter(parelig_status != "Missing") %>%
  filter(!is.na(race)) %>%
  group_by(state, race) %>%
  count(race) %>%
  select(state, race, total_prison_pop_by_race = n)

current_ped_2020_race <- parole_elgibility_2020 %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(race)) %>%
  group_by(state) %>%
  count(race) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%")
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          race, "</b><br>",
                          prop_label, "<br>"))

current_ped_2020_race1 <- parole_elgibility_2020 %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(race)) %>%
  group_by(state, race) %>%
  count(race) %>%
  rename(currently_eligible_for_parole = n) %>%
  left_join(pop_2020_race, by = c("state", "race")) %>%
  mutate(
    prop = currently_eligible_for_parole/total_prison_pop_by_race,
    prop_label = paste0(round(prop*100, 0), "%")
  )





##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table_2020,           file=file.path(folder, "parole_eligibility_table_2020.rds"))
  save(current_ped_2020_offenses,               file=file.path(folder, "current_ped_2020_offenses.rds"))
  save(current_ped_2020_race,                   file=file.path(folder, "current_ped_2020_race.rds"))

}
