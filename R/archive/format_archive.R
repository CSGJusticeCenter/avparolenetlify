#######################################
# Project: AV Parole
# File: format.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Format data files for analysis and website visualizations
#######################################

#######################################

# Format Data

#######################################

#------ Prepare NCRP Term Records ------#

ncrp_term_records <- da38492.0001 %>%
  clean_names() %>%
  mutate(across(state:agerelease, ~ str_sub(., 5, -1))) %>%
  mutate(across(everything(), trimws))



#------ Prepare NCRP Admissions ------#

ncrp_admissions <- da38492.0002 %>%
  clean_names() %>%
  mutate(across(c(state, sex, education, admtype, offgeneral, offdetail, race,
                  sentlgth, ageadmit), ~ str_sub(., 5, -1))) %>%
  mutate(offdetail = trimws(offdetail)) %>%
  fnc_create_admtype()



#------ Prepare NCRP Releases ------#

ncrp_releases <- da38492.0003 %>%
  clean_names() %>%
  mutate(across(c(state, offgeneral, offdetail, admtype, race, sex, ageadmit,
                  agerlse, sentlgth, reltype, timesrvd_rel, education), ~ str_sub(., 5, -1))) %>%
  mutate(offdetail = trimws(offdetail)) %>%
  fnc_create_parelig_status() %>%
  fnc_create_fbi_index() %>%
  mutate(
    time_between_admisson_release = relyr - admityr,
    time_between_ped_release = relyr - parelig_year,
    time_between_ped_release_category = case_when(
      time_between_ped_release < 0    ~ "Released before Parole Eligibility Year",
      time_between_ped_release == 0   ~ "Released on Parole Eligibility Year",
      time_between_ped_release <= 5 &
        time_between_ped_release > 0  ~ "Released 1 to 5 Years After Parole Eligibility Year",
      time_between_ped_release > 5    ~ "Released more than 5 Years After Parole Eligibility Year",
      is.na(time_between_ped_release) ~ "Missing Parole Eligibility Year") %>%
      factor(levels = c("Released before Parole Eligibility Year",
                        "Released on Parole Eligibility Year",
                        "Released 1 to 5 Years After Parole Eligibility Year",
                        "Released more than 5 Years After Parole Eligibility Year",
                        "Missing Parole Eligibility Year"))) %>%
  fnc_create_admtype() %>%
  mutate(across(c(race, agerlse, sentlgth), ~ ifelse(is.na(.), "Unknown", .))) %>%
  mutate(
    race = factor(race, levels = c("Unknown",
                                   "Other race(s), non-Hispanic",
                                   "White, non-Hispanic",
                                   "Hispanic, any race",
                                   "Black, non-Hispanic")),
    agerlse = factor(agerlse, levels = c("55+ years",
                                         "45-54 years",
                                         "35-44 years",
                                         "25-34 years",
                                         "18-24 years")),
    sentlgth = factor(sentlgth, levels = c("< 1 year",
                                           "1-1.9 years",
                                           "2-4.9 years",
                                           "5-9.9 years",
                                           "10-24.9 years",
                                           ">=25 years",
                                           "Life, LWOP, Life plus additional years, Death",
                                           "Unknown")))



#------ Prepare NCRP Year End Population ------#

ncrp_yearendpop <- da38492.0004 %>%
  clean_names() %>%
  mutate(across(c(state, offgeneral, offdetail, race, education, admtype, sex,
                  sentlgth, ageadmit, ageyrend, timesrvd_yrend), ~ str_sub(., 5, -1))) %>%
  mutate(
    offdetail = trimws(offdetail),
    offgeneral = case_when(is.na(offgeneral) ~ "Other or Unknown",
                           offgeneral == "Other/unspecified" ~ "Other or Unknown",
                           TRUE ~ offgeneral)) %>%
  fnc_create_fbi_index() %>%
  fnc_create_parelig_status() %>%
  fnc_create_admtype() %>%
  mutate(across(c(race, ageyrend, sentlgth), ~ ifelse(is.na(.), "Unknown", .))) %>%
  mutate(
    race = factor(race, levels = c("Unknown",
                                   "Other race(s), non-Hispanic",
                                   "White, non-Hispanic",
                                   "Hispanic, any race",
                                   "Black, non-Hispanic")),
    ageyrend = factor(ageyrend, levels = c("55+ years",
                                           "45-54 years",
                                           "35-44 years",
                                           "25-34 years",
                                           "18-24 years")),
    sentlgth = factor(sentlgth, levels = c("< 1 year",
                                           "1-1.9 years",
                                           "2-4.9 years",
                                           "5-9.9 years",
                                           "10-24.9 years",
                                           ">=25 years",
                                           "Life, LWOP, Life plus additional years, Death",
                                           "Unknown")))



#------ Prepare BJS: Prisoners in 2020 ------#

total_pop <- bjs_prison_pop_by_race_state_2020 %>%
  clean_names() %>%
  filter(jurisdiction == "") %>%
  select(x, total) %>%
  rename(state = x) %>%
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

bjs_prison_pop_by_race <- bjs_prison_pop_by_race_state_2020 %>%
  clean_names() %>%
  filter(jurisdiction == "") %>%
  select(-jurisdiction) %>%
  rename(state = x) %>%
  mutate(across(everything(), ~str_replace_all(., ",", ""))) %>%
  mutate(across(-state, as.numeric)) %>%
  pivot_longer(cols = total:did_not_report,
               names_to = "race",
               values_to = "n") %>%
  mutate(race = case_when(
    race == "total" ~ "Total Population",
    race == "white_a" ~ "White, non-Hispanic",
    race == "black_a" ~ "Black, non-Hispanic",
    race == "hispanic" ~ "Hispanic, any race",
    race %in% c("american_indian_alaska_native_a",
                "asian_a",
                "native_hawaiian_other_pacific_islander_a",
                "two_or_more_races_a",
                "other_a") ~ "Other race(s), non-Hispanic",
    race == "unknown" ~ "Unknown",
    race == "did_not_report" ~ "Unknown",
    TRUE ~ race
  )) %>%
  filter(race != "Unknown" & race != "Total Population") %>%
  group_by(state, race) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  left_join(total_pop, by = "state") %>%
  ungroup() %>%
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") %>%
  select(-total)



#------ Prepare BJS: Prisoners from 2010-2021 ------#

bjs_prison_pop_by_state <- bind_rows(lapply(names(file_info), function(year) {
  info <- file_info[[year]]
  fnc_load_bjs_prison_data(year, info$subfolder, info$file_name) %>%
    clean_names() %>%
    select(state = x, bjs_prison_population = x_5) %>%
    fnc_clean_bjs_data() %>%
    mutate(rptyear = as.numeric(year))
}))



#------ Prepare shapefile for map ------#

hex_gj <- hex %>%
  st_transform(3857) %>%
  sf_geojson() %>%
  fromJSON(simplifyVector = FALSE)



#------ Prepare parole info by state for map ------#

parole_info_by_state <- parole_info_by_state %>%
  clean_names()



#------ Prepare BJS Annual Parole Survey ------#

state_names_abb <- data.frame(abbreviation = state.abb,
                              name = state.name,
                              stringsAsFactors = FALSE) %>%
  rename(state = name, stateid = abbreviation)

aps_data_list <- lapply(1:nrow(aps_data_info), function(i) {
  fnc_prepare_aps_data(get(paste0("da", aps_data_info$icpsr_code[i], ".0001")), aps_data_info$year[i], aps_data_info$year[i] < 2008)
})

aps_parole_2000_2018 <- bind_rows(aps_data_list) %>%
  filter(!state %in% c("District of Columbia", "Federal") & !is.na(state))



#######################################

# Save Data

#######################################


#------ Save data to SharePoint ------#

save_data <- function(data, name) {
  save(data, file = file.path(sp_data_path, "data/analysis/app", paste0(name, ".rds")))
}

save_data(parole_info_by_state, "parole_info_by_state")

save_data(robinadefinitions, "robinadefinitions")
save_data(robinainfo, "robinainfo")
save_data(robinaparoleeligibility, "robinaparoleeligibility")

save_data(hex_gj, "hex_gj")

save_data(ncrp_yearendpop,   "ncrp_yearendpop")
save_data(ncrp_admissions,   "ncrp_admissions")
save_data(ncrp_term_records, "ncrp_term_records")
save_data(ncrp_releases,     "ncrp_releases")

save_data(aps_parole_2000_2018, "aps_parole_2000_2018")

save_data(bjs_prison_pop_by_race,  "bjs_prison_pop_by_race")
save_data(bjs_prison_pop_by_state, "bjs_prison_pop_by_state")









