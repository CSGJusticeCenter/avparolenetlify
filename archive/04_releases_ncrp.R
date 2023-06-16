#######################################
# Project: AV Parole
# File: releases_ncrp.R
# Authors: Mari Roberts
# Date last updated: May 22, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for app
#######################################

#
#
# ncrp_sentlgth_timesrvd_rel <- ncrp_releases %>%
#
#   # create order for sentence length and time served length
#   # for example, <1 is 1 and 1-1.9 is 2, and so on
#   mutate(
#     sentlgth_order = case_when(
#       sentlgth == "< 1 year"      ~ 1,
#       sentlgth == "1-1.9 years"   ~ 2,
#       sentlgth == "2-4.9 years"   ~ 3,
#       sentlgth == "5-9.9 years"   ~ 4,
#       sentlgth == "10-24.9 years" ~ 5,
#       sentlgth == ">=25 years"    ~ 5,
#       sentlgth == "Life, LWOP, Life plus additional years, Death" ~ 5,
#       TRUE ~ NA),
#     timesrvd_rel_order = case_when(
#       timesrvd_rel == "< 1 year"      ~ 1,
#       timesrvd_rel == "1-1.9 years"   ~ 2,
#       timesrvd_rel == "2-4.9 years"   ~ 3,
#       timesrvd_rel == "5-9.9 years"   ~ 4,
#       timesrvd_rel == ">=10 years"    ~ 5,
#       TRUE ~ NA)) %>%
#
#   # determine differences between time served and sentenced length
#   # calculate actual time served
#   mutate(
#     timesrvd_rel_vs_sentlgth = case_when(
#       is.na(timesrvd_rel_order) | is.na(sentlgth_order) ~ NA,
#       timesrvd_rel_order == sentlgth_order ~ "Full Sentence Length Served",
#       timesrvd_rel_order > sentlgth_order  ~ "More than Sentence Length Served",
#       timesrvd_rel_order < sentlgth_order  ~ "Less than Sentence Length Served"),
#     time_served = relyr - admityr) %>%
#
#   # https://www.icpsr.umich.edu/web/NACJD/studies/38492/datasets/0003/variables/PARELIG_YEAR?archive=nacjd
#   # remove parelig_year/mand_prisrel_year 2100
#   mutate(parelig_year_clean =
#            ifelse(parelig_year <= 2105, parelig_year, NA),
#          mand_prisrel_year_clean =
#            ifelse(mand_prisrel_year <= 2105, mand_prisrel_year, NA),
#
#          time_between_release_ped = relyr - parelig_year_clean,
#          time_between_ped_admission = parelig_year_clean - admityr,
#          time_between_mandatoryrelease_release = mand_prisrel_year_clean - relyr,
#          time_between_release_admissions = relyr - admityr) %>%
#
#   mutate(released_at_ped_status = case_when(
#     time_between_release_ped < 0 ~ "Released Before Parole Eligibility Year",
#     time_between_release_ped == 0 ~ "Released on Parole Eligibility Year",
#     time_between_release_ped > 0 ~ "Released After Parole Eligibility Year",
#     is.na(time_between_release_ped) ~ NA))
#
#
#
#
#
# ########################################
#
# # Released to Parole Over Time
#
# ########################################
#
# # count number of people released to parole by year and state
# ncrp_released_to_parole <- ncrp_sentlgth_timesrvd_rel %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   group_by(rptyear, state) %>%
#   summarise(releases_to_parole = n())









########################################

# Releases in 2020

# How many people are being released at first eligibility?
# How long after eligibility does release occur?
# How does release vary by the person's demographic and criminal history characteristics?
# What is the mean and median time between parole eligibility and release for those released after the PED, by maximum sentence length?

########################################


# # Subset to 2020 report
# ncrp_releases_2020 <- ncrp_sentlgth_timesrvd_rel %>%
#   filter(rptyear == 2020)
# # filter(!is.na(admityr) &
# # !is.na(parelig_year_clean) &
# # !is.na(mand_prisrel_year_clean) &
# # !is.na(relyr)) # removes a lot of data
#
# # How many people are being released at first eligibility?
# ncrp_released_at_ped_2020 <- ncrp_releases_2020 %>%
#   # remove states with NA's
#   filter(!is.na(released_at_ped_status) & state != "Illinois") %>%
#   group_by(state) %>%
#   count(released_at_ped_status) %>%
#   mutate(prop = n/sum(n),
#          prop_label = paste0(round(prop*100, 0), "%"),
#          chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Timing of Release: <b>",
#                   released_at_ped_status,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))
#
# # How many people are being released at first eligibility by adm type?
# ncrp_released_at_ped_admtype_2020 <- ncrp_releases_2020 %>%
#   # remove states with NA's
#   filter(!is.na(released_at_ped_status) &
#          !is.na(admtype) &
#            state != "Illinois") %>%
#   group_by(state, admtype) %>%
#   count(released_at_ped_status) %>%
#   mutate(prop = n/sum(n),
#          prop_label = paste0(round(prop*100, 0), "%"),
#          chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Timing of Release: <b>",
#                   released_at_ped_status,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))
#
# # How many people are being released at first eligibility by offgeneral?
# ncrp_released_at_ped_offgeneral_2020 <- ncrp_releases_2020 %>%
#   # remove states with NA's
#   filter(!is.na(released_at_ped_status) &
#            !is.na(offgeneral) &
#            state != "Illinois") %>%
#   group_by(state, offgeneral) %>%
#   count(released_at_ped_status) %>%
#   mutate(prop = n/sum(n),
#          prop_label = paste0(round(prop*100, 0), "%"),
#          chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Timing of Release: <b>",
#                   released_at_ped_status,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))





# ########################################
#
# # Profile of People on Parole
#
# ########################################
#
# # Get people on parole characteristics (race)
# ncrp_people_released_early_race <- ncrp_releases_2020 %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(!is.na(race)) %>%
#   filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   group_by(state, race) %>%
#   summarise(total_race = n()) %>%
#   mutate(total_releases = sum(total_race, na.rm = TRUE),
#          prop = total_race/total_releases)
#
#
# # Get people on parole characteristics (sex)
# ncrp_people_released_early_sex <- ncrp_releases_2020 %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(!is.na(sex)) %>%
#   # filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   group_by(state, sex) %>%
#   summarise(total_sex = n()) %>%
#   mutate(total_releases = sum(total_sex, na.rm = TRUE),
#          prop = total_sex/total_releases)
#
#
# # Get people on parole characteristics (age)
# ncrp_people_released_early_age <- ncrp_releases_2020 %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(!is.na(agerlse)) %>%
#   # filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   group_by(state, agerlse) %>%
#   summarise(total_agerlse = n()) %>%
#   mutate(total_releases = sum(total_agerlse, na.rm = TRUE),
#          prop = total_agerlse/total_releases)
#
#
# # Get people on parole characteristics (education)
# ncrp_people_released_early_age_median <- ncrp_releases_2020 %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(!is.na(agerlse)) %>%
#   # filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   mutate(agerlse_level = case_when(
#     agerlse == "18-24 years" ~ 1,
#     agerlse == "25-34 years" ~ 2,
#     agerlse == "35-44 years" ~ 3,
#     agerlse == "45-54 years" ~ 4,
#     agerlse == "55+ years"   ~ 5)) %>%
#   group_by(state) %>%
#   summarise(agerlse_median = median(agerlse_level, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(agerlse_median = case_when(
#     agerlse_median == 1 ~ "18-24 years",
#     agerlse_median == 2 ~ "25-34 years",
#     agerlse_median == 3 ~ "35-44 years",
#     agerlse_median == 4 ~ "45-54 years",
#     agerlse_median == 5 ~ "55+ years"
#   )) %>%
#   mutate(data = "Median Age") %>%
#   select(data, everything())
#
#
# # Get people on parole characteristics (education)
# ncrp_people_released_early_education_median <- ncrp_releases_2020 %>%
#   filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
#   filter(!is.na(education)) %>%
#   # filter(state != "Alabama") %>%
#   filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
#   mutate(education_level = case_when(
#     education == "(1) <HS diploma/GED" ~ 1,
#     education == "(2) HS diploma/GED"  ~ 2,
#     education == "(3) Any college"     ~ 3)) %>%
#   group_by(state) %>%
#   summarise(education_median = median(education_level, na.rm = TRUE)) %>%
#   mutate(data = "Median Education",
#          education_median = case_when(
#            education_median == 1 ~ "Less than HS diploma/GED",
#            education_median == 2 ~ "HS diploma/GED",
#            education_median == 3 ~ "Any college")) %>%
#   mutate() %>%
#   select(data, everything())




# ########################################
#
# # Histogram of time between release and PED
#
# ########################################
#
# ncrp_time_between_release_ped_2020 <- ncrp_releases_2020 %>%
#   # remove states with NA's
#   filter(!is.na(released_at_ped_status) & state != "Illinois") %>%
#   # combine years greater than 10 or less than -10
#   mutate(
#     time_between_release_ped_combined = case_when(
#       time_between_release_ped > 5 ~ "More than 10 Years After PED",
#       time_between_release_ped < -5 ~ "More than 10 Years Before PED",
#       TRUE ~ as.character(time_between_release_ped)),
#     time_between_release_ped_combined = factor(time_between_release_ped_combined,
#                                                levels = c("More than 10 Years Before PED",
#                                                           "-5",
#                                                           "-4",
#                                                           "-3",
#                                                           "-2",
#                                                           "-1",
#                                                           "0",
#                                                           "1",
#                                                           "2",
#                                                           "3",
#                                                           "4",
#                                                           "5",
#                                                           "More than 10 Years After PED"))
#   ) %>%
#   group_by(state) %>%
#   count(time_between_release_ped_combined) %>%
#   mutate(
#     prop = n / sum(n),
#     prop_label = paste0(round(prop * 100, 0), "%"),
#     tooltip = paste0("<b>", state, "</b><br><br>",
#               "Years between Release and PED: <b>",
#               time_between_release_ped_combined,
#               "</b><br><br>",
#               "Number of People: <b>",
#               scales::comma(n),
#               "</b><br><br>",
#               "Percentage of Prison Population: <b>",
#               prop_label, "</b></b>", sep = ""))




# ########################################
#
# # Bar graph of proportion of population by demographic released year of PED
#
# ########################################
#
# ncrp_time_between_release_ped_2020_by_race <-
#   ncrp_releases_2020 %>%
#   filter(!is.na(time_between_release_ped)) %>%
#   filter(race == "Hispanic, any race" |
#          race == "White, non-Hispanic" |
#          race == "Black, non-Hispanic") %>%
#   mutate(time_between_release_ped_overall =
#            case_when(
#              time_between_release_ped > 1 ~ "Released After Year of PED",
#              time_between_release_ped <= 1 ~ "Released Before or on Year of PED",
#              is.na(time_between_release_ped) ~ "No PED Data"
#            )
#          ) %>%
#   group_by(state, race) %>%
#   count(time_between_release_ped_overall) %>%
#   mutate(
#     prop = n / sum(n),
#     prop_label = paste0(round(prop * 100, 0), "%"),
#     tooltip = paste0("<b>", state, "</b><br><br><b>",
#                      time_between_release_ped_overall,
#                      "</b><br><br>",
#                      "Number of People: <b>",
#                      scales::comma(n),
#                      "</b><br><br>",
#                      "Percentage of Prison Population: <b>",
#                      prop_label, "</b></b>", sep = ""))












##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(ncrp_released_at_ped_2020,                    file=file.path(folder, "ncrp_released_at_ped_2020.rds"))
  save(ncrp_released_to_parole,                      file=file.path(folder, "ncrp_released_to_parole.rds"))
  save(ncrp_released_to_parole,                      file=file.path(folder, "ncrp_released_to_parole.rds"))

  save(ncrp_people_released_early_race,              file=file.path(folder, "ncrp_people_released_early_race.rds"))
  save(ncrp_people_released_early_sex,               file=file.path(folder, "ncrp_people_released_early_sex.rds"))
  save(ncrp_people_released_early_age,               file=file.path(folder, "ncrp_people_released_early_age.rds"))
  save(ncrp_people_released_early_age_median,        file=file.path(folder, "ncrp_people_released_early_age_median.rds"))
  save(ncrp_people_released_early_education_median,  file=file.path(folder, "ncrp_people_released_early_education_median.rds"))

  save(ncrp_time_between_release_ped_2020,           file=file.path(folder, "ncrp_time_between_release_ped_2020.rds"))
  save(ncrp_time_between_release_ped_2020_by_race,   file=file.path(folder, "ncrp_time_between_release_ped_2020_by_race.rds"))


}
