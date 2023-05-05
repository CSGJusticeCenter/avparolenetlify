#######################################
# Project: AV Parole
# File: releases_ncrp.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for shiny app
#######################################



ncrp_releases_clean <- ncrp_releases %>%

  # create order for sentence length and time served length
  # for example, <1 is 1 and 1-1.9 is 2, and so on
  mutate(
    sentlgth_order = case_when(
      sentlgth == "< 1 year"      ~ 1,
      sentlgth == "1-1.9 years"   ~ 2,
      sentlgth == "2-4.9 years"   ~ 3,
      sentlgth == "5-9.9 years"   ~ 4,
      sentlgth == "10-24.9 years" ~ 5,
      sentlgth == ">=25 years"    ~ 5,
      sentlgth == "Life, LWOP, Life plus additional years, Death" ~ 5,
      TRUE ~ NA),
    timesrvd_rel_order = case_when(
      timesrvd_rel == "< 1 year"      ~ 1,
      timesrvd_rel == "1-1.9 years"   ~ 2,
      timesrvd_rel == "2-4.9 years"   ~ 3,
      timesrvd_rel == "5-9.9 years"   ~ 4,
      timesrvd_rel == ">=10 years"    ~ 5,
      TRUE ~ NA)) %>%

  # determine differences between time served and sentenced length
  # calculate actual time served
  mutate(
    timesrvd_rel_vs_sentlgth = case_when(
      is.na(timesrvd_rel_order) | is.na(sentlgth_order) ~ NA,
      timesrvd_rel_order == sentlgth_order ~ "Full Sentence Length Served",
      timesrvd_rel_order > sentlgth_order  ~ "More than Sentence Length Served",
      timesrvd_rel_order < sentlgth_order  ~ "Less than Sentence Length Served"),
    time_served = relyr - admityr) %>%

  # https://www.icpsr.umich.edu/web/NACJD/studies/38492/datasets/0003/variables/PARELIG_YEAR?archive=nacjd
  # remove parelig_year/mand_prisrel_year 2100
  mutate(parelig_year_clean =
           ifelse(parelig_year <= 2105, parelig_year, NA),
         mand_prisrel_year_clean =
           ifelse(mand_prisrel_year <= 2105, mand_prisrel_year, NA),

         time_between_release_ped = relyr - parelig_year_clean,
         time_between_ped_admission = parelig_year_clean - admityr,
         time_between_mandatoryrelease_release = mand_prisrel_year_clean - relyr) %>%

  mutate(released_at_ped_status = case_when(
    time_between_release_ped < 0 ~ "Released before Parole Eligibility",
    time_between_release_ped == 0 ~ "Released at Parole Eligibility",
    time_between_release_ped > 0 ~ "Released after Parole Eligibility",
    is.na(time_between_release_ped) ~ NA))





########################################

# Released to Parole Over Time

########################################

# count number of people released to parole by year and state
ncrp_released_to_parole <- ncrp_releases_clean %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  group_by(rptyear, state) %>%
  summarise(releases_to_parole = n())









########################################

# Releases in 2020

# How many people are being released at first eligibility?
# How long after eligibility does release occur?
# How does release vary by the person's demographic and criminal history characteristics?
# What is the mean and median time between parole eligibility and release for those released after the PED, by maximum sentence length?

########################################


# Subset to 2020 report
ncrp_releases_2020 <- ncrp_releases_clean %>%
  filter(rptyear == 2020)
# filter(!is.na(admityr) &
# !is.na(parelig_year_clean) &
# !is.na(mand_prisrel_year_clean) &
# !is.na(relyr)) # removes a lot of data

# How many people are being released at first eligibility?
ncrp_released_at_ped <- ncrp_releases_2020 %>%
  # remove states with NA's
  filter(!is.na(released_at_ped_status) & state != "Illinois") %>%
  group_by(state) %>%
  count(released_at_ped_status) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Timing of Release: <b>",
                  released_at_ped_status,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))







########################################

# Profile of People on Parole

########################################

# Get people on parole characteristics (race)
people_released_to_parole_race <- ncrp_releases_2020 %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(!is.na(race)) %>%
  filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  group_by(state, race) %>%
  summarise(total_race = n()) %>%
  mutate(total_releases = sum(total_race, na.rm = TRUE),
         prop = total_race/total_releases)


# Get people on parole characteristics (sex)
people_released_to_parole_sex <- ncrp_releases_2020 %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(!is.na(sex)) %>%
  # filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  group_by(state, sex) %>%
  summarise(total_sex = n()) %>%
  mutate(total_releases = sum(total_sex, na.rm = TRUE),
         prop = total_sex/total_releases)


# Get people on parole characteristics (age)
people_released_to_parole_age <- ncrp_releases_2020 %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(!is.na(agerlse)) %>%
  # filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  group_by(state, agerlse) %>%
  summarise(total_agerlse = n()) %>%
  mutate(total_releases = sum(total_agerlse, na.rm = TRUE),
         prop = total_agerlse/total_releases)


# Get people on parole characteristics (education)
people_released_to_parole_age_median <- ncrp_releases_2020 %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(!is.na(agerlse)) %>%
  # filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  mutate(agerlse_level = case_when(
    agerlse == "18-24 years" ~ 1,
    agerlse == "25-34 years" ~ 2,
    agerlse == "35-44 years" ~ 3,
    agerlse == "45-54 years" ~ 4,
    agerlse == "55+ years"   ~ 5)) %>%
  group_by(state) %>%
  summarise(agerlse_median = median(agerlse_level, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(agerlse_median = case_when(
    agerlse_median == 1 ~ "18-24 years",
    agerlse_median == 2 ~ "25-34 years",
    agerlse_median == 3 ~ "35-44 years",
    agerlse_median == 4 ~ "45-54 years",
    agerlse_median == 5 ~ "55+ years"
  )) %>%
  mutate(data = "Median Age") %>%
  select(data, everything())


# Get people on parole characteristics (education)
people_released_to_parole_education_median <- ncrp_releases_2020 %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(!is.na(education)) %>%
  # filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  mutate(education_level = case_when(
    education == "(1) <HS diploma/GED" ~ 1,
    education == "(2) HS diploma/GED"  ~ 2,
    education == "(3) Any college"     ~ 3)) %>%
  group_by(state) %>%
  summarise(education_median = median(education_level, na.rm = TRUE)) %>%
  mutate(data = "Median Education",
         education_median = case_when(
           education_median == 1 ~ "Less than HS diploma/GED",
           education_median == 2 ~ "HS diploma/GED",
           education_median == 3 ~ "Any college")) %>%
  mutate() %>%
  select(data, everything())


##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(ncrp_released_at_ped,                        file=file.path(folder, "ncrp_released_at_ped.rds"))
  save(ncrp_released_to_parole,                     file=file.path(folder, "ncrp_released_to_parole.rds"))

  save(people_released_to_parole_race,              file=file.path(folder, "people_released_to_parole_race.rds"))
  save(people_released_to_parole_sex,               file=file.path(folder, "people_released_to_parole_sex.rds"))
  save(people_released_to_parole_age,               file=file.path(folder, "people_released_to_parole_age.rds"))
  save(people_released_to_parole_age_median,        file=file.path(folder, "people_released_to_parole_age_median.rds"))
  save(people_released_to_parole_education_median,  file=file.path(folder, "people_released_to_parole_education_median.rds"))


}
