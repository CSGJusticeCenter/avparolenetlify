#######################################
# Project: AV Parole
# File: missing_data.R
# Authors: Mari Roberts
# Date last updated: August 3, 2023 (MAR)
# Description:
#    Understanding missing parole eligibility data
#    Is data missing, does it not exist, or are people ineligible for parole?
#######################################

################################################################################

# Prepare data to analyze missingness by state

# Obtained from NCRP year end population

################################################################################

# get data by state
cross_tab_data <- ncrp_yearendpop %>%
  filter(!is.na(sentlgth) &
           !is.na(offgeneral)) %>%
  mutate(offgeneral = case_when(offgeneral == "Other/unspecified" ~ "Other or unspecified",
                                TRUE ~ offgeneral),
         offdetail  = case_when(offdetail == " Other/unspecified" ~ "Other or unspecified",
                                TRUE ~ offdetail),
         release_id = row_number(),
         sentlgth =
           factor(sentlgth,
                  levels = c("< 1 year",
                             "1-1.9 years",
                             "2-4.9 years",
                             "5-9.9 years",
                             "10-24.9 years",
                             ">=25 years",
                             "Life, LWOP, Life plus additional years, Death"),
                  ordered = TRUE),
         offgeneral =
           factor(offgeneral,
                  levels = c("Drugs",
                             "Public order",
                             "Property",
                             "Other or unspecified",
                             "Violent",
                             "All Offenses"),
                  ordered = TRUE)) %>%
  filter(rptyear == 2020)









################################################################################

# Missingness by sentence length and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_sentlgth_ped <- cross_tab_data %>%
  group_by(state) %>%
  count(sentlgth, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         sentlgth = as.factor(sentlgth))








################################################################################

# Missingness by offense type and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_offgeneral_ped <- cross_tab_data %>%
  group_by(state) %>%
  count(offgeneral, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         offgeneral = as.factor(offgeneral))







################################################################################

# Missingness by detailed offense type and parole eligibility status

# Obtained from NCRP year end population

################################################################################

# cross tab by state
all_offdetail_ped <- cross_tab_data %>%
  group_by(state) %>%
  count(offdetail, parelig_status) %>%
  pivot_wider(names_from = parelig_status, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           missing +
           current +
           future_1_5_years +
           future_6_years) %>%
  mutate_at(vars(missing:future_6_years), list(percent = ~ (. / total))) %>%
  mutate(state = as.factor(state),
         offdetail = as.factor(offdetail))






################################################################################

# Missingness by sentence length and offense type (general) - COLUMN SUMS

# Obtained from NCRP year end population (missing data only)

################################################################################

# cross tab by offense length and sentence length and by state
all_sentlgth_offgeneral <- cross_tab_data %>%
  filter(parelig_status == "Missing") %>%
  group_by(state) %>%
  count(sentlgth, offgeneral) %>%
  pivot_wider(names_from = sentlgth, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           x1_year +
           x1_1_9_years +
           x2_4_9_years +
           x5_9_9_years +
           x10_24_9_years +
           x25_years +
           life_lwop_life_plus_additional_years_death) %>%
  mutate_at(vars(x1_year:life_lwop_life_plus_additional_years_death), list(percent = ~ (. / total)))

# cross tab by sentence length and by state
all_sentlgth_offgeneral_overview <- cross_tab_data %>%
  group_by(state) %>%
  count(sentlgth) %>%
  pivot_wider(names_from = sentlgth, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(offgeneral = "All Offenses",
         total =
           x1_year +
           x1_1_9_years +
           x2_4_9_years +
           x5_9_9_years +
           x10_24_9_years +
           x25_years +
           life_lwop_life_plus_additional_years_death) %>%
  mutate_at(vars(x1_year:life_lwop_life_plus_additional_years_death), list(percent = ~ (. / total)))

# add data together
all_sentlgth_offgeneral <- rbind(all_sentlgth_offgeneral, all_sentlgth_offgeneral_overview)
all_sentlgth_offgeneral <- all_sentlgth_offgeneral %>% mutate(state = as.factor(state),
                                                              offgeneral = as.factor(offgeneral))






################################################################################

# Missingness by sentence length and offense type - ROW SUMS

# Obtained from NCRP year end population (missing data only)

################################################################################

# cross tab by offense length and sentence length and by state
all_offgeneral_sentlgth <- cross_tab_data %>%
  filter(parelig_status == "Missing") %>%
  group_by(state) %>%
  count(sentlgth, offgeneral) %>%
  pivot_wider(names_from = offgeneral, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(total =
           drugs  +
           public_order  +
           property  +
           other_or_unspecified  +
           violent) %>%
  mutate_at(vars(drugs:violent), list(percent = ~ (. / total)))

# cross tab by offense length and by state
all_offgeneral_sentlgth_overview <- cross_tab_data %>%
  filter(parelig_status == "Missing") %>%
  group_by(state) %>%
  count(offgeneral) %>%
  pivot_wider(names_from = offgeneral, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(sentlgth = "All Sentence Lengths",
         total =
           drugs  +
           public_order  +
           property  +
           other_or_unspecified  +
           violent) %>%
  mutate_at(vars(drugs:violent), list(percent = ~ (. / total)))

# add data together
all_offgeneral_sentlgth <- rbind(all_offgeneral_sentlgth, all_offgeneral_sentlgth_overview)
all_offgeneral_sentlgth <- all_offgeneral_sentlgth %>% mutate(state = as.factor(state),
                                                              sentlgth = as.factor(sentlgth))





################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_offgeneral_ped, file=file.path(folder, "all_offgeneral_ped.rds"))
  save(all_offdetail_ped, file=file.path(folder, "all_offdetail_ped.rds"))
  save(all_sentlgth_ped, file=file.path(folder, "all_sentlgth_ped.rds"))
  save(all_sentlgth_offgeneral, file=file.path(folder, "all_sentlgth_offgeneral.rds"))
  save(all_offgeneral_sentlgth, file=file.path(folder, "all_offgeneral_sentlgth.rds"))

}

