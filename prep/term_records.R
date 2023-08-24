#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: August 22, 2023 (MAR)
# Description:
#    Parole eligibility tables and graphics for app
#######################################

# Get number of prison episodes and identify most recent episode
# dim(ncrp_term_records_flags) #13897198 rows
ncrp_term_records_flags <- ncrp_term_records %>%
  filter(!is.na(admityr)) %>%
  mutate(prison_episode_id = paste(abt_inmate_id, admityr, sep = "_")) %>%
  arrange(abt_inmate_id, admityr, releaseyr) %>%
  group_by(abt_inmate_id) %>%
  mutate(
    prison_episode_number = row_number(),
    recent_prison_episode = row_number() == n(),
    has_multiple_prison_episode = n() > 1
  ) %>%
  ungroup()

# Flag people who have been to prison for a parole return or revocation
ncrp_has_previous_parole_return <- ncrp_term_records_flags %>%
  filter(recent_prison_episode == FALSE) %>%
  group_by(abt_inmate_id) %>%
  summarize(has_previous_parole_return = any(admtype == "Parole return/revocation" & !is.na(admtype))) %>%
  ungroup()

# Add flag to data
# If they only have one prison episode and it's not a parole return,
# then make has_previous_parole_return FALSE
# dim(ncrp_term_records_parole_flags) #13897198 rows
ncrp_term_records_parole_flags <- ncrp_term_records_flags %>%
  left_join(ncrp_has_previous_parole_return, by = "abt_inmate_id") %>%
  mutate(has_previous_parole_return =
           ifelse(is.na(has_previous_parole_return) & has_multiple_prison_episode == FALSE,
                  FALSE,
                  has_previous_parole_return))

# Subset to people in prison for the first time
# Which means they don't have a history of prison episodes or returns to prison from parole
# dim(ncrp_in_prison_first_time) #4428320 rows
ncrp_in_prison_first_time <- ncrp_term_records_parole_flags %>%
  filter(has_multiple_prison_episode == FALSE &
           has_previous_parole_return  == FALSE)

# Subset to people released in 2020
# subset to people in prison for the first time
# Create parole eligibility status
ncrp_in_prison_first_time_releases_2020 <- ncrp_term_records_parole_flags %>%
  filter(has_multiple_prison_episode == FALSE &
           has_previous_parole_return  == FALSE) %>%
  filter(releaseyr == 2020)

# Subset to people who were in prison in 2019 so 2019 is between admission year and release year
# At first I thought we could assume that if release year is missing, it's because they haven't been released but this probably isn't the case all the time
# dim(ncrp_in_prison_first_time_during2019) # 845326rows/people
# Create year variable of 2020
ncrp_in_prison_first_time_during_2020 <- ncrp_term_records_parole_flags %>%
  filter(has_multiple_prison_episode == FALSE &
           has_previous_parole_return  == FALSE) %>%
  mutate(in_prison_2020 = ifelse(admityr <= 2020 & releaseyr >= 2020, 1, 0)) %>%
  # (releaseyr >= 2020 | is.na(releaseyr)), 1, 0)) %>%
  filter(in_prison_2020 == 1) %>%
  mutate(rptyear = 2020) %>%
  fnc_create_parelig_status()










################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(ncrp_in_prison_first_time,               file = file.path(folder, "ncrp_in_prison_first_time.rds"))
  save(ncrp_in_prison_first_time_releases_2020, file = file.path(folder, "ncrp_in_prison_first_time_releases_2020.rds"))
  save(ncrp_in_prison_first_time_during_2020,   file = file.path(folder, "ncrp_in_prison_first_time_during_2020.rds"))

}
