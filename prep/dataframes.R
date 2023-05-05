

# State Donuts
load(file = paste0(sp_data_path, "/data/analysis/all_donut_currently_eligible.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_donut_future_eligible.rds"))

# Parole Overview
load(file = paste0(sp_data_path, "/data/analysis/people_released_to_parole_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/people_released_to_parole_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/people_released_to_parole_age.rds"))
load(file = paste0(sp_data_path, "/data/analysis/people_released_to_parole_age_median.rds"))
load(file = paste0(sp_data_path, "/data/analysis/people_released_to_parole_education_median.rds"))


# Parole Eligibility
load(file = paste0(sp_data_path, "/data/analysis/all_pie_parole_elgibility_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/parole_eligibility_table_2020.rds"))
load(file = paste0(sp_data_path, "/data/analysis/current_ped_2020_offenses.rds"))


# Releases from Prison
load(file = paste0(sp_data_path, "/data/analysis/all_pie_released_at_ped.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_line_pop_released_to_parole.rds"))


# RRI
load(file = paste0(sp_data_path, "/data/analysis/race_eth_rri_table.rds"))


# Predicted Probabilities
load(file = paste0(sp_data_path, "/data/analysis/all_pp_by_variable.rds"))
