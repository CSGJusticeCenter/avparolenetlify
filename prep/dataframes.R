

# # NCRP files
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_yearendpop.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_admissions.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_term_records.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/ncrp_releases.rds"))

################################################################################
# National Trends
################################################################################

# Map
load(file = paste0(sp_data_path, "/data/analysis/hex_gj.rds"))
load(file = paste0(sp_data_path, "/data/analysis/parole_info_by_state.rds"))

# Table
load(file = paste0(sp_data_path, "/data/analysis/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/parole_eligibility_table_select_year.rds"))

################################################################################
# Prison Population
################################################################################

# New crime vs Parole return
load(file = paste0(sp_data_path, "/data/analysis/all_stackedbar_admtype.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_pie_admtype.rds"))

# Prison Population
load(file = paste0(sp_data_path, "/data/analysis/all_line_pop_released_to_parole.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_population.rds"))

load(file = paste0(sp_data_path, "/data/analysis/all_stackedbar_pe_type.rds"))

# Who is in Prison?
load(file = paste0(sp_data_path, "/data/analysis/all_stackedbar_prison_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_stackedbar_prison_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_stackedbar_prison_ageyrend.rds"))

# Sentence Length
load(file = paste0(sp_data_path, "/data/analysis/all_groupedbar_prison_sentlgth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_bar_prison_sentlgth.rds")) # may not need

# Offenses
load(file = paste0(sp_data_path, "/data/analysis/all_groupedbar_prison_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_bar_prison_fbi_index.rds")) # may not need




################################################################################
# Parole Eligibility
################################################################################

# Robina Institute information
load(file = paste0(sp_data_path, "/data/analysis/robinainfo.rds"))
load(file = paste0(sp_data_path, "/data/analysis/robinadefinitions.rds"))
load(file = paste0(sp_data_path, "/data/analysis/robinaparoleeligibility.rds"))

# Parole Eligibility
load(file = paste0(sp_data_path, "/data/analysis/all_bar_parole_elgibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_bar_parole_elgibility_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_bar_parole_elgibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_bar_parole_elgibility_sentlgth.rds"))

load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_sentlgth.rds"))

# Offenses
load(file = paste0(sp_data_path, "/data/analysis/all_bar_parole_elgibility_fbi_index_new_crime.rds"))
load(file = paste0(sp_data_path, "/data/analysis/all_sentence_parole_elgibility_fbi_index.rds"))




################################################################################
# Releases
################################################################################

# # Maxout
# load(file = paste0(sp_data_path, "/data/analysis/all_pie_maxout.rds"))
#
# # Unconditional vs conditional release
# load(file = paste0(sp_data_path, "/data/analysis/all_pie_release_type.rds"))
#
# # Releases from Prison
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped_publicorder.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped_property.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped_other.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped_drugs.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_released_at_ped_violent.rds"))
#
# # Length of Stay
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_overview.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_publicorder.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_property.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_other.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_drugs.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_los_violent.rds"))






################################################################################
# Disparities
################################################################################

# # Disparities
# load(file = paste0(sp_data_path, "/data/analysis/all_time_between_release_ped_by_race.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_pp_by_variable.rds"))
#
# # RRI
# load(file = paste0(sp_data_path, "/data/analysis/race_eth_rri_table.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_rri_sentence_length_black.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/all_bar_rri_sentence_length_hispanic.rds"))
#




