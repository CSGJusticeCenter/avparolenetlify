
################################################################################
# National Trends
################################################################################

# Map
load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/parole_info_by_state.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/map_count.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/map_percent.rds"))

# Table
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table_select_year.rds"))

################################################################################
# Prison Population
################################################################################

# Overall Trends
load(file = paste0(sp_data_path, "/data/analysis/app/all_yearendpop_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_admissions_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_releases_by_year.rds"))

# New crime vs Parole return
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_admtype.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_admtype.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_admtype.rds"))

# Prison Population
# load(file = paste0(sp_data_path, "/data/analysis/app/all_line_pop_released_to_parole.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_population.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_pe_type.rds"))

# Who is in Prison?
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_prison_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_prison_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_prison_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_prison_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_prison_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_prison_gender.rds"))


# Sentence Length
load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_prison_sentlgth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_prison_sentlgth.rds")) # may not need

# Offenses
load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_prison_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_prison_fbi_index.rds")) # may not need




################################################################################
# Parole Eligibility
################################################################################

# Robina Institute information
load(file = paste0(sp_data_path, "/data/analysis/app/robinainfo.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/robinadefinitions.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/robinaparoleeligibility.rds"))

# Parole Eligibility
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_elgibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_elgibility_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_elgibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_elgibility_sentlgth.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_sentlgth.rds"))

# Offenses
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_elgibility_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_elgibility_fbi_index.rds"))




################################################################################
# Releases
################################################################################

# Release Timing by Parole Eligibility Status
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_parole_eligibility_release.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_release_timing_reltype.rds"))

# Demographics
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_release_agerlse.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_release_gender.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_release_race.rds"))

# Unconditional vs conditional release
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_release_type.rds"))

# Release Timing by Offense Type
load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_release_timing_fbi_index.rds"))

# LOS by Offense Type
load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_los_by_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_los_by_offense.rds"))



################################################################################
# Disparities
################################################################################

# Disparities
load(file = paste0(sp_data_path, "/data/analysis/app/all_rri_infographic_race.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedbar_disparities_race.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_groupedcolumn_disparities_release_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedcolumn_disparities_release_race.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/rri_in_prison_data.rds"))




