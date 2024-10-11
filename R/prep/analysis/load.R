
# Load data necessary for analysis
# This data was imported and saved in prep/import/import_format.R

load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))


load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_rptyear.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race_2020.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_sex_2022.rds"))


# filter(!state %in% states_to_exclude$state) |>
#
#
#   hc_exporting(enabled = TRUE,
#                filename = paste0(gsub(" ", "_", tolower(title)), "_", "by_race_ethnicity_", select_year)) |>
#
#
#   hc_exporting(enabled = TRUE,
#                filename = paste0(gsub(" ", "_", tolower(title)), "_", select_year)) |>
