




# Load Data - Keep for now
# load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_not_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing_race.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_undercounted.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_rptyear.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race_2019.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_sex_2019.rds"))

# Define the data objects and their corresponding file names
data_files <- list(
  ncrp_projections                 = "ncrp_projections.rds",
  ncrp_population_projections      = "ncrp_population_projections.rds",
  ncrp_releases_not_consolidated   = "ncrp_releases_not_consolidated.rds",
  ncrp_yearendpop_consolidated     = "ncrp_yearendpop_consolidated.rds",
  ncrp_releases_consolidated       = "ncrp_releases_consolidated.rds",
  ncrp_yearendpop_not_consolidated = "ncrp_yearendpop_not_consolidated.rds",
  ncrp_yearendpop_combined         = "ncrp_yearendpop_combined.rds",
  ncrp_releases_combined           = "ncrp_releases_combined.rds",

  bjs_prison_pop_by_race           = "bjs_prison_pop_by_race.rds",
  bjs_prison_pop_by_sex            = "bjs_prison_pop_by_sex.rds",
  bjs_prison_pop_by_rptyear        = "bjs_prison_pop_by_rptyear.rds",

  hex_gj                           = "hex_gj.rds",
  states_abolished_parole          = "states_abolished_parole.rds",
  state_notes                      = "state_notes.rds",
  states_to_exclude                = "states_to_exclude.rds",
  states_nofilter                  = "states_nofilter.rds",
  states_undercounted              = "states_undercounted.rds",
  states_with_high_missing         = "states_with_high_missing.rds",
  states_with_high_missing_race    = "states_with_high_missing_race.rds",
  states_national_page_only        = "states_national_page_only.rds",
  states_use_other_race_eth        = "states_use_other_race_eth.rds",
  which_overall_year               = "which_overall_year.rds",
  which_years                      = "which_years.rds"
)

# Generate load statements
for (file_name in data_files) {
  cat(paste0("load(file = paste0(sp_data_path, \"/data/analysis/app/", file_name, "\"))\n"))
}

load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_projections.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_population_projections.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_not_consolidated.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_consolidated.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_consolidated.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_not_consolidated.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_combined.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_combined.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_rptyear.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_abolished_parole.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_nofilter.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_undercounted.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_national_page_only.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_use_other_race_eth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/which_overall_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/which_years.rds"))
