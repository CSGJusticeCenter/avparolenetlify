
# Define the footnote based on the state
footnote_text <- if(state_for_report %in% c("Connecticut", "Idaho")) {
  "*Includes people with any admission type or sentence length."
} else {
  "*Only includes people in prison for new offenses and excludes people with life sentences and sentences less than one year."
}


#------------------------------------------------------------------------------#
# Missingness/No Data Text
#------------------------------------------------------------------------------#

# no data text
no_data_text <- HTML(paste0("<div style='text-align:center;'>
               <br><span style='font-size:24px; font-weight:bold;'>NO DATA</span><br>
               Data is not available. ", state_for_report,
                            " submitted incomplete or missing data to the National Corrections Reporting Program in ",
                            select_year, ".<br><br>
             </div>"))

#------------------------------------------------------------------------------#
# How Parole Eligibility Is Determined?
# Citations (import_format.R)
#------------------------------------------------------------------------------#

# Load Prepared Data
load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/state_methodology.rds"))

state_citation <- state_notes |>
  filter(state == state_for_report) |>
  pull(citation)

state_imputation_notes <- state_notes |>
  filter(state == state_for_report) |>
  pull(methodology_notes)

# TITLE: How is Parole Eligibility Determined?
parole_eligibility_criteria <- subset(state_notes,
                                      state == state_for_report)$release_systems


#------------------------------------------------------------------------------#
# Highlighted Findings (page_national_trends.R)
#------------------------------------------------------------------------------#

# Load Prepared Data
load(file = paste0(sp_data_path, "/data/analysis/app/map_percent.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))

# Get number of people currently eligible for parole
if (state_for_report %in% unique(parole_eligibility_table$state)) {
  state_data <- parole_eligibility_table |> filter(state == state_for_report)
} else {
  state_data <- no_data_text
}

num_people_current <- parole_eligibility_table |> filter(state == state_for_report) |> pull(current_count_rounded)
num_people_current_perc <- parole_eligibility_table |> filter(state == state_for_report) |> pull(current_perc)
num_parole_board_mem <- parole_eligibility_table |> filter(state == state_for_report) |> pull(members)





#------------------------------------------------------------------------------#
# Parole Eligibility
#------------------------------------------------------------------------------#

# Load necessary data files
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pop_pe_by_year.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_pop_pe_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pop_pe_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_ped_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_sentlgth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_sentlgth.rds"))

# Define a helper function to conditionally apply size and colors if chart is not NULL
apply_chart_settings <- function(chart, height = NULL, color = NULL) {
  if (!is.null(chart)) {
    if (!is.null(height)) {
      chart <- chart |> hc_size(height = height)
    }
    if (!is.null(color)) {
      chart <- chart |> hc_colors(c(color))
    }
  }
  return(chart)
}

# State-specific data assignments
state_sentence_pe_type <- all_sentence_pe_type[[state_for_report]]

state_pie_pe_type <- apply_chart_settings(
  all_pie_pe_type[[state_for_report]],
  height = 300
)

state_sentence_pop_pe_by_year <- all_sentence_pop_pe_by_year[[state_for_report]]

state_bar_pop_pe_by_year <- apply_chart_settings(
  all_bar_pop_pe_by_year[[state_for_report]],
  height = 300,
  color = color4
)
# state_stackedbar_pop_pe_by_year <- apply_chart_settings(
#   all_stackedbar_pop_pe_by_year[[state_for_report]],
#   height = 400
# )

state_sentence_parole_eligibility_race <- all_sentence_parole_eligibility_race[[state_for_report]]

state_bar_parole_eligibility_race <- apply_chart_settings(
  all_bar_parole_eligibility_race[[state_for_report]],
  height = 300,
  color = color4
)

state_sentence_parole_eligibility_sex <- all_sentence_parole_eligibility_sex[[state_for_report]]

state_bar_parole_eligibility_sex <- apply_chart_settings(
  all_bar_parole_eligibility_sex[[state_for_report]],
  height = 300,
  color = color4
)

state_sentence_parole_eligibility_ageyrend <- all_sentence_parole_eligibility_ageyrend[[state_for_report]]

state_bar_parole_eligibility_ageyrend <- apply_chart_settings(
  all_bar_parole_eligibility_ageyrend[[state_for_report]],
  height = 300,
  color = color4
)

state_sentence_parole_eligibility_fbi_index <- all_sentence_parole_eligibility_fbi_index[[state_for_report]]

state_bar_ped_fbi_index <- apply_chart_settings(
  all_bar_ped_fbi_index[[state_for_report]],
  height = 500,
  color = color4
)

state_sentence_parole_eligibility_sentlgth <- all_sentence_parole_eligibility_sentlgth[[state_for_report]]

state_bar_parole_eligibility_sentlgth <- apply_chart_settings(
  all_bar_parole_eligibility_sentlgth[[state_for_report]],
  height = 400,
  color = color4
)




#------------------------------------------------------------------------------#
# Population Tab (tab_population.R)
#------------------------------------------------------------------------------#

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_line_population_by_year.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_population_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_population_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_population_ageyrend.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_population_fbi_index.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_sentlgth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_population_sentlgth.rds"))


state_sentence_population <- all_sentence_population[[state_for_report]]

state_line_population_by_year <- apply_chart_settings(
  all_line_population_by_year[[state_for_report]],
  height = 300,
  color = color2
)


state_sentence_population_race <- all_sentence_population_race[[state_for_report]]

state_bar_population_race <- apply_chart_settings(
  all_bar_population_race[[state_for_report]],
  height = 300,
  color = color2
)

state_sentence_population_sex <- all_sentence_population_sex[[state_for_report]]

state_bar_population_sex <- apply_chart_settings(
  all_bar_population_sex[[state_for_report]],
  height = 300,
  color = color2
)

state_sentence_population_ageyrend <- all_sentence_population_ageyrend[[state_for_report]]

state_bar_population_ageyrend <- apply_chart_settings(
  all_bar_population_ageyrend[[state_for_report]],
  height = 300,
  color = color2
)

state_sentence_population_fbi_index <- all_sentence_population_fbi_index[[state_for_report]]

state_bar_population_fbi_index <- apply_chart_settings(
  all_bar_population_fbi_index[[state_for_report]],
  height = 500,
  color = color2
)

state_sentence_population_sentlgth <- all_sentence_population_sentlgth[[state_for_report]]

state_bar_population_sentlgth <- apply_chart_settings(
  all_bar_population_sentlgth[[state_for_report]],
  height = 400,
  color = color2
)






#------------------------------------------------------------------------------#
# Releases Tab (tab_releases.R)
#------------------------------------------------------------------------------#

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_line_releases_by_year.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_release_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_release_type.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_proportion_released.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_parole_eligibility_release.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_agerlse.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_agerlse.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_fbi_index.rds"))

state_sentence_releases <- all_sentence_releases[[state_for_report]]

state_line_releases_by_year <- apply_chart_settings(
  all_line_releases_by_year[[state_for_report]],
  height = 300,
  color = color5
)

state_sentence_pe_proportion_released <- all_sentence_pe_proportion_released[[state_for_report]]

state_stackedbar_parole_eligibility_release <- apply_chart_settings(
  all_stackedbar_parole_eligibility_release[[state_for_report]],
  height = 400
)

state_sentence_release_type <- all_sentence_release_type[[state_for_report]]

state_pie_release_type <- apply_chart_settings(
  all_pie_release_type[[state_for_report]],
  height = 300,
  color = c(color5, color3)
)

state_sentence_releases_race <- all_sentence_releases_race[[state_for_report]]

state_bar_releases_race <- apply_chart_settings(
  all_bar_releases_race[[state_for_report]],
  height = 300,
  color = color5
)

state_sentence_releases_sex <- all_sentence_releases_sex[[state_for_report]]

state_bar_releases_sex <- apply_chart_settings(
  all_bar_releases_sex[[state_for_report]],
  height = 300,
  color = color5
)

state_sentence_releases_agerlse <- all_sentence_releases_agerlse[[state_for_report]]

state_bar_releases_agerlse <- apply_chart_settings(
  all_bar_releases_agerlse[[state_for_report]],
  height = 300,
  color = color5
)

state_sentence_releases_fbi_index <- all_sentence_releases_fbi_index[[state_for_report]]

state_bar_releases_fbi_index <- apply_chart_settings(
  all_bar_releases_fbi_index[[state_for_report]],
  height = 500,
  color = color5
)














#------------------------------------------------------------------------------#
# RRI Disparities (tab_disparities_rris.R)
#------------------------------------------------------------------------------#

# Race and Ethnicity
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_rri_black.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_rri_hispanic.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_black.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_hispanic.rds"))

rri_infographic_black       <- paste0(sp_data_path, "/data/analysis/app/pngs/rri_infographic_black_",
                                      state_for_report, ".png")
rri_infographic_hispanic    <- paste0(sp_data_path, "/data/analysis/app/pngs/rri_infographic_hispanic_",
                                      state_for_report, ".png")
pe_rri_infographic_black    <- paste0(sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_black_",
                                      state_for_report, ".png")
pe_rri_infographic_hispanic <- paste0(sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_hispanic_",
                                      state_for_report, ".png")

state_sentence_rri_black <- all_sentence_rri_black[[state_for_report]]

state_sentence_rri_hispanic <- all_sentence_rri_hispanic[[state_for_report]]

state_sentence_pe_rri_black <- all_sentence_pe_rri_black[[state_for_report]]

state_sentence_pe_rri_hispanic <- all_sentence_pe_rri_hispanic[[state_for_report]]


# Sex
# load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_rri_male.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_male.rds"))

# rri_infographic_male     <- paste0(sp_data_path, "/data/analysis/app/pngs/rri_infographic_male_",
#                                       state_for_report, ".png")

pe_rri_infographic_male     <- paste0(sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_male_",
                                      state_for_report, ".png")

state_sentence_pe_rri_male <- all_sentence_pe_rri_male[[state_for_report]]




#------------------------------------------------------------------------------#
# Other Disparities (tab_disparities.R)
#------------------------------------------------------------------------------#

# Past PE
load(file = paste0(sp_data_path, "/data/analysis/app/avg_current_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/avg_current_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_sex.rds"))

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_sex_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_avg_past_pe_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_avg_past_pe_sex_offense.rds"))

state_sentence_avg_past_pe_race <- all_sentence_avg_past_pe_race[[state_for_report]]

state_sentence_avg_past_pe_race_offense <- all_sentence_avg_past_pe_race_offense[[state_for_report]]

state_scatter_avg_past_pe_race_offense <- apply_chart_settings(
  all_scatter_avg_past_pe_race_offense[[state_for_report]],
  height = 600
)

state_sentence_avg_past_pe_sex <- all_sentence_avg_past_pe_sex[[state_for_report]]

state_sentence_avg_past_pe_sex_offense <- all_sentence_avg_past_pe_sex_offense[[state_for_report]]

state_scatter_avg_past_pe_sex_offense <- apply_chart_settings(
  all_scatter_avg_past_pe_sex_offense[[state_for_report]],
  height = 600
)






load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_los_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_los_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_sex_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_los_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_los_sex_offense.rds"))

state_sentence_los_race <- all_sentence_los_race[[state_for_report]]

state_lollipop_los_race <- apply_chart_settings(
  all_lollipop_los_race[[state_for_report]]
)

state_sentence_los_race_offense <- all_sentence_los_race_offense[[state_for_report]]

state_scatter_los_race_offense <- apply_chart_settings(
  all_scatter_los_race_offense[[state_for_report]],
  height = 600
)

state_sentence_los_sex <- all_sentence_los_sex[[state_for_report]]

state_lollipop_los_sex <- apply_chart_settings(
  all_lollipop_los_sex[[state_for_report]],
  height = 150
)

state_sentence_los_sex_offense <- all_sentence_los_sex_offense[[state_for_report]]

state_scatter_los_sex_offense <- apply_chart_settings(
  all_scatter_los_sex_offense[[state_for_report]],
  height = 600
)




