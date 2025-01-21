# Load prepared data created in import_format.R
load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/state_methodology.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_nofilter.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_undercounted.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/which_overall_year.rds"))

# Determine select year depending on state
select_year <- which_overall_year |> filter(state == state_for_report) |> pull(year_to_use)

# Create NCRP/CSG source
ncrp_csg_source_year <- paste0("National Corrections Reporting Program, ", select_year, " and CSG Justice Center estimates.")

# Define the base additional asterisk text based on the state that weren't filtered by adm type and sentence length
additional_asterisks_text <- if (state_for_report %in% states_nofilter$state) {
  "*Projection based on 2023 prison population. Includes people in prison with any admission type or sentence length."
} else {
  "*Projection based on 2023 prison population. This and other figures regarding people past parole eligibility includes people in prison with sentences of more than one year who have not already been released on parole and excludes people with life sentences."
}

# Define the secondary asterisk text based on the states that were likely undercounted
additional_asterisks_text1 <- if (state_for_report %in% states_undercounted$state) {
  "Due to missing or unreported data, we are likely underestimating the percent of people past their parole eligibility year, especially for people with longer sentences. Results should be interpreted with caution."
} else {
  NULL
}

# Combine all non-NULL and non-empty texts into one, ensuring proper spacing
additional_asterisks_combined <- c(
  additional_asterisks_text,
  if (!is.null(additional_asterisks_text1) && nzchar(additional_asterisks_text1)) additional_asterisks_text1
) |> paste(collapse = " ")

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

# state_notes created in import_format.R
# Get citations; state notes created in import_format.R
state_citation <- state_notes |>
  filter(state == state_for_report) |>
  pull(citation)

# Get estimation methodology
state_imputation_notes <- state_notes |>
  filter(state == state_for_report) |>
  pull(methodology_notes)

# TITLE: How is Parole Eligibility Determined?
# Release Systems by State Doc is where release_systems from and they were added
# to state_notes so they are in csv format.
parole_eligibility_criteria <- subset(state_notes,
                                      state == state_for_report)$release_systems


#------------------------------------------------------------------------------#
# Highlighted Findings (page_national_trends.R)
#------------------------------------------------------------------------------#

# Load data created in page_national_trends.R
load(file = paste0(sp_data_path, "/data/analysis/app/map_percent.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))

# Get number of people currently eligible for parole
if (state_for_report %in% unique(parole_eligibility_table$state)) {
  state_data <- parole_eligibility_table |> filter(state == state_for_report)
} else {
  state_data <- no_data_text
}

# Get projected population past PE
proj_ppey <- parole_eligibility_table |> filter(state == state_for_report) |> pull(proj_pop_past_pey_rounded)

# Get percent projected population past PE
proj_pcnt_ppey <- parole_eligibility_table |> filter(state == state_for_report) |> pull(proj_pcnt_ppey_rounded)

# Get number of parole board members
parole_board_mem <- parole_eligibility_table |> filter(state == state_for_report) |> pull(members)





#------------------------------------------------------------------------------#
# Parole Eligibility
#------------------------------------------------------------------------------#

# Load necessary data files created in tab_parole_eligiblity.R
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pop_pe_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_line_pop_pe_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pe_ageyrend.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pe_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_sentlgth.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_pe_sentlgth.rds"))

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

state_sentence_pe_type <- all_sentence_pe_type[[state_for_report]]

state_pie_pe_type <- apply_chart_settings(
  all_pie_pe_type[[state_for_report]],
  height = 400
)

state_sentence_pop_pe_by_year <- all_sentence_pop_pe_by_year[[state_for_report]]

state_line_pop_pe_by_year <- apply_chart_settings(
  all_line_pop_pe_by_year[[state_for_report]],
  height = 500
)

state_sentence_pe_race <- all_sentence_pe_race[[state_for_report]]

state_bar_pe_race <- apply_chart_settings(
  all_bar_pe_race[[state_for_report]],
  height = 450,
  color = color4
)

state_sentence_pe_sex <- all_sentence_pe_sex[[state_for_report]]

state_bar_pe_sex <- apply_chart_settings(
  all_bar_pe_sex[[state_for_report]],
  height = 400,
  color = color4
)

state_sentence_pe_ageyrend <- all_sentence_pe_ageyrend[[state_for_report]]

state_bar_pe_ageyrend <- apply_chart_settings(
  all_bar_pe_ageyrend[[state_for_report]],
  height = 400,
  color = color4
)

state_sentence_pe_fbi_index <- all_sentence_pe_fbi_index[[state_for_report]]

state_bar_pe_fbi_index <- apply_chart_settings(
  all_bar_pe_fbi_index[[state_for_report]],
  height = 550,
  color = color4
)

state_sentence_pe_sentlgth <- all_sentence_pe_sentlgth[[state_for_report]]

state_bar_pe_sentlgth <- apply_chart_settings(
  all_bar_pe_sentlgth[[state_for_report]],
  height = 400,
  color = color4
)




#------------------------------------------------------------------------------#
# Population Tab (tab_population.R)
#------------------------------------------------------------------------------#

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_population_by_year.rds"))
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


state_sentence_population <- all_sentence_population_by_year[[state_for_report]]

state_line_population_by_year <- apply_chart_settings(
  all_line_population_by_year[[state_for_report]],
  height = 400,
  color = color2
)


state_sentence_population_race <- all_sentence_population_race[[state_for_report]]

state_bar_population_race <- apply_chart_settings(
  all_bar_population_race[[state_for_report]],
  height = 450,
  color = color2
)

state_sentence_population_sex <- all_sentence_population_sex[[state_for_report]]

state_bar_population_sex <- apply_chart_settings(
  all_bar_population_sex[[state_for_report]],
  height = 400,
  color = color2
)

state_sentence_population_ageyrend <- all_sentence_population_ageyrend[[state_for_report]]

state_bar_population_ageyrend <- apply_chart_settings(
  all_bar_population_ageyrend[[state_for_report]],
  height = 400,
  color = color2
)

state_sentence_population_fbi_index <- all_sentence_population_fbi_index[[state_for_report]]

state_bar_population_fbi_index <- apply_chart_settings(
  all_bar_population_fbi_index[[state_for_report]],
  height = 550,
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

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_line_releases_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_release_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_release_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_proportion_released.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_pe_release.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_agerlse.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_agerlse.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_releases_fbi_index.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_bar_releases_fbi_index.rds"))

state_sentence_releases <- all_sentence_releases_by_year[[state_for_report]]

state_line_releases_by_year <- apply_chart_settings(
  all_line_releases_by_year[[state_for_report]],
  height = 400,
  color = color5
)

state_sentence_pe_proportion_released <- all_sentence_pe_proportion_released[[state_for_report]]

state_stackedbar_pe_release <- apply_chart_settings(
  all_stackedbar_pe_release[[state_for_report]],
  height = 450
)

state_sentence_release_type <- all_sentence_release_type[[state_for_report]]

state_pie_release_type <- apply_chart_settings(
  all_pie_release_type[[state_for_report]],
  height = 350,
  color = c(color5, color3)
)

state_sentence_releases_race <- all_sentence_releases_race[[state_for_report]]

state_bar_releases_race <- apply_chart_settings(
  all_bar_releases_race[[state_for_report]],
  height = 450,
  color = color5
)

state_sentence_releases_sex <- all_sentence_releases_sex[[state_for_report]]

state_bar_releases_sex <- apply_chart_settings(
  all_bar_releases_sex[[state_for_report]],
  height = 400,
  color = color5
)

state_sentence_releases_agerlse <- all_sentence_releases_agerlse[[state_for_report]]

state_bar_releases_agerlse <- apply_chart_settings(
  all_bar_releases_agerlse[[state_for_report]],
  height = 400,
  color = color5
)

state_sentence_releases_fbi_index <- all_sentence_releases_fbi_index[[state_for_report]]

state_bar_releases_fbi_index <- apply_chart_settings(
  all_bar_releases_fbi_index[[state_for_report]],
  height = 550,
  color = color5
)







#------------------------------------------------------------------------------#
# RRI Disparities (tab_disparities_rris.R)
#------------------------------------------------------------------------------#

# Race and Ethnicity
load(file = paste0(sp_data_path, "/data/analysis/app/all_pe_rri_data.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pe_rri_data_male.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_black.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_hispanic.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_other.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/states_use_other_race_eth.rds"))

# Format state_for_report to lowercase and replace spaces with underscores
formatted_state_for_report <- str_to_lower(str_replace_all(state_for_report, " ", "_"))

# Update the file paths with the formatted state name
pe_rri_infographic_black <- paste0(
  sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_black_",
  formatted_state_for_report, ".png"
)

pe_rri_infographic_hispanic <- paste0(
  sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_hispanic_",
  formatted_state_for_report, ".png"
)

pe_rri_infographic_other <- paste0(
  sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_other_",
  formatted_state_for_report, ".png"
)


state_sentence_pe_rri_black <- all_sentence_pe_rri_black[[state_for_report]]

state_sentence_pe_rri_hispanic <- all_sentence_pe_rri_hispanic[[state_for_report]]

state_sentence_pe_rri_other <- all_sentence_pe_rri_other[[state_for_report]]

# Sex
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_male.rds"))

pe_rri_infographic_male <- paste0(
  sp_data_path, "/data/analysis/app/pngs/pe_rri_infographic_male_",
  formatted_state_for_report, ".png"
)

state_sentence_pe_rri_male <- all_sentence_pe_rri_male[[state_for_report]]




#------------------------------------------------------------------------------#
# Other Disparities (tab_disparities.R)
#------------------------------------------------------------------------------#

# Past PE
load(file = paste0(sp_data_path, "/data/analysis/app/avg_past_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/avg_past_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_avg_past_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_avg_past_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_sex_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_avg_past_pe_race_offense.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_scatter_avg_past_pe_sex_offense.rds"))

state_sentence_avg_past_pe_race <- all_sentence_avg_past_pe_race[[state_for_report]]

state_lollipop_avg_past_pe_race <- apply_chart_settings(
  all_lollipop_avg_past_pe_race[[state_for_report]]
)

state_scatter_avg_past_pe_race_offense <- apply_chart_settings(
  all_scatter_avg_past_pe_race_offense[[state_for_report]],
  height = 600
)

state_sentence_avg_past_pe_sex <- all_sentence_avg_past_pe_sex[[state_for_report]]

state_lollipop_avg_past_pe_sex <- apply_chart_settings(
  all_lollipop_avg_past_pe_sex[[state_for_report]],
  height = 175
)

state_sentence_avg_past_pe_race_offense <- all_sentence_avg_past_pe_race_offense[[state_for_report]]

state_scatter_avg_past_pe_race_offense <- apply_chart_settings(
  all_scatter_avg_past_pe_race_offense[[state_for_report]],
  height = 600
)

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
  height = 175
)

state_sentence_los_sex_offense <- all_sentence_los_sex_offense[[state_for_report]]

state_scatter_los_sex_offense <- apply_chart_settings(
  all_scatter_los_sex_offense[[state_for_report]],
  height = 600
)




