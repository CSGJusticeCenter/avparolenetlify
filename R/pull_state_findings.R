
#------------------------------------------------------------------------------#
# Missingness/No Data Text
#------------------------------------------------------------------------------#

# no data text
no_data_text <- paste0("Data is not available. ", state_for_report,
                       " did not submit this data to the National Corrections Reporting Program in ",
                       select_year, ".")
no_sentence <- ""
no_visualization <- paste0("Data is not available. ", state_for_report,
                           " did not submit this data to the National Corrections Reporting Program in ",
                           select_year, ".")



#------------------------------------------------------------------------------#
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
# Parole Eligibility Tab (tab_parole_eligibility.R)
#------------------------------------------------------------------------------#

load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_type.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_pie_pe_type.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pop_pe_by_year.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_stackedbar_pop_pe_by_year.rds"))

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

# TITLE: How is Parole Eligibility Determined?
parole_eligibility_criteria <- subset(state_notes,
                                      state == state_for_report)$release_systems

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison.
if (state_for_report %in% names(all_sentence_pe_type)) {
  state_sentence_pe_type <-
    all_sentence_pe_type[[state_for_report]]
} else {
  state_sentence_pe_type <- no_sentence
}

# PAROLE ELIGIBILITY TRENDS ------------------

# TITLE: Pct. of Prison Population by Parole Eligibility Status
# Stacked bar chart showing the  proportion of parole eligibility types
# if (state_for_report %in% names(all_stackedbar_pe_type)) {
#   state_stackedbar_pe_type <-
#     all_stackedbar_pe_type[[state_for_report]] |>
#     hc_size(height = 170)
# } else {
#   state_stackedbar_pe_type <- no_data_text
# }
# Pie option- TEMP
if (state_for_report %in% names(all_pie_pe_type)) {
  state_pie_pe_type <-
    all_pie_pe_type[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_pie_pe_type <- no_data_text
}


# SENTENCE: From X to X, the proportion of people in prison past parole eligibility increased/decreased by X percent/or stayed the same.
if (state_for_report %in% names(all_sentence_pop_pe_by_year)) {
  state_sentence_pop_pe_by_year <-
    all_sentence_pop_pe_by_year[[state_for_report]]
} else {
  state_sentence_pop_pe_by_year <- no_sentence
}

# TITLE: Line chart
if (state_for_report %in% names(all_stackedbar_pop_pe_by_year)) {
  state_stackedbar_pop_pe_by_year <-
    all_stackedbar_pop_pe_by_year[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_stackedbar_pop_pe_by_year <- no_data_text
}


# DEMOGRAPHICS ------------------

if (state_for_report %in% names(all_sentence_parole_eligibility_race)) {
  state_sentence_parole_eligibility_race <-
    all_sentence_parole_eligibility_race[[state_for_report]]
} else {
  state_sentence_parole_eligibility_race <- no_sentence
}

if (state_for_report %in% names(all_bar_parole_eligibility_race)) {
  state_bar_parole_eligibility_race <-
    all_bar_parole_eligibility_race[[state_for_report]] |>
    hc_size(height = 300) |>
  hc_colors(c(color4))
} else {
  state_bar_parole_eligibility_race <- no_visualization
}

if (state_for_report %in% names(all_sentence_parole_eligibility_sex)) {
  state_sentence_parole_eligibility_sex <-
    all_sentence_parole_eligibility_sex[[state_for_report]]
} else {
  state_sentence_parole_eligibility_sex <- no_sentence
}

if (state_for_report %in% names(all_bar_parole_eligibility_sex)) {
  state_bar_parole_eligibility_sex <-
    all_bar_parole_eligibility_sex[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color4))
} else {
  state_bar_parole_eligibility_sex <- no_visualization
}

if (state_for_report %in% names(all_sentence_parole_eligibility_ageyrend)) {
  state_sentence_parole_eligibility_ageyrend <-
    all_sentence_parole_eligibility_ageyrend[[state_for_report]]
} else {
  state_sentence_parole_eligibility_ageyrend <- no_sentence
}

if (state_for_report %in% names(all_bar_parole_eligibility_ageyrend)) {
  state_bar_parole_eligibility_ageyrend <-
    all_bar_parole_eligibility_ageyrend[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color4))
} else {
  state_bar_parole_eligibility_ageyrend <- no_visualization
}





# OFFENSE TYPE ------------------

# SENTENCE: In 2020, 61% of people in prison past their parole consideration year
#           were in prison for violent offenses. The breakdown of criminal
#           offenses of people in prison past their parole consideration year
#           reveals a varied landscape, with the majority of people incarcerated
#           for aggravated or simple assault (26%) and property (19%) offenses."
if (state_for_report %in% names(all_sentence_parole_eligibility_fbi_index)) {
  state_sentence_parole_eligibility_fbi_index <-
    all_sentence_parole_eligibility_fbi_index[[state_for_report]]
} else {
  state_sentence_parole_eligibility_fbi_index <- no_sentence
}

# TITLE:  Offense Breakdown for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_bar_ped_fbi_index)) {
  state_bar_ped_fbi_index <-
    all_bar_ped_fbi_index[[state_for_report]] |>
    hc_size(height = 400) |>
    hc_colors(c(color4))
} else {
  state_bar_ped_fbi_index <- no_data_text
}

# SENTENCE LENGTH ------------------

# SENTENCE: In YEAR, among the prison population eligible for parole but not yet
#           released, people with sentences between X years constituted
#           the majority, representing X percent.
if (state_for_report %in% names(all_sentence_parole_eligibility_sentlgth)) {
  state_sentence_parole_eligibility_sentlgth <-
    all_sentence_parole_eligibility_sentlgth[[state_for_report]]
} else {
  state_sentence_parole_eligibility_sentlgth <- no_sentence
}

# TITLE: Sentence Lengths for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_bar_parole_eligibility_sentlgth)) {
  state_bar_parole_eligibility_sentlgth <-
    all_bar_parole_eligibility_sentlgth[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color4))
} else {
  state_bar_parole_eligibility_sentlgth <- no_data_text
}


















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

# SENTENCE: "From YEAR to YEAR, the prison population decreased/increased X percent."
if (state_for_report %in% names(all_sentence_population)) {
  state_sentence_population <-
    all_sentence_population[[state_for_report]]
} else {
  state_sentence_population <- no_sentence
}

# TITLE: Prison Population by Year
if (state_for_report %in% names(all_line_population_by_year)) {
  state_line_population_by_year <-
    all_line_population_by_year[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 300)
} else {
  state_line_population_by_year <- no_data_text
}


# DEMOGRAPHICS ------------------

if (state_for_report %in% names(all_sentence_population_race)) {
  state_sentence_population_race <-
    all_sentence_population_race[[state_for_report]]
} else {
  state_sentence_population_race <- no_sentence
}

if (state_for_report %in% names(all_bar_population_race)) {
  state_bar_population_race <-
    all_bar_population_race[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 300)
} else {
  state_bar_population_race <- no_visualization
}

if (state_for_report %in% names(all_sentence_population_sex)) {
  state_sentence_population_sex <-
    all_sentence_population_sex[[state_for_report]]
} else {
  state_sentence_population_sex <- no_sentence
}

if (state_for_report %in% names(all_bar_population_sex)) {
  state_bar_population_sex <-
    all_bar_population_sex[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 300)
} else {
  state_bar_population_sex <- no_visualization
}

if (state_for_report %in% names(all_sentence_population_ageyrend)) {
  state_sentence_population_ageyrend <-
    all_sentence_population_ageyrend[[state_for_report]]
} else {
  state_sentence_population_ageyrend <- no_sentence
}

if (state_for_report %in% names(all_bar_population_ageyrend)) {
  state_bar_population_ageyrend <-
    all_bar_population_ageyrend[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 300)
} else {
  state_bar_population_ageyrend <- no_visualization
}

# OFFENSE TYPE ------------------

if (state_for_report %in% names(all_sentence_population_fbi_index)) {
  state_sentence_population_fbi_index <-
    all_sentence_population_fbi_index[[state_for_report]]
} else {
  state_sentence_population_fbi_index <- no_sentence
}

if (state_for_report %in% names(all_bar_population_fbi_index)) {
  state_bar_population_fbi_index <-
    all_bar_population_fbi_index[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 400)
} else {
  state_bar_population_fbi_index <- no_visualization
}

# SENTENCE LENGTH ------------------

if (state_for_report %in% names(all_sentence_population_sentlgth)) {
  state_sentence_population_sentlgth <-
    all_sentence_population_sentlgth[[state_for_report]]
} else {
  state_sentence_population_sentlgth <- no_sentence
}

if (state_for_report %in% names(all_bar_population_sentlgth)) {
  state_bar_population_sentlgth <-
    all_bar_population_sentlgth[[state_for_report]] |>
    hc_colors(c(color2)) |>
    hc_size(height = 400)
} else {
  state_bar_population_sentlgth <- no_visualization
}




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

# SENTENCE: "From YEAR to YEAR, prison releases decreased/increased X percent."
if (state_for_report %in% names(all_sentence_releases)) {
  state_sentence_releases <-
    all_sentence_releases[[state_for_report]]
} else {
  state_sentence_releases <- no_sentence
}

# TITLE: Prison Releases by Year
if (state_for_report %in% names(all_line_releases_by_year)) {
  state_line_releases_by_year <-
    all_line_releases_by_year[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_line_releases_by_year <- no_data_text
}

# SENTENCE: In 2020, 40% of people eligible for parole were released during
#           their eligibility year. This represents a 3% decrease compared to 2010.
if (state_for_report %in% names(all_sentence_pe_proportion_released)) {
  state_sentence_pe_proportion_released <-
    all_sentence_pe_proportion_released[[state_for_report]]
} else {
  state_sentence_pe_proportion_released <- no_sentence
}

# TITLE: Parole-Eligible Prison Population Released by Year
if (state_for_report %in% names(all_stackedbar_parole_eligibility_release)) {
  state_stackedbar_parole_eligibility_release <-
    all_stackedbar_parole_eligibility_release[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_stackedbar_parole_eligibility_release <- no_data_text
}

# SENTENCE: "In YEAR, X percent of prison releases were conditional releases."
if (state_for_report %in% names(all_sentence_release_type)) {
  state_sentence_release_type <-
    all_sentence_release_type[[state_for_report]]
} else {
  state_sentence_release_type <- no_sentence
}

# TITLE: Proportion of Conditional vs Unconditional Releases
if (state_for_report %in% names(all_pie_release_type)) {
  state_pie_release_type <-
    all_pie_release_type[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_pie_release_type <- no_data_text
}

# DEMOGRAPHICS ------------------

if (state_for_report %in% names(all_sentence_releases_race)) {
  state_sentence_releases_race <-
    all_sentence_releases_race[[state_for_report]]
} else {
  state_sentence_releases_race <- no_sentence
}

if (state_for_report %in% names(all_bar_releases_race)) {
  state_bar_releases_race <-
    all_bar_releases_race[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_releases_race <- no_visualization
}

if (state_for_report %in% names(all_sentence_releases_sex)) {
  state_sentence_releases_sex <-
    all_sentence_releases_sex[[state_for_report]]
} else {
  state_sentence_releases_sex <- no_sentence
}

if (state_for_report %in% names(all_bar_releases_sex)) {
  state_bar_releases_sex <-
    all_bar_releases_sex[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_releases_sex <- no_visualization
}

if (state_for_report %in% names(all_sentence_releases_agerlse)) {
  state_sentence_releases_agerlse <-
    all_sentence_releases_agerlse[[state_for_report]]
} else {
  state_sentence_releases_agerlse <- no_sentence
}

if (state_for_report %in% names(all_bar_releases_agerlse)) {
  state_bar_releases_agerlse <-
    all_bar_releases_agerlse[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_releases_agerlse <- no_visualization
}

# OFFENSE TYPE ------------------

if (state_for_report %in% names(all_sentence_releases_fbi_index)) {
  state_sentence_releases_fbi_index <-
    all_sentence_releases_fbi_index[[state_for_report]]
} else {
  state_sentence_releases_fbi_index <- no_sentence
}

if (state_for_report %in% names(all_bar_releases_fbi_index)) {
  state_bar_releases_fbi_index <-
    all_bar_releases_fbi_index[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_bar_releases_fbi_index <- no_visualization
}







####################

# Disparities

####################

# RRIs
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_rri_black.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_rri_hispanic.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_black.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_hispanic.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_pe_rri_male.rds"))

rri_infographic_black       <- paste0(sp_data_path, "/data/analysis/app/rri_infographic_black_",
                                      state_for_report, ".png")
rri_infographic_hispanic    <- paste0(sp_data_path, "/data/analysis/app/rri_infographic_hispanic_",
                                      state_for_report, ".png")
pe_rri_infographic_black    <- paste0(sp_data_path, "/data/analysis/app/pe_rri_infographic_black_",
                                      state_for_report, ".png")
pe_rri_infographic_hispanic <- paste0(sp_data_path, "/data/analysis/app/pe_rri_infographic_hispanic_",
                                      state_for_report, ".png")

# SENTENCE: "In STATE, X people are incarcerated at a rate X
#            times</b> higher than White non-Hispanic people, when accounting for
#            population sizes in the community."
if (state_for_report %in% names(all_sentence_rri_black)) {
  state_sentence_rri_black <-
    all_sentence_rri_black[[state_for_report]]
} else {
  state_sentence_rri_black <- no_sentence
}
if (state_for_report %in% names(all_sentence_rri_hispanic)) {
  state_sentence_rri_hispanic <-
    all_sentence_rri_hispanic[[state_for_report]]
} else {
  state_sentence_rri_hispanic <- no_sentence
}

if (state_for_report %in% names(all_sentence_pe_rri_black)) {
  state_sentence_pe_rri_black <-
    all_sentence_pe_rri_black[[state_for_report]]
} else {
  state_sentence_pe_rri_black <- no_sentence
}
if (state_for_report %in% names(all_sentence_pe_rri_hispanic)) {
  state_sentence_pe_rri_hispanic <-
    all_sentence_pe_rri_hispanic[[state_for_report]]
} else {
  state_sentence_pe_rri_hispanic <- no_sentence
}

if (state_for_report %in% names(all_sentence_pe_rri_male)) {
  state_sentence_pe_rri_male <-
    all_sentence_pe_rri_male[[state_for_report]]
} else {
  state_sentence_pe_rri_male <- no_sentence
}

# LOS
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_los_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_lollipop_los_sex.rds"))

# SENTENCE: "Hispanic, any race individuals faced the longest average time
#            served in prison in 2020, with an average of 3.8 years.
#            White, non-Hispanic individuals experienced shorter prison stays,
#            averaging 2.3 years compared to their counterparts."
if (state_for_report %in% names(all_sentence_los_race)) {
  state_sentence_los_race <-
    all_sentence_los_race[[state_for_report]]
} else {
  state_sentence_los_race <- no_sentence
}

# TITLE: Average Length of Stay by Race, Ethnicity, and Offense Type
if (state_for_report %in% names(all_lollipop_los_race)) {
  state_lollipop_los_race <-
    all_lollipop_los_race[[state_for_report]] |>
    hc_size(height = 100)
} else {
  state_lollipop_los_race <- no_data_text
}

# SENTENCE: "Hispanic, any sex individuals faced the longest average time
#            served in prison in 2020, with an average of 3.8 years.
#            White, non-Hispanic individuals experienced shorter prison stays,
#            averaging 2.3 years compared to their counterparts."
if (state_for_report %in% names(all_sentence_los_sex)) {
  state_sentence_los_sex <-
    all_sentence_los_sex[[state_for_report]]
} else {
  state_sentence_los_sex <- no_sentence
}

# TITLE: Average Length of Stay by sex, Ethnicity, and Offense Type
if (state_for_report %in% names(all_lollipop_los_sex)) {
  state_lollipop_los_sex <-
    all_lollipop_los_sex[[state_for_report]] |>
    hc_size(height = 100)
} else {
  state_lollipop_los_sex <- no_data_text
}


load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_los_race_offense.rds"))

# SENTENCE: "By offense type, disparities were observed in time served by race
#            and ethnicity. For Robbery offenses, Hispanic, any race individuals
#            had 4.47 more years on average compared to White
#            individuals."
if (state_for_report %in% names(all_sentence_los_race_offense)) {
  state_sentence_los_race_offense <-
    all_sentence_los_race_offense[[state_for_report]]
} else {
  state_sentence_los_race_offense <- no_sentence
}




# Past PE
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/all_sentence_avg_past_pe_sex.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/avg_current_pe_race.rds"))
load(file = paste0(sp_data_path, "/data/analysis/app/avg_current_pe_sex.rds"))

if (state_for_report %in% names(all_sentence_avg_past_pe_race)) {
  state_sentence_avg_past_pe_race <-
    all_sentence_avg_past_pe_race[[state_for_report]]
} else {
  state_sentence_avg_past_pe_race <- no_sentence
}

if (state_for_report %in% names(all_sentence_avg_past_pe_sex)) {
  state_sentence_avg_past_pe_sex <-
    all_sentence_avg_past_pe_sex[[state_for_report]]
} else {
  state_sentence_avg_past_pe_sex <- no_sentence
}
















#

#
# if (state_for_report %in% names(all_scatter_los_race_offense)) {
#   state_scatter_los_race_offense <-
#     all_scatter_los_race_offense[[state_for_report]] |>
#     hc_size(height = 600)
# } else {
#   state_scatter_los_race_offense <- no_data_text
# }
#
# if (state_for_report %in% names(all_sentence_avg_past_pe_race_offense)) {
#   state_sentence_avg_past_pe_race_offense <-
#     all_sentence_avg_past_pe_race_offense[[state_for_report]]
# } else {
#   state_sentence_avg_past_pe_race_offense <- no_sentence
# }
#
# if (state_for_report %in% names(all_scatter_avg_past_pe_race_offense)) {
#   state_scatter_avg_past_pe_race_offense <-
#     all_scatter_avg_past_pe_race_offense[[state_for_report]] |>
#     hc_size(height = 600)
# } else {
#   state_scatter_avg_past_pe_race_offense <- no_data_text
# }
#
# # SENTENCE: "By offense type, disparities were observed in time served by sex
# #            and ethnicity. For Robbery offenses, Hispanic, any sex individuals
# #            had 4.47 more years on average compared to Other sex(s), non-Hispanic
# #            individuals, who had the shortest time served for these offenses."
# if (state_for_report %in% names(all_sentence_los_sex_offense)) {
#   state_sentence_los_sex_offense <-
#     all_sentence_los_sex_offense[[state_for_report]]
# } else {
#   state_sentence_los_sex_offense <- no_sentence
# }
#
# if (state_for_report %in% names(all_scatter_los_sex_offense)) {
#   state_scatter_los_sex_offense <-
#     all_scatter_los_sex_offense[[state_for_report]] |>
#     hc_size(height = 600)
# } else {
#   state_scatter_los_sex_offense <- no_data_text
# }
#
#
# if (state_for_report %in% names(all_sentence_avg_past_pe_sex_offense)) {
#   state_sentence_avg_past_pe_sex_offense <-
#     all_sentence_avg_past_pe_sex_offense[[state_for_report]]
# } else {
#   state_sentence_avg_past_pe_sex_offense <- no_sentence
# }
#
# if (state_for_report %in% names(all_scatter_avg_past_pe_sex_offense)) {
#   state_scatter_avg_past_pe_sex_offense <-
#     all_scatter_avg_past_pe_sex_offense[[state_for_report]] |>
#     hc_size(height = 600)
# } else {
#   state_scatter_avg_past_pe_sex_offense <- no_data_text
# }
#

