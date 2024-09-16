
#------------------------------------------------------------------------------#
# Missingness/No Data Text
#------------------------------------------------------------------------------#

# no data text
no_data_text <- paste0("Data is not available. ", state_for_report,
                       " did not submit this data to the National Corrections Reporting Program in ",
                       select_year, ".")
no_data_text <- ""


#------------------------------------------------------------------------------#
# Highlighted Findings (page_national_trends.R)
#------------------------------------------------------------------------------#

# Load Prepared Data
load(file = paste0(config$sp_data_path, "/data/analysis/app/parole_eligibility_table.rds"))

# Get number of people currently eligible for parole
if (state_for_report %in% unique(parole_eligibility_table$state)) {
  state_data <- parole_eligibility_table |> filter(state == state_for_report)
} else {
  state_data <- no_data_text
}

num_people_current <- state_data |> filter(state == state_for_report) |> pull(current_count)
num_people_current_perc <- state_data |> filter(state == state_for_report) |> pull(current_perc)
num_parole_board_mem <- state_data |> filter(state == state_for_report) |> pull(parole_board_members)

#------------------------------------------------------------------------------#
# Parole Eligibility Tab (tab_parole_eligibility.R)
#------------------------------------------------------------------------------#

load(file = paste0(config$sp_data_path, "/data/analysis/app/carl_state_notes.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/parole_info_by_state.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_pe_type.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stackedbar_pe_type.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_pop_pe_by_year.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stackedbar_pop_pe_by_year.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_sex.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_sex.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_ageyrend.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_ageyrend.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_ped_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_sentlgth.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_sentlgth.rds"))

# TITLE: How is Parole Eligibility Determined?
parole_eligibility_criteria <- subset(carl_state_notes,
                                      state == state_for_report)$parole_eligibility_criteria

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison.
if (state_for_report %in% names(all_sentence_pe_type)) {
  state_sentence_pe_type <-
    all_sentence_pe_type[[state_for_report]]
} else {
  state_sentence_pe_type <- ""
}

# PAROLE ELIGIBILITY TRENDS ------------------

# TITLE: Pct. of Prison Population by Parole Eligibility Status
# Stacked bar chart showing the  proportion of parole eligibility types
if (state_for_report %in% names(all_stackedbar_pe_type)) {
  state_stackedbar_pe_type <-
    all_stackedbar_pe_type[[state_for_report]] |>
    hc_size(height = 170)
} else {
  state_stackedbar_pe_type <- no_data_text
}

# SENTENCE: From X to X, the proportion of people in prison past parole eligibility increased/decreased by X percent/or stayed the same.
if (state_for_report %in% names(all_sentence_pop_pe_by_year)) {
  state_sentence_pop_pe_by_year <-
    all_sentence_pop_pe_by_year[[state_for_report]]
} else {
  state_sentence_pop_pe_by_year <- ""
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
  state_sentence_parole_eligibility_race <- ""
}

if (state_for_report %in% names(all_bar_parole_eligibility_race)) {
  state_bar_parole_eligibility_race <-
    all_bar_parole_eligibility_race[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_parole_eligibility_race <- ""
}

if (state_for_report %in% names(all_sentence_parole_eligibility_sex)) {
  state_sentence_parole_eligibility_sex <-
    all_sentence_parole_eligibility_sex[[state_for_report]]
} else {
  state_sentence_parole_eligibility_sex <- ""
}

if (state_for_report %in% names(all_bar_parole_eligibility_sex)) {
  state_bar_parole_eligibility_sex <-
    all_bar_parole_eligibility_sex[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_parole_eligibility_sex <- ""
}

if (state_for_report %in% names(all_sentence_parole_eligibility_ageyrend)) {
  state_sentence_parole_eligibility_ageyrend <-
    all_sentence_parole_eligibility_ageyrend[[state_for_report]]
} else {
  state_sentence_parole_eligibility_ageyrend <- ""
}

if (state_for_report %in% names(all_bar_parole_eligibility_ageyrend)) {
  state_bar_parole_eligibility_ageyrend <-
    all_bar_parole_eligibility_ageyrend[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_bar_parole_eligibility_ageyrend <- ""
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
  state_sentence_parole_eligibility_fbi_index <- ""
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
  state_sentence_parole_eligibility_sentlgth <- ""
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

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_line_population_by_year.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_population_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population_sex.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_population_sex.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population_ageyrend.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_population_ageyrend.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_population_fbi_index.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_population_sentlgth.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_population_sentlgth.rds"))

# SENTENCE: "From YEAR to YEAR, the prison population decreased/increased X percent."
if (state_for_report %in% names(all_sentence_population)) {
  state_sentence_population <-
    all_sentence_population[[state_for_report]]
} else {
  state_sentence_population <- ""
}

# TITLE: Prison Population by Year
if (state_for_report %in% names(all_line_population_by_year)) {
  state_line_population_by_year <-
    all_line_population_by_year[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_line_population_by_year <- no_data_text
}


# DEMOGRAPHICS ------------------

if (state_for_report %in% names(all_sentence_population_race)) {
  state_sentence_population_race <-
    all_sentence_population_race[[state_for_report]]
} else {
  state_sentence_population_race <- ""
}

if (state_for_report %in% names(all_bar_population_race)) {
  state_bar_population_race <-
    all_bar_population_race[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color2))
} else {
  state_bar_population_race <- ""
}

if (state_for_report %in% names(all_sentence_population_sex)) {
  state_sentence_population_sex <-
    all_sentence_population_sex[[state_for_report]]
} else {
  state_sentence_population_sex <- ""
}

if (state_for_report %in% names(all_bar_population_sex)) {
  state_bar_population_sex <-
    all_bar_population_sex[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color2))
} else {
  state_bar_population_sex <- ""
}

if (state_for_report %in% names(all_sentence_population_ageyrend)) {
  state_sentence_population_ageyrend <-
    all_sentence_population_ageyrend[[state_for_report]]
} else {
  state_sentence_population_ageyrend <- ""
}

if (state_for_report %in% names(all_bar_population_ageyrend)) {
  state_bar_population_ageyrend <-
    all_bar_population_ageyrend[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color2))
} else {
  state_bar_population_ageyrend <- ""
}

# OFFENSE TYPE ------------------

if (state_for_report %in% names(all_sentence_population_fbi_index)) {
  state_sentence_population_fbi_index <-
    all_sentence_population_fbi_index[[state_for_report]]
} else {
  state_sentence_population_fbi_index <- ""
}

if (state_for_report %in% names(all_bar_population_fbi_index)) {
  state_bar_population_fbi_index <-
    all_bar_population_fbi_index[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color2))
} else {
  state_bar_population_fbi_index <- ""
}

# SENTENCE LENGTH ------------------

if (state_for_report %in% names(all_sentence_population_sentlgth)) {
  state_sentence_population_sentlgth <-
    all_sentence_population_sentlgth[[state_for_report]]
} else {
  state_sentence_population_sentlgth <- ""
}

if (state_for_report %in% names(all_bar_population_sentlgth)) {
  state_bar_population_sentlgth <-
    all_bar_population_sentlgth[[state_for_report]] |>
    hc_size(height = 300) |>
    hc_colors(c(color2))
} else {
  state_bar_population_sentlgth <- ""
}




#------------------------------------------------------------------------------#
# Releases Tab (tab_releases.R)
#------------------------------------------------------------------------------#

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_releases.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_line_releases_by_year.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_pie_release_type.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_pe_proportion_released.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stackedbar_parole_eligibility_release.rds"))

# SENTENCE: "From YEAR to YEAR, prison releases decreased/increased X percent."
if (state_for_report %in% names(all_sentence_releases)) {
  state_sentence_releases <-
    all_sentence_releases[[state_for_report]]
} else {
  state_sentence_releases <- ""
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
  state_sentence_pe_proportion_released <- ""
}

# TITLE: Parole-Eligible Prison Population Released by Year
if (state_for_report %in% names(all_stackedbar_parole_eligibility_release)) {
  state_stackedbar_parole_eligibility_release <-
    all_stackedbar_parole_eligibility_release[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_stackedbar_parole_eligibility_release <- no_data_text
}

# TITLE: Proportion of Conditional vs Unconditional Releases
if (state_for_report %in% names(all_pie_release_type)) {
  state_pie_release_type <-
    all_pie_release_type[[state_for_report]] |>
    hc_size(height = 225)
} else {
  state_pie_release_type <- no_data_text
}









####################

# Disparities

####################

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_parole_release_disparities.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_scatter_race_ped_release.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bubble_race_ped_release.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_hc_rri_chart.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_rri.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_los_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_lollipop_los_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_los_race_offense.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_scatter_los_race_offense.rds"))

# SENTENCE: "In STATE, X people are incarcerated at a rate X
#            times</b> higher than White non-Hispanic people, when accounting for
#            population sizes in the community."
if (state_for_report %in% names(all_sentence_rri)) {
  state_sentence_rri <-
    all_sentence_rri[[state_for_report]]
} else {
  state_sentence_rri <- ""
}

# SENTENCE: "Hispanic, any race individuals faced the longest average time
#            served in prison in 2020, with an average of 3.8 years.
#            White, non-Hispanic individuals experienced shorter prison stays,
#            averaging 2.3 years compared to their counterparts."
if (state_for_report %in% names(all_sentence_los_race)) {
  state_sentence_los_race <-
    all_sentence_los_race[[state_for_report]]
} else {
  state_sentence_los_race <- ""
}

# TITLE: Average Length of Stay by Race, Ethnicity, and Offense Type
if (state_for_report %in% names(all_lollipop_los_race)) {
  state_lollipop_los_race <-
    all_lollipop_los_race[[state_for_report]] |>
    hc_size(height = 150)
} else {
  state_lollipop_los_race <- no_data_text
}


# SENTENCE: "By offense type, disparities were observed in time served by race
#            and ethnicity. For Robbery offenses, Hispanic, any race individuals
#            had 4.47 more years on average compared to Other race(s), non-Hispanic
#            individuals, who had the shortest time served for these offenses."
if (state_for_report %in% names(all_sentence_los_race_offense)) {
  state_sentence_los_race_offense <-
    all_sentence_los_race_offense[[state_for_report]]
} else {
  state_sentence_los_race_offense <- ""
}

if (state_for_report %in% names(all_scatter_los_race_offense)) {
  state_scatter_los_race_offense <-
    all_scatter_los_race_offense[[state_for_report]] |>
    hc_size(height = 600)
} else {
  state_scatter_los_race_offense <- no_data_text
}



