
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
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stackedbar_pe_type.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_pe_type.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_ped_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_sentence_parole_eligibility_sentlgth.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_bar_parole_eligibility_sentlgth.rds"))


# TITLE: How is Parole Eligibility Determined?
parole_eligibility_criteria <- subset(carl_state_notes,
                                      state == state_for_report)$parole_eligibility_criteria

# TITLE: Pct. of Prison Population by Parole Eligibility Status
# Stacked bar chart showing the  proportion of parole eligibility types
if (state_for_report %in% names(all_stackedbar_pe_type)) {
  state_stackedbar_pe_type <-
    all_stackedbar_pe_type[[state_for_report]] |>
    hc_size(height = 170)
} else {
  state_stackedbar_pe_type <- no_data_text
}

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison for
#           new crimes and sentence lengths between 1-25 years.
if (state_for_report %in% names(all_sentence_pe_type)) {
  state_sentence_pe_type <-
    all_sentence_pe_type[[state_for_report]]
} else {
  state_sentence_pe_type <- ""
}










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

# SENTENCE: In YEAR, among the prison population eligible for parole but not yet
#           released, people with sentences between X years constituted
#           the majority, representing X percent.
if (state_for_report %in% names(all_sentence_parole_eligibility_sentlgth)) {
  state_sentence_parole_eligibility_sentlgth <-
    all_sentence_parole_eligibility_sentlgth[[state_for_report]]
} else {
  state_sentence_parole_eligibility_sentlgth <- ""
}

# TITLE:  Offense Breakdown for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_bar_ped_fbi_index)) {
  state_bar_ped_fbi_index <-
    all_bar_ped_fbi_index[[state_for_report]] |>
    hc_size(height = 400) |>
    hc_colors(c(color1))
} else {
  state_bar_ped_fbi_index <- no_data_text
}

# TITLE: Sentence Lengths for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_bar_parole_eligibility_sentlgth)) {
  state_bar_parole_eligibility_sentlgth <-
    all_bar_parole_eligibility_sentlgth[[state_for_report]] |>
    hc_size(height = 400) |>
    hc_colors(c(color5))
} else {
  state_bar_parole_eligibility_sentlgth <- no_data_text
}


#------------------------------------------------------------------------------#
# Population Tab (tab_population.R)
#------------------------------------------------------------------------------#

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stacked_bar_pe_race.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stacked_bar_pe_sex.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stacked_bar_pe_ageyrend.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stacked_bar_pe_fbi_index.rds"))
load(file = paste0(config$sp_data_path, "/data/analysis/app/all_stacked_bar_pe_sentlgth.rds"))

load(file = paste0(config$sp_data_path, "/data/analysis/app/all_line_pop_pe_by_year.rds"))

# TITLE: Line chart
if (state_for_report %in% names(all_line_pop_pe_by_year)) {
  state_line_pop_pe_by_year <-
    all_line_pop_pe_by_year[[state_for_report]] |>
    hc_title(text = "Prison Population Trends") |>
    hc_size(height = 400) |>
    hc_colors(c(color4, color2))
} else {
  state_line_pop_pe_by_year <- no_data_text
}

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_stacked_bar_pe_race)) {
  state_stacked_bar_pe_race <-
    all_stacked_bar_pe_race[[state_for_report]]|>
    hc_chart(backgroundColor = "white") |>
    hc_size(height = 400)
} else {
  state_stacked_bar_pe_race <- no_data_text
}

# TITLE: Sex
if (state_for_report %in% names(all_stacked_bar_pe_sex)) {
  state_stacked_bar_pe_sex <-
    all_stacked_bar_pe_sex[[state_for_report]]|>
    hc_chart(backgroundColor = "white") |>
    hc_size(height = 400)
} else {
  state_stacked_bar_pe_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_stacked_bar_pe_ageyrend)) {
  state_stacked_bar_pe_ageyrend <-
    all_stacked_bar_pe_ageyrend[[state_for_report]]|>
    hc_chart(backgroundColor = "white") |>
    hc_size(height = 400)
} else {
  state_stacked_bar_pe_ageyrend <- no_data_text
}

# TITLE: Offense Breakdown for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_stacked_bar_pe_fbi_index)) {
  state_stacked_bar_pe_fbi_index <-
    all_stacked_bar_pe_fbi_index[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_stacked_bar_pe_fbi_index <- no_data_text
}

# TITLE: Sentence Lengths for People in Prison Past Their Parole Eligibility Year
if (state_for_report %in% names(all_stacked_bar_pe_sentlgth)) {
  state_stacked_bar_pe_sentlgth <-
    all_stacked_bar_pe_sentlgth[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_stacked_bar_pe_sentlgth <- no_data_text
}




#------------------------------------------------------------------------------#
# Releases Tab (tab_releases.R)
#------------------------------------------------------------------------------#
















