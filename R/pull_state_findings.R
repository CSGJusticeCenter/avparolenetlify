
select_year <- 2020

# no data text
no_data_text <- paste0("Data is not available. ", state_for_report,
                       " did not submit this data to the National Corrections Reporting Program in ",
                       select_year, ".")

####################

# Highlighted Findings

####################

# Get number of people currently eligible for parole
if (state_for_report %in% unique(parole_eligibility_table$state)) {
  state_data <- parole_eligibility_table |> filter(state == state_for_report)
} else {
  state_data <- no_data_text
}

num_people_current <- state_data |> filter(state == state_for_report) |> pull(current_count)
num_people_current_perc <- state_data |> filter(state == state_for_report) |> pull(current_perc)
num_parole_board_mem <- state_data |> filter(state == state_for_report) |> pull(parole_board_members)



####################

# Parole Eligibility

####################

# TITLE: How is Parole Eligibility Determined?
parole_eligibility_criteria <- subset(carl_state_notes,
                                      state == state_for_report)$parole_eligibility_criteria

# TITLE: Pct. of Prison Population by Parole Eligibility Status
# Stacked bar chart showing the  proportion of parole eligibility types
if (state_for_report %in% names(all_stackedbar_pe_type)) {
  state_stackedbar_pe_type <-
    all_stackedbar_pe_type[[state_for_report]] |>
    hc_size(height = 175)
} else {
  state_stackedbar_pe_type <- no_data_text
}

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison for
#           new crimes and sentence lengths between 1-25 years.
if (state_for_report %in% names(all_sentence_parole_eligibility_population)) {
  state_sentence_parole_eligibility_population <-
    all_sentence_parole_eligibility_population[[state_for_report]]
} else {
  state_sentence_parole_eligibility_population <- ""
}

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_waffle_parole_eligibility_race)) {
  state_waffle_parole_eligibility_race <-
    all_waffle_parole_eligibility_race[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Race and Ethnicity"))
} else {
  state_waffle_parole_eligibility_race <- no_data_text
}

# TITLE: Gender
if (state_for_report %in% names(all_waffle_parole_eligibility_sex)) {
  state_waffle_parole_eligibility_sex <-
    all_waffle_parole_eligibility_sex[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Gender"))
} else {
  state_waffle_parole_eligibility_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_waffle_parole_eligibility_ageyrend)) {
  state_waffle_parole_eligibility_ageyrend <-
    all_waffle_parole_eligibility_ageyrend[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Age"))
} else {
  state_waffle_parole_eligibility_ageyrend <- no_data_text
}

# TITLE: Years Spent in Prison After Parole Eligibility
if (state_for_report %in% names(all_scatter_race_ped_release)) {
  state_scatter_race_ped_release <-
    all_scatter_race_ped_release[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_scatter_race_ped_release <- no_data_text
}

# TITLE: Offense Breakdown for People in Prison Past Their Parole Eligibility Date
if (state_for_report %in% names(all_bubble_ped_fbi_index)) {
  state_bubble_ped_fbi_index <-
    all_bubble_ped_fbi_index[[state_for_report]] |>
    hc_size(height = 250)
} else {
  state_bubble_ped_fbi_index <- no_data_text
}

# TITLE: Sentence Lengths for People in Prison Past Their Parole Eligibility Date
if (state_for_report %in% names(all_bar_parole_eligibility_sentlgth)) {
  state_bar_parole_eligibility_sentlgth <-
    all_bar_parole_eligibility_sentlgth[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_bar_parole_eligibility_sentlgth <- no_data_text
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







####################

# Disparities

####################

# TITLE: Sentence Lengths for People in Prison Past Their Parole Eligibility Date
if (state_for_report %in% names(all_bubble_race_ped_release)) {
  state_bubble_race_ped_release <-
    all_bubble_race_ped_release[[state_for_report]] |>
    hc_size(height = 500)
} else {
  state_bubble_race_ped_release <- no_data_text
}
