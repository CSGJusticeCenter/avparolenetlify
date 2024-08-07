
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

# Releases

####################

# TITLE: Prison Releases by Year
if (state_for_report %in% names(all_line_releases_by_year)) {
  state_line_releases_by_year <-
    all_line_releases_by_year[[state_for_report]] |>
    hc_size(height = 300)
} else {
  state_line_releases_by_year <- no_data_text
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
    hc_size(height = 200)
} else {
  state_pie_release_type <- no_data_text
}

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_waffle_releases_race)) {
  state_waffle_releases_race <-
    all_waffle_releases_race[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Race and Ethnicity"))
} else {
  state_waffle_releases_race <- no_data_text
}

# TITLE: Gender
if (state_for_report %in% names(all_waffle_releases_sex)) {
  state_waffle_releases_sex <-
    all_waffle_releases_sex[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Gender"))
} else {
  state_waffle_releases_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_waffle_releases_agerlse)) {
  state_waffle_releases_agerlse <-
    all_waffle_releases_agerlse[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Age"))
} else {
  state_waffle_releases_agerlse <- no_data_text
}

# TITLE: LOS by Offense Type
if (state_for_report %in% names(all_lollipop_offense_los)) {
  state_lollipop_offense_los <-
    all_lollipop_offense_los[[state_for_report]] |>
    hc_size(height = 500)
} else {
  state_lollipop_offense_los <- no_data_text
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

# SENTENCE: "In STATE, X people are incarcerated at a rate X
#            times</b> higher than White non-Hispanic people, when accounting for
#            population sizes in the community."
if (state_for_report %in% names(all_sentence_rri)) {
  state_sentence_rri <-
    all_sentence_rri[[state_for_report]]
} else {
  state_sentence_rri <- ""
}

# TITLE: For every 100,000 Black people in the community, X are in prison
if (state_for_report %in% names(all_hc_waffle_rri_black)) {
  state_hc_waffle_rri_black <-
    all_hc_waffle_rri_black[[state_for_report]]
} else {
  state_hc_waffle_rri_black <- no_data_text
}

# TITLE: For every 100,000 White people in the community, X are in prison
if (state_for_report %in% names(all_hc_waffle_rri_white)) {
  state_hc_waffle_rri_white <-
    all_hc_waffle_rri_white[[state_for_report]]
} else {
  state_hc_waffle_rri_white <- no_data_text
}

# TITLE: For every 100,000 Hispanic people in the community, X are in prison
if (state_for_report %in% names(all_hc_waffle_rri_hispanic)) {
  state_hc_waffle_rri_hispanic <-
    all_hc_waffle_rri_hispanic[[state_for_report]]
} else {
  state_hc_waffle_rri_hispanic <- no_data_text
}

# TITLE: For every 100,000 Other race(s) people in the community, X are in prison
if (state_for_report %in% names(all_hc_waffle_rri_other)) {
  state_hc_waffle_rri_other <-
    all_hc_waffle_rri_other[[state_for_report]]
} else {
  state_hc_waffle_rri_other <- no_data_text
}


# TITLE: Average Length of Stay by Race, Ethnicity, and Offense Type
if (state_for_report %in% names(all_scatter_los_race_offense)) {
  state_scatter_los_race_offense <-
    all_scatter_los_race_offense[[state_for_report]]
} else {
  state_scatter_los_race_offense <- no_data_text
}
