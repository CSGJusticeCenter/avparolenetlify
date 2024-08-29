
select_year <- 2020

# no data text
no_data_text <- paste0("Data is not available. ", state_for_report,
                       " did not submit this data to the National Corrections Reporting Program in ",
                       select_year, ".")
no_data_text <- ""

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
    hc_size(height = 170)
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

# SENTENCE: The demographics of people in prison past their parole eligibility
#           year reveal notable proportions among Black, non-Hispanic (56%)
#           and white, non-hispanic (40%) people. Gender distribution
#           indicates a predominance of males (93%) over females (7%).
#           Age-wise, the majority of people were 25-34 years (39%) and
#           35-44 years (26%) old."
if (state_for_report %in% names(all_sentence_parole_eligibility_demographics)) {
  state_sentence_parole_eligibility_demographics <-
    all_sentence_parole_eligibility_demographics[[state_for_report]]
} else {
  state_sentence_parole_eligibility_demographics <- ""
}

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_stacked_bar_pe_race)) {
  state_stacked_bar_pe_race <-
    all_stacked_bar_pe_race[[state_for_report]]|>
    hc_size(height = 350)
} else {
  state_stacked_bar_pe_race <- no_data_text
}

# TITLE: Gender
if (state_for_report %in% names(all_stacked_bar_pe_sex)) {
  state_stacked_bar_pe_sex <-
    all_stacked_bar_pe_sex[[state_for_report]]|>
    hc_size(height = 200)
} else {
  state_stacked_bar_pe_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_stacked_bar_pe_ageyrend)) {
  state_stacked_bar_pe_ageyrend <-
    all_stacked_bar_pe_ageyrend[[state_for_report]]|>
    hc_size(height = 350)
} else {
  state_stacked_bar_pe_ageyrend <- no_data_text
}

# TITLE: Years Spent in Prison After Parole Eligibility
if (state_for_report %in% names(all_scatter_race_ped_release)) {
  state_scatter_race_ped_release <-
    all_scatter_race_ped_release[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_scatter_race_ped_release <- no_data_text
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

# TITLE: Violent vs Non-Violent
if (state_for_report %in% names(all_bubble_ped_offense_group)) {
  state_bubble_ped_offense_group <-
    all_bubble_ped_offense_group[[state_for_report]]
} else {
  state_bubble_ped_offense_group <- no_data_text
}

# # TITLE: Offense Breakdown for People in Prison Past Their Parole Consideration Date
# if (state_for_report %in% names(all_bubble_ped_fbi_index)) {
#   state_bubble_ped_fbi_index <-
#     all_bubble_ped_fbi_index[[state_for_report]] |>
#     hc_size(height = 250)
# } else {
#   state_bubble_ped_fbi_index <- no_data_text
# }

# TITLE: OPTION 2 = Offense Breakdown for People in Prison Past Their Parole Consideration Date
if (state_for_report %in% names(all_bar_ped_fbi_index)) {
  state_bar_ped_fbi_index <-
    all_bar_ped_fbi_index[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_bar_ped_fbi_index <- no_data_text
}

# TITLE: Sentence Lengths for People in Prison Past Their Parole Consideration Year
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

# Population

####################

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

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_waffle_population_race)) {
  state_waffle_population_race <-
    all_waffle_population_race[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Race and Ethnicity"))|>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_population_race <- no_data_text
}

# TITLE: Gender
if (state_for_report %in% names(all_waffle_population_sex)) {
  state_waffle_population_sex <-
    all_waffle_population_sex[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Gender"))|>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_population_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_waffle_population_ageyrend)) {
  state_waffle_population_ageyrend <-
    all_waffle_population_ageyrend[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Age")) |>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_population_ageyrend <- no_data_text
}

# TITLE: Offenses for Prison Population
if (state_for_report %in% names(all_bar_population_fbi_index)) {
  state_bar_population_fbi_index <-
    all_bar_population_fbi_index[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_bar_population_fbi_index <- no_data_text
}

# TITLE: Sentence Lengths for Prison Population
if (state_for_report %in% names(all_bar_population_sentlgth)) {
  state_bar_population_sentlgth <-
    all_bar_population_sentlgth[[state_for_report]] |>
    hc_size(height = 400)
} else {
  state_bar_population_sentlgth <- no_data_text
}

# SENTENCE: The demographics of people in prison reveal notable proportions
#           among Black, non-Hispanic and White, non-Hispanic people. By gender,
#           there were more males than females. Age-wise, the majority of people
#           were 25-34 years and 35-44 years old.
if (state_for_report %in% names(all_sentence_population_demographics)) {
  state_sentence_population_demographics <-
    all_sentence_population_demographics[[state_for_report]]
} else {
  state_sentence_population_demographics <- ""
}

# SENTENCE: In 2020, 69% of people in prison were incarcerated for violent offenses.
#           The breakdown of criminal offenses reveals a more varied landscape,
#           with the majority of people incarcerated for murder and non-negligent
#           manslaughter (17%) and rape or sexual assault (15%) offenses.
if (state_for_report %in% names(all_sentence_population_fbi_index)) {
  state_sentence_population_fbi_index <-
    all_sentence_population_fbi_index[[state_for_report]]
} else {
  state_sentence_population_fbi_index <- ""
}

# SENTENCE: In 2020, the majority of people in prison had original sentence
#           lengths between 10 to 24.9 years representing 41%.
if (state_for_report %in% names(all_sentence_population_sentlgth)) {
  state_sentence_population_sentlgth <-
    all_sentence_population_sentlgth[[state_for_report]]
} else {
  state_sentence_population_sentlgth <- ""
}












####################

# Releases

####################

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


# SENTENCE: The demographics of people released from prison reveal
#           notable proportions among Black, non-Hispanic and white,
#           non-hispanic people. Gender distribution indicates a
#           predominance of males over females. Age-wise, the majority
#           of people were 25-34 years and 35-44 years old. These findings
#           provide insights into the populations transitioning back into the community
if (state_for_report %in% names(all_sentence_releases_demographics)) {
  state_sentence_releases_demographics <-
    all_sentence_releases_demographics[[state_for_report]]
} else {
  state_sentence_releases_demographics <- ""
}

# TITLE: Race and Ethnicity
if (state_for_report %in% names(all_waffle_releases_race)) {
  state_waffle_releases_race <-
    all_waffle_releases_race[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Race and Ethnicity"))|>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_releases_race <- no_data_text
}

# TITLE: Gender
if (state_for_report %in% names(all_waffle_releases_sex)) {
  state_waffle_releases_sex <-
    all_waffle_releases_sex[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Gender"))|>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_releases_sex <- no_data_text
}

# TITLE: Age
if (state_for_report %in% names(all_waffle_releases_agerlse)) {
  state_waffle_releases_agerlse <-
    all_waffle_releases_agerlse[[state_for_report]]|>
    hc_size(height = 350)|>
    hc_title(text = paste0("Age"))|>
    hc_exporting(enabled = FALSE)
} else {
  state_waffle_releases_agerlse <- no_data_text
}

# SENTENCE: Between 2010 and 2020, shifts in average time served by individuals
#           for different offense types have been observed in Georgia.
#           The largest change was for Robbery offenses, which increased by 24%."
if (state_for_report %in% names(all_sentence_los_offense)) {
  state_sentence_los_offense <-
    all_sentence_los_offense[[state_for_report]]
} else {
  state_sentence_los_offense <- ""
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

# TITLE:
if (state_for_report %in% names(all_bubble_race_ped_release)) {
  state_bubble_race_ped_release <-
    all_bubble_race_ped_release[[state_for_report]] |>
    hc_size(height = 650)|>
    hc_title(text = "How Soon People Are Released After Their Parole Consideration Year")
} else {
  state_bubble_race_ped_release <- no_data_text
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
