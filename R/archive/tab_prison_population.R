#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: January 15, 2024 (MAR)
# Description:
#    Prison population and graphics for Prison Population Tab
#    All figures and tables are for select year
#######################################

################################################################################

# Section: Prison Population

# Prison population from 2010-2020
# Obtained from NCRP year end population, NCRP releases, NCRP admissions
# CHANGE TO BJS DATA ???????????????????????????????????????????????????????????

################################################################################

# Prison population by year
ncrp_yearendpop_by_year <- ncrp_yearendpop %>%
  filter(rptyear >= 2010) %>%
  group_by(state, rptyear) %>%
  summarise(total = n()) %>%
  fnc_tooltip(rptyear, formattable::comma(total, 0), "Year-End Population")

# Highchart by state since 2010
states <- unique(ncrp_yearendpop_by_year$state)
all_yearendpop_by_year <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_by_year %>%
    ungroup() %>%
    filter(state == x)

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$total)*1.1

  hc_accessibility_text <- paste0("This graph shows the year-end prison population in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_columnchart(df1, "rptyear", "Year-End Population", hc_accessibility_text) %>%
    hc_yAxis(min = 0, max = max_value)

  return(highcharts)
})
all_yearendpop_by_year <- setNames(all_yearendpop_by_year, states)
all_yearendpop_by_year$Georgia







################################################################################

# Section: Who's in Prison?

# People in prison by race/ethnicity, age range, gender

# Obtained from NCRP year end population

################################################################################

##########
# Race and Ethnicity
##########

# Get number/prop people by race and admission type
ncrp_yearendpop_race <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_labels(race) %>%
  fnc_tooltip(race, prop_label,
              paste0("Race and Ethnicity: "))

# Highchart showing prison population by admission type and race and ethnicity
states <- unique(ncrp_yearendpop_race$state)
all_stackedbar_prison_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_race %>%
    ungroup() %>%
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the number of people in prison by race and ethnicity and
                                  by admissio type in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "admtype","race", hc_accessibility_text)

  return(highcharts)
})
all_stackedbar_prison_race <- setNames(all_stackedbar_prison_race, states)
all_stackedbar_prison_race$Georgia

# Create sentences describing who is in prison by admission type and race and ethnicity
states <- ncrp_yearendpop_race %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
         any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()
all_sentence_prison_race <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_race %>%
    filter(state == x)

  max_new_court <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(race)

  max_parole_return <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(race)

  if(max_new_court == max_parole_return) {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", ", max_new_court,
                        " individuals made up the largest portion of people in prison for new court commitments, while ",
                        max_parole_return,
                        " individuals made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_race <- setNames(all_sentence_prison_race, states)
all_sentence_prison_race$Georgia




##########
# Age
##########

# Get number/prop people by age
ncrp_yearendpop_ageyrend <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_labels(ageyrend) %>%
  fnc_tooltip(ageyrend, prop_label,
              paste0("Age: "))

# Highchart showing prison population by admission type and age
states <- unique(ncrp_yearendpop_ageyrend$state)
all_stackedbar_prison_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_ageyrend %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the number of people in prison by age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "admtype","ageyrend", hc_accessibility_text)
  return(highcharts)
})
all_stackedbar_prison_ageyrend <- setNames(all_stackedbar_prison_ageyrend, states)
all_stackedbar_prison_ageyrend$Georgia

# Sentence about prison population by admission type and age
states <- ncrp_yearendpop_ageyrend  %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
         any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()
all_sentence_prison_ageyrend  <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_ageyrend  %>%
    filter(state == x)

  max_new_court_age <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(ageyrend)

  max_parole_return_age <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(ageyrend)

  if(max_new_court_age == max_parole_return_age) {
    sentences <- paste0("In ", select_year, ", individuals aged ", max_new_court_age,
                        " made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", individuals aged ", max_new_court_age,
                        " made up the largest portion of people in prison for new court commitments, while individuals aged ",
                        max_parole_return_age,
                        " made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_ageyrend <- setNames(all_sentence_prison_ageyrend, states)
all_sentence_prison_ageyrend$Georgia






##########
# Gender
##########

# Get number/prop people by gender
ncrp_yearendpop_gender <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state, admtype) %>%
  fnc_values_labels(sex) %>%
  fnc_tooltip(sex, prop_label,
              paste0("Gender: "))

# Highchart showing prison population by admission type and gender
states <- unique(ncrp_yearendpop_gender$state)
all_stackedbar_prison_gender <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_gender %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the number of people in prison by gender in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_grouped_stacked_barchart(df1, "admtype","sex", hc_accessibility_text)
  return(highcharts)
})

all_stackedbar_prison_gender <- setNames(all_stackedbar_prison_gender, states)
all_stackedbar_prison_gender$Georgia

# Create sentences describing prison population by admtype and gender
states <- ncrp_yearendpop_gender %>%
  group_by(state) %>%
  filter(any(admtype == "New court commitment") &
           any(admtype == "Parole return/revocation")) %>%
  pull(state) %>%
  unique()
all_sentence_prison_gender <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_gender %>%
    filter(state == x)

  max_new_court <- df1 %>%
    filter(admtype == "New court commitment") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(sex)

  max_parole_return <- df1 %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(desc(prop)) %>%
    slice(1) %>%
    pull(sex)

  if(max_new_court == max_parole_return) {
    sentences <- paste0("In ", select_year, ", ", tolower(max_new_court),
                        " individuals made up the largest portion of people in prison for both new court commitments and parole returns and revocations.")
  } else {
    sentences <- paste0("In ", select_year, ", ", tolower(max_new_court),
                        " individuals made up the largest portion of people in prison for new court commitments, while ",
                        tolower(max_parole_return),
                        " individuals made up the largest portion of people in prison for parole returns and revocations.")
  }

  return(sentences)
})

all_sentence_prison_gender <- setNames(all_sentence_prison_gender, states)
all_sentence_prison_gender$Georgia







################################################################################

# Section: Offense Types

# People in prison by most serious sentenced offense

# Obtained from NCRP year end population

################################################################################

# Get number/prop people by offense type
ncrp_yearendpop_fbi_index <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_labels(fbi_index) %>%
  fnc_tooltip(fbi_index, prop_label,
              paste0("Criminal Offense: ")) %>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Highchart showing the prison population by offense type
states <- unique(ncrp_yearendpop_fbi_index$state)
all_bar_prison_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_fbi_index %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the proportion people in prison population by most serious sentenced offense in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "fbi_index", hc_accessibility_text)
  return(highcharts)
})
all_bar_prison_fbi_index <- setNames(all_bar_prison_fbi_index, states)
all_bar_prison_fbi_index$Georgia








################################################################################

# Section: Sentence Length

# People in prison by sentence length

# Obtained from NCRP year end population

################################################################################

# Get number/prop people by sentence length
ncrp_yearendpop_sentlgth <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  group_by(state) %>%
  fnc_values_labels(sentlgth) %>%
  fnc_tooltip(sentlgth, prop_label,
              paste0("Sentence Length: "))%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Highchart showing prison population by sentence length
states <- unique(ncrp_yearendpop_sentlgth$state)
all_bar_prison_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_sentlgth %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  hc_accessibility_text <- paste0("This graph shows the proportion people in prison population by original sentence length in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "sentlgth", hc_accessibility_text)
  return(highcharts)
})
all_bar_prison_sentlgth <- setNames(all_bar_prison_sentlgth, states)
all_bar_prison_sentlgth$Georgia









################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_yearendpop_by_year,          file = file.path(folder, "all_yearendpop_by_year.rds"))
  save(all_releases_by_year,            file = file.path(folder, "all_releases_by_year.rds"))

  save(all_stackedbar_admtype,          file = file.path(folder, "all_stackedbar_admtype.rds"))
  save(all_sentence_admtype,            file = file.path(folder, "all_sentence_admtype.rds"))

  save(all_stackedbar_prison_race,      file = file.path(folder, "all_stackedbar_prison_race.rds"))
  save(all_stackedbar_prison_gender,    file = file.path(folder, "all_stackedbar_prison_gender.rds"))
  save(all_stackedbar_prison_ageyrend,  file = file.path(folder, "all_stackedbar_prison_ageyrend.rds"))

  save(all_sentence_prison_race,        file = file.path(folder, "all_sentence_prison_race.rds"))
  save(all_sentence_prison_ageyrend,    file = file.path(folder, "all_sentence_prison_ageyrend.rds"))
  save(all_sentence_prison_gender,      file = file.path(folder, "all_sentence_prison_gender.rds"))

  # TO DO ONCE TEAM DECIDES ON CHART OPTIONS
  # save(all_sentence_prison_sentlgth,    file = file.path(folder, "all_sentence_prison_sentlgth.rds"))
  # save(all_sentence_prison_fbi_index,   file = file.path(folder, "all_sentence_prison_fbi_index.rds"))

  save(all_bar_prison_sentlgth,         file = file.path(folder, "all_bar_prison_sentlgth.rds"))
  save(all_bar_prison_fbi_index,        file = file.path(folder, "all_bar_prison_fbi_index.rds"))

}

