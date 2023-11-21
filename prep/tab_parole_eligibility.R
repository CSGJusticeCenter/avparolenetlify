#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: November 14, 2023 (MAR)

# Description:
#    Parole eligibility tables and graphics for "Parole Eligibility" tab
#######################################

################################################################################

# Prison population by parole eligibility status
# Single, horizontal stacked bar chart
# Also a pie chart option

# Obtained from NCRP year end population

################################################################################

# get total prison population by state and year
ncrp_prison_population <- ncrp_yearendpop %>%
  fnc_parameters() %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  summarise(yearendpop = sum(n, na.rm = FALSE))

# get number of people in prison by parole eligibility status
# but just for people in prison for a new court commitment and sentence length 1-25 years
# merge prison population numbers to get percentages
ncrp_parole_eligible_125years_new_crime <- ncrp_yearendpop %>%
  fnc_parameters() %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  left_join(ncrp_prison_population,
            by = c("state", "rptyear")) %>%
  mutate(prop = n / yearendpop)

# reshape data for table
parole_eligibility_table <- ncrp_parole_eligible_125years_new_crime %>%
  group_by(state, rptyear, parelig_status) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    prop = sum(prop, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, rptyear, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()

# filter to select year
parole_eligibility_table_select_year <- parole_eligibility_table %>%
  filter(rptyear == select_year)

# find missing states
# Arizona, Michigan, New Jersey, New Mexico
missing_data <- tibble(state = setdiff(state.name,
                                       parole_eligibility_table_select_year$state),
                       rptyear = select_year)

# combine the missing states with the original dataframe to get all 50 states
# this final table shows parole eligibility statuses for people in prison for a
#     new crime, not a parole return/revocation.
parole_eligibility_table_select_year <-
  bind_rows(parole_eligibility_table_select_year, missing_data) %>%
  left_join(ncrp_prison_population,
            by = c("state", "rptyear")) %>%
  left_join(ncrp_prison_population_125years_new_crime,
            by = c("state", "rptyear")) %>%
  arrange(state) %>%
  select(state,
         rptyear,
         yearendpop,
         yearendpop_125years_new_crime,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count,
         current_perc,
         future_1_5_years_perc,
         future_6_years_perc,
         missing_perc)

# Create long form based on perc variables (type and prop columns)
ncrp_pe_type_prop <- parole_eligibility_table_select_year %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count,
         current_perc,
         future_1_5_years_perc,
         future_6_years_perc,
         missing_perc) %>%
  pivot_longer(cols = c(current_perc,
                        future_1_5_years_perc,
                        future_6_years_perc,
                        missing_perc),
               names_to = "type",
               values_to = "prop") %>%
  mutate(type = case_when(
    type == "current_perc"          ~ "Currently Eligible",
    type == "future_1_5_years_perc" ~ "Eligible in 1 to 5 Years",
    type == "future_6_years_perc"   ~ "Eligible in 6 or more Years",
    type == "missing_perc"          ~ "Missing Parole Eligibility Data"
  ))

# create long form based on count variables (type and n columns)
ncrp_pe_type_count <- parole_eligibility_table_select_year %>%
  filter(rptyear == select_year) %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count) %>%
  pivot_longer(cols = c(current_count,
                        future_1_5_years_count,
                        future_6_years_count,
                        missing_count),
               names_to = "type",
               values_to = "n") %>%
  mutate(type = case_when(
    type == "current_count"          ~ "Currently Eligible",
    type == "future_1_5_years_count" ~ "Eligible in 1 to 5 Years",
    type == "future_6_years_count"   ~ "Eligible in 6 or more Years",
    type == "missing_count"          ~ "Missing Parole Eligibility Data"
  ))

# join the two long forms together
ncrp_pe_type <- ncrp_pe_type_prop %>%
  left_join(ncrp_pe_type_count, by = c("state", "rptyear", "type")) %>%
   mutate(type = factor(type,
                        levels = c("Missing Parole Eligibility Data",
                                   "Eligible in 6 or more Years",
                                   "Eligible in 1 to 5 Years",
                                   "Currently Eligible"))) %>%
   mutate(tooltip =
            paste0("<b>", state, "</b><br><br>",
                   "<b>", type, "</b><br><br>",
                   "Percentage of the Prison Population: <br><b>",
                    paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
          prop_label = paste0(round(prop*100, 0), "%"))

# horizontal stacked bar chart showing prison population by parole eligibility status
states <- unique(ncrp_pe_type$state)
all_stackedbar_pe_type <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_pe_type %>%
    filter(state == x)
    hc_accessibility_text <-
      paste0("This graph shows the proportion of the prison population by parole eligibility status in ",
      select_year, " in the state of ", x, ". Parole eligibility statuses include the new court commitment popultion currently eligible,
      new court commitment population eligible in 1 to 5 years, new court commitment population eligible in 6 or more years, other population currently or
      eligible in the future, and population with missing parole eligibility data.")
    highcharts <-
      fnc_single_grouped_columnchart(df1, "prop", "type", "state", hc_accessibility_text)
  return(highcharts)
})
all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Georgia


##########
# Sentence about parole eligible prison population
##########

# get list of states
states <- unique(ncrp_pe_type$state)

# generate sentence about most serious sentenced offense in select year by state
all_sentence_parole_elgibility_population <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type %>%
    filter(state == x &
           type == "Currently Eligible")

  sentences <- paste0("In ", select_year, ", there were ", formattable::comma(df1$n, digits = 0),
                      " people who were in prison and currently eligible for parole. This group made up ",
                      df1$prop_label, " of the prison population in prison for new court commitments with
                      sentences between 1 to 24.99 years.")
  return(sentences)
})

all_sentence_parole_elgibility_population <- setNames(all_sentence_parole_elgibility_population, states)
all_sentence_parole_elgibility_population$Georgia








################################################################################

# Parole eligibility by demographics
# Highchart bar charts and sentences

# Obtained from NCRP year end population

################################################################################

##########
# Race
##########

# Currently parole eligible population but still in prison by race in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_race <- fnc_prepare_pe_data(ncrp_yearendpop, race) %>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by race
states <- unique(current_ped_race$state)
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  race in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "race", hc_accessibility_text)
  return(highcharts)
})
all_bar_parole_elgibility_race <- setNames(all_bar_parole_elgibility_race, states)
all_bar_parole_elgibility_race$Georgia

# Create sentences describing breakdown of parole-eligible prison population by race
all_sentence_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_race %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", among the prison population currently eligible for parole, ",
                      df1$race, " individuals constituted the majority, representing ", round(df1$prop*100, 0), " percent.")
  return(sentences)
})

all_sentence_parole_elgibility_race <- setNames(all_sentence_parole_elgibility_race, states)
all_sentence_parole_elgibility_race$Georgia


##########
# Age
##########

# Currently parole eligible population but still in prison by ageyrend in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_ageyrend <- fnc_prepare_pe_data(ncrp_yearendpop, ageyrend) %>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by ageyrend
states <- unique(current_ped_ageyrend$state)
all_bar_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_ageyrend %>%
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "ageyrend", hc_accessibility_text)
  return(highcharts)
})
all_bar_parole_elgibility_ageyrend <- setNames(all_bar_parole_elgibility_ageyrend, states)
all_bar_parole_elgibility_ageyrend$Georgia

# Create sentences describing breakdown of parole-eligible prison population by ageyrend
all_sentence_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_ageyrend %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", among the prison population currently eligible for parole, people between the ages of ",
                      df1$ageyrend, " constituted the majority, representing ", round(df1$prop*100, 0), " percent.")
  return(sentences)
})

all_sentence_parole_elgibility_ageyrend <- setNames(all_sentence_parole_elgibility_ageyrend, states)
all_sentence_parole_elgibility_ageyrend$Georgia


##########
# Gender
##########

# Currently parole eligible population but still in prison by gender in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_gender <- fnc_prepare_pe_data(ncrp_yearendpop, sex)%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by gender
states <- unique(current_ped_gender$state)
all_bar_parole_elgibility_gender <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_gender %>%
    filter(state == x) %>%
    arrange(desc(n))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  gender in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "sex", hc_accessibility_text) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1.2
    )
  return(highcharts)
})
all_bar_parole_elgibility_gender <- setNames(all_bar_parole_elgibility_gender, states)
all_bar_parole_elgibility_gender$Georgia

# Create sentences describing breakdown of parole-eligible prison population by gender
all_sentence_parole_elgibility_gender <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_gender %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", among the prison population currently eligible for parole, ",
                      tolower(df1$sex), "s constituted the majority, representing ", round(df1$prop*100, 0), " percent.")
  return(sentences)
})

all_sentence_parole_elgibility_gender <- setNames(all_sentence_parole_elgibility_gender, states)
all_sentence_parole_elgibility_gender$Georgia







################################################################################

# Sentence lengths for people eligible for parole but in prison in select year

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data(ncrp_yearendpop, sentlgth)%>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by sentlgth
states <- unique(current_ped_sentlgth$state)
all_bar_parole_elgibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth %>%
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their original sentence length in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "sentlgth", hc_accessibility_text)
  return(highcharts)
})
all_bar_parole_elgibility_sentlgth <- setNames(all_bar_parole_elgibility_sentlgth, states)
all_bar_parole_elgibility_sentlgth$Georgia

# Create sentences describing breakdown of parole-eligible prison population by sentlgth
states <- unique(current_ped_sentlgth$state)
all_sentence_parole_elgibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth %>%
    filter(state == x) %>%
    arrange(-prop) %>%
    slice(1)
  df1$sentlgth <- gsub("-", " to ", df1$sentlgth)
  sentences <- paste0("In ", select_year, ", among the prison population currently eligible for parole, people with sentences between ",
                      df1$sentlgth, " years constituted the majority, representing ", round(df1$prop*100, 0), " percent.")
  return(sentences)
})

all_sentence_parole_elgibility_sentlgth <- setNames(all_sentence_parole_elgibility_sentlgth, states)
all_sentence_parole_elgibility_sentlgth$Georgia











################################################################################

# Most serious sentenced offenses for people eligible for parole but in prison in select year

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by fbi_index in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_fbi_index <-
  fnc_prepare_pe_data(ncrp_yearendpop, fbi_index) %>%
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by fbi_index
states <- unique(current_ped_fbi_index$state)
all_bar_parole_elgibility_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_fbi_index %>%
    filter(state == x)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their most serious sentenced offense in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_barchart(df1, "fbi_index", hc_accessibility_text)
  return(highcharts)
})
all_bar_parole_elgibility_fbi_index <- setNames(all_bar_parole_elgibility_fbi_index, states)
all_bar_parole_elgibility_fbi_index$Georgia

# Create sentences describing breakdown of parole-eligible prison population by fbi_index
states <- unique(current_ped_fbi_index$state)
all_sentence_parole_elgibility_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_fbi_index %>%
    filter(state == x) %>%
    arrange(-prop) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", among the prison population currently eligible for parole, people with ",
                      tolower(df1$fbi_index), " offenses constituted the majority, representing ", round(df1$prop*100, 0), " percent.")
  return(sentences)
})

all_sentence_parole_elgibility_fbi_index <- setNames(all_sentence_parole_elgibility_fbi_index, states)
all_sentence_parole_elgibility_fbi_index$Georgia






################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_stackedbar_pe_type,                        file = file.path(folder, "all_stackedbar_pe_type.rds"))
  save(all_sentence_parole_elgibility_population,     file = file.path(folder, "all_sentence_parole_elgibility_population.rds"))

  save(current_ped_race,                              file = file.path(folder, "current_ped_race.rds"))
  save(all_sentence_parole_elgibility_race,           file = file.path(folder, "all_sentence_parole_elgibility_race.rds"))
  save(all_bar_parole_elgibility_race,                file = file.path(folder, "all_bar_parole_elgibility_race.rds"))

  save(all_sentence_parole_elgibility_ageyrend,       file = file.path(folder, "all_sentence_parole_elgibility_ageyrend.rds"))
  save(all_bar_parole_elgibility_ageyrend,            file = file.path(folder, "all_bar_parole_elgibility_ageyrend.rds"))

  save(all_sentence_parole_elgibility_gender,         file = file.path(folder, "all_sentence_parole_elgibility_gender.rds"))
  save(all_bar_parole_elgibility_gender,              file = file.path(folder, "all_bar_parole_elgibility_gender.rds"))

  save(all_sentence_parole_elgibility_sentlgth,       file = file.path(folder, "all_sentence_parole_elgibility_sentlgth.rds"))
  save(all_bar_parole_elgibility_sentlgth,            file = file.path(folder, "all_bar_parole_elgibility_sentlgth.rds"))

  save(all_sentence_parole_elgibility_fbi_index,      file = file.path(folder, "all_sentence_parole_elgibility_fbi_index.rds"))
  save(all_bar_parole_elgibility_fbi_index,           file = file.path(folder, "all_bar_parole_elgibility_fbi_index.rds"))

}

