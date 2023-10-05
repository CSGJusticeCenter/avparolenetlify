#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: October 2, 2023 (MAR)

# Description:
#    Parole eligibility tables and graphics for "Parole Eligibility" tab
#######################################

################################################################################

# Prison population by parole eligibility status
# Single, horizontal stacked bar chart
# Also a pie chart option

# Obtained from NCRP year end population

################################################################################

# Create long form based on perc variables (type and prop columns)
# parole_eligibility_table_select_year created in page_national_trends.R
ncrp_pe_type_prop <- parole_eligibility_table_select_year %>%
  mutate(other_count = yearendpop - (current_count +
                                     missing_count +
                                     future_1_5_years_count +
                                     future_6_years_count),
         other_perc = other_count/yearendpop) %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         other_count,
         missing_count,
         current_perc,
         future_1_5_years_perc,
         future_6_years_perc,
         other_perc,
         missing_perc) %>%
  pivot_longer(cols = c(current_perc,
                        future_1_5_years_perc,
                        future_6_years_perc,
                        other_perc,
                        missing_perc),
               names_to = "type",
               values_to = "prop") %>%
  mutate(type = case_when(
    type == "current_perc"          ~ "New Crime Population<br>Currently Eligible",
    type == "future_1_5_years_perc" ~ "New Crime Population<br>Eligible in 1-5 Years",
    type == "future_6_years_perc"   ~ "New Crime Population<br>Eligible in 6+ Years",
    type == "other_perc"            ~ "Other Population<br>Currently/Future Eligible",
    type == "missing_perc"          ~ "Missing Data"
  ))

# Create long form based on count variables (type and n columns)
ncrp_pe_type_count <- parole_eligibility_table_select_year %>%
  filter(rptyear == select_year) %>%
  mutate(other_count = yearendpop - (current_count +
                                     missing_count +
                                     future_1_5_years_count +
                                     future_6_years_count)) %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         other_count,
         missing_count) %>%
  pivot_longer(cols = c(current_count,
                        future_1_5_years_count,
                        future_6_years_count,
                        other_count,
                        missing_count),
               names_to = "type",
               values_to = "n") %>%
  mutate(type = case_when(
    type == "current_count"          ~ "New Crime Population<br>Currently Eligible",
    type == "future_1_5_years_count" ~ "New Crime Population<br>Eligible in 1-5 Years",
    type == "future_6_years_count"   ~ "New Crime Population<br>Eligible in 6+ Years",
    type == "other_count"                      ~ "Other Population<br>Currently/Future Eligible",
    type == "missing_count"                    ~ "Missing Data"
  ))

# Join the two long forms together
ncrp_pe_type <- ncrp_pe_type_prop %>%
  left_join(ncrp_pe_type_count, by = c("state", "rptyear", "type")) %>%
   mutate(type = factor(type,
                        levels = c("Missing Data",
                                   "Other Population<br>Currently/Future Eligible",
                                   "New Crime Population<br>Eligible in 6+ Years",
                                   "New Crime Population<br>Eligible in 1-5 Years",
                                   "New Crime Population<br>Currently Eligible"))) %>%
   mutate(tooltip =
            paste0("<b>", state, "</b><br><br>",
                   "<b>", type, "</b><br><br>",
                   "Percentage of the Prison Population: <br><b>",
                    paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
          prop_label = paste0(round(prop*100, 0), "%"))

# Horizontal stacked bar chart showing prison population by parole eligibility status
states <- unique(ncrp_pe_type$state)
all_stackedbar_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type %>%
    filter(state == x)

  highcharts <- hchart(df1, "bar",
                       hcaes(x = state,
                             y = prop,
                             group = type),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "bold",
                                                      fontSize = "16px",
                                                      fontFamily = "Graphik"))
  ) %>%
    hc_yAxis(labels = list(format = "{value}%",
                           enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(
      layout = "horizontal",
      align = "center",
      verticalAlign = "top",
      reversed = TRUE,
      itemMarginTop = 10,
      labelFormatter = JS("
      function() {
        var text = this.name;
        switch(text) {
          case 'Missing Data':
            return '<span style=\"font-weight: normal;\">' + text + '</span>';
          case 'New Crime Population<br>Currently Eligible':
            return '<span style=\"font-weight: normal;\">' + text + '</span>';
          case 'Other Population<br>Currently/Future Eligible':
            return '<span style=\"font-weight: normal;\">' + text + '</span>';
          case 'New Crime Population<br>Eligible in 6+ Years':
            return '<span style=\"font-weight: normal;\">' + text + '</span>';
          case 'New Crime Population<br>Eligible in 1-5 Years':
            return '<span style=\"font-weight: normal;\">' + text + '</span>';
          default:
            return '<span style=\"font-weight: bold;\">' + text + '</span>';
        }
      }
    ")
    ) %>%
    hc_plotOptions(
      series = list(stacking = "normal",
                    animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3,
                    minPointLength = 4),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = "TBD.",
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = "TBD.")))

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
             type == "New Crime Population<br>Currently Eligible")

  sentences <- paste0("In ", select_year, ", there were ", formattable::comma(df1$n, digits = 0),
                      " people were eligible for parole but still in prison for new crimes, with original sentence lengths ranging from 1 to 25 years. This group made up ",
                      df1$prop_label, " of the prison population.")
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
# Only for people in prison most recently for a new crime, sentence lengths (1-25 years)
current_ped_race <- fnc_prepare_basic_data(ncrp_yearendpop, race)

# Create highcharts showing breakdown of parole-eligible prison population by race
states <- unique(current_ped_race$state)
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  highcharts <- fnc_basic_barchart(df1, "race", "TBD accessibility text")
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
  sentences <- paste0("In ", select_year, ", ", df1$race,
                      " people made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes and with an original sentence length between 1-25 years.")
  return(sentences)
})

all_sentence_parole_elgibility_race <- setNames(all_sentence_parole_elgibility_race, states)
all_sentence_parole_elgibility_race$Georgia


##########
# Age
##########

# Currently parole eligible population but still in prison by ageyrend in select year
# Only for people in prison most recently for a new crime, sentence lengths (1-25 years)
current_ped_ageyrend <- fnc_prepare_basic_data(ncrp_yearendpop, ageyrend)

# Create highcharts showing breakdown of parole-eligible prison population by ageyrend
states <- unique(current_ped_ageyrend$state)
all_bar_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_ageyrend %>%
    filter(state == x) %>%
    arrange(desc(n))
  highcharts <- fnc_basic_barchart(df1, "ageyrend", "TBD accessibility text")
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
  sentences <- paste0("In ", select_year, ", people who were between the ages of ", df1$ageyrend,
                      " made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes and with an original sentence length between 1-25 years.")
  return(sentences)
})

all_sentence_parole_elgibility_ageyrend <- setNames(all_sentence_parole_elgibility_ageyrend, states)
all_sentence_parole_elgibility_ageyrend$Georgia


##########
# Gender
##########

# Currently parole eligible population but still in prison by gender in select year
# Only for people in prison most recently for a new crime, sentence lengths (1-25 years)
current_ped_gender <- fnc_prepare_basic_data(ncrp_yearendpop, sex)

# Create highcharts showing breakdown of parole-eligible prison population by gender
states <- unique(current_ped_gender$state)
all_bar_parole_elgibility_gender <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_gender %>%
    filter(state == x) %>%
    arrange(desc(n))
  highcharts <- fnc_basic_barchart(df1, "sex", "TBD accessibility text")
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
  sentences <- paste0("In ", select_year, ", ", tolower(df1$sex),
                      " people made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes and with an original sentence length between 1-25 years.")
  return(sentences)
})

all_sentence_parole_elgibility_gender <- setNames(all_sentence_parole_elgibility_gender, states)
all_sentence_parole_elgibility_gender$Georgia







################################################################################

# Sentence lengths for people eligible for parole but in prison in select year

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new crime, sentence lengths (1-25 years)
current_ped_sentlgth <- fnc_prepare_basic_data(ncrp_yearendpop, sentlgth)

# Create highcharts showing breakdown of parole-eligible prison population by sentlgth
states <- unique(current_ped_sentlgth$state)
all_bar_parole_elgibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth %>%
    filter(state == x)
  highcharts <- fnc_basic_barchart(df1, "sentlgth", "TBD accessibility text")
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
  sentences <- paste0("In ", select_year, ", ", "the majority of the parole-eligible prison population serving sentences for new crimes with sentence lengths ranging from 1 to 25 years were incarcerated with an original sentence length of ",
                      df1$sentlgth, ", accounting for ", df1$prop_label, " of this group.")
  return(sentences)
})

all_sentence_parole_elgibility_sentlgth <- setNames(all_sentence_parole_elgibility_sentlgth, states)
all_sentence_parole_elgibility_sentlgth$Georgia











################################################################################

# Most serious sentenced offenses for people eligible for parole but in prison in select year

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by fbi_index in select year
# Only for people in prison most recently for a new crime, sentence lengths (1-25 years)
current_ped_fbi_index <- fnc_prepare_basic_data(ncrp_yearendpop, fbi_index)

# Create highcharts showing breakdown of parole-eligible prison population by fbi_index
states <- unique(current_ped_fbi_index$state)
all_bar_parole_elgibility_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_fbi_index %>%
    filter(state == x)
  highcharts <- fnc_basic_barchart(df1, "fbi_index", "TBD accessibility text")
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
  sentences <- paste0("In ", select_year, ", ", "the majority of the parole-eligible prison population serving sentences for new crimes with sentence lengths ranging from 1 to 25 years were incarcerated for offenses related to ",
                      tolower(df1$fbi_index), ", accounting for ", df1$prop_label, " of this group.")
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

