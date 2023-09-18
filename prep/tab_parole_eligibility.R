#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: September 18, 2023 (MAR)

# Description:
#    Parole eligibility tables and graphics for "Parole Eligibility" tab
#######################################


################################################################################

# Highchart - horizontal stacked bar chart
# Prison population by parole eligibility status

# Obtained from NCRP year end population

################################################################################

# Create long form based on perc variables (type and prop columns)
ncrp_pe_type_2020_prop <- parole_eligibility_table %>%
  filter(rptyear == 2020) %>%
  mutate(other_perc = current_other_perc + future_1_5_years_other_perc + future_6_years_other_perc,
         other_count = current_other_count + future_1_5_years_other_count + future_6_years_other_count) %>%
  select(state,
         rptyear,
         current_new_crime_count,
         future_1_5_years_new_crime_count,
         future_6_years_new_crime_count,
         other_count,
         missing_count,
         current_new_crime_perc,
         future_1_5_years_new_crime_perc,
         future_6_years_new_crime_perc,
         other_perc,
         missing_perc) %>%
  pivot_longer(cols = c(current_new_crime_perc,
                        future_1_5_years_new_crime_perc,
                        future_6_years_new_crime_perc,
                        other_perc,
                        missing_perc),
               names_to = "type",
               values_to = "prop") %>%
  mutate(type = case_when(
    type == "current_new_crime_perc"          ~ "New Crime Population<br>Currently Eligible",
    type == "future_1_5_years_new_crime_perc" ~ "New Crime Population<br>Eligible in 1-5 Years",
    type == "future_6_years_new_crime_perc"   ~ "New Crime Population<br>Eligible in 6+ Years",
    type == "other_perc"                      ~ "Other Population<br>Currently/Future Eligible",
    type == "missing_perc"                    ~ "Missing Data"
  ))

# Create long form based on count variables (type and n columns)
ncrp_pe_type_2020_count <- parole_eligibility_table %>%
  filter(rptyear == 2020) %>%
  mutate(other_count = current_other_count + future_1_5_years_other_count + future_6_years_other_count) %>%
  select(state,
         rptyear,
         current_new_crime_count,
         future_1_5_years_new_crime_count,
         future_6_years_new_crime_count,
         other_count,
         missing_count) %>%
  pivot_longer(cols = c(current_new_crime_count,
                        future_1_5_years_new_crime_count,
                        future_6_years_new_crime_count,
                        other_count,
                        missing_count),
               names_to = "type",
               values_to = "n") %>%
  mutate(type = case_when(
    type == "current_new_crime_count"          ~ "New Crime Population<br>Currently Eligible",
    type == "future_1_5_years_new_crime_count" ~ "New Crime Population<br>Eligible in 1-5 Years",
    type == "future_6_years_new_crime_count"   ~ "New Crime Population<br>Eligible in 6+ Years",
    type == "other_count"                      ~ "Other Population<br>Currently/Future Eligible",
    type == "missing_count"                    ~ "Missing Data"
  ))

# Join the two long forms together
ncrp_pe_type_2020 <- ncrp_pe_type_2020_prop %>%
  left_join(ncrp_pe_type_2020_count, by = c("state", "rptyear", "type")) %>%
   mutate(type = factor(type,
                        levels = c("Missing Data",
                                   "Other Population<br>Currently/Future Eligible",
                                   "New Crime Population<br>Eligible in 6+ Years",
                                   "New Crime Population<br>Eligible in 1-5 Years",
                                   "New Crime Population<br>Currently Eligible"))) %>%
   mutate(tooltip =
            paste0("<b>", state, "</b><br><br>",
                   "<b>", type, "</b><br><br>",
                   # "Number of People: <br><b>",
                   #  formattable::comma(n, digits = 0), "</b><br><br>",
                   "Percentage of the Prison Population: <br><b>",
                    paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
          prop_label = paste0(round(prop*100, 0), "%"))

# get list of states
states <- unique(ncrp_pe_type_2020$state)

all_stackedbar_pe_type_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
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
    hc_colors(c("gray", orange, yellow, purple, teal)) %>%
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

all_stackedbar_pe_type_2020 <- setNames(all_stackedbar_pe_type_2020, states)








################################################################################

# Sentence about parole eligible prison population:

# In 2020, there were ___ people who were eligible for parole but remained incarcerated for a new crime.
# This group made up ___% of the prison population.

# Obtained from NCRP year end population

################################################################################

# get list of states
states <- unique(ncrp_pe_type_2020$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_population <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
    filter(state == x &
             type == "New Crime Population<br>Currently Eligible")

  sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
                      " people who were eligible for parole but remained incarcerated for a new crime. This group made up ",
                      df1$prop_label, " of the prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_population <- setNames(all_sentence_parole_elgibility_population, states)
all_sentence_parole_elgibility_population$Georgia








################################################################################

# Highchart - bar chart
# Sentence about it
# Parole eligibility by race

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by race in 2020
# Only in for people in prison most recently for a new crime
current_ped_2020_race <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" &
         rptyear == 2020 &
         parelig_status == "Current") %>%
  group_by(state) %>%
  count(race) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          race, "</b><br>",
                          prop_label, "<br>"))

# get states with data
states <- unique(current_ped_2020_race$state)

# generate bar chart showing parole eligible populations by race and state in 2020
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  xaxis_order <- df1$race

  # assign color for each race
  df1$color <- case_when(df1$race == "Black, non-Hispanic" ~ yellow,
                         df1$race == "White, non-Hispanic" ~ orange,
                         df1$race == "Hispanic, any race" ~ teal,
                         df1$race == "Other race(s), non-Hispanic" ~ purple,
                         df1$race == "Unknown" ~ darkblue)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(race), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD")))

  return(highcharts)
})

all_bar_parole_elgibility_race <- setNames(all_bar_parole_elgibility_race, states)
all_bar_parole_elgibility_race$Georgia

# generate sentence about parole eligible populations by race and state in 2020
all_sentence_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, ", df1$race,
                      " people made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes.")
  return(sentences)
})

all_sentence_parole_elgibility_race <- setNames(all_sentence_parole_elgibility_race, states)
all_sentence_parole_elgibility_race$Georgia

# Currently parole eligible population but still in prison by ageyrend in 2020
# Only in for people in prison most recently for a new crime
current_ped_2020_ageyrend <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" &
           rptyear == 2020 &
           parelig_status == "Current") %>%
  group_by(state) %>%
  count(ageyrend) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          ageyrend, "</b><br>",
                          prop_label, "<br>"))

# get states with data
states <- unique(current_ped_2020_ageyrend$state)

# generate bar chart showing parole eligible populations by ageyrend and state in 2020
all_bar_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_2020_ageyrend %>%
    filter(state == x)
  xaxis_order <- (df1$ageyrend)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(ageyrend), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD")))

  return(highcharts)
})

all_bar_parole_elgibility_ageyrend <- setNames(all_bar_parole_elgibility_ageyrend, states)
all_bar_parole_elgibility_ageyrend$Georgia

# generate sentence about parole eligible populations by ageyrend and state in 2020
all_sentence_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_ageyrend %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, people who were between the ages of ", df1$ageyrend,
                      " old made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes.")
  return(sentences)
})

all_sentence_parole_elgibility_ageyrend <- setNames(all_sentence_parole_elgibility_ageyrend, states)
all_sentence_parole_elgibility_ageyrend$Georgia

# Currently parole eligible population but still in prison by gender in 2020
# Only in for people in prison most recently for a new crime
current_ped_2020_gender <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" &
           rptyear == 2020 &
           parelig_status == "Current") %>%
  group_by(state) %>%
  count(sex) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          sex, "</b><br>",
                          prop_label, "<br>"))

# get states with data
states <- unique(current_ped_2020_gender$state)

# generate bar chart showing parole eligible populations by gender and state in 2020
all_bar_parole_elgibility_gender <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_2020_gender %>%
    filter(state == x)
  xaxis_order <- (df1$sex)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(sex), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD")))

  return(highcharts)
})

all_bar_parole_elgibility_gender <- setNames(all_bar_parole_elgibility_gender, states)
all_bar_parole_elgibility_gender$Georgia

# generate sentence about parole eligible populations by gender and state in 2020
all_sentence_parole_elgibility_gender <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_gender %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, ", tolower(df1$sex),
                      " people made up the largest proportion of those eligible for parole yet still incarcerated for a new criminal offense, comprising ",
                      df1$prop_label, " of the parole-eligible population serving time for new crimes.")
  return(sentences)
})

all_sentence_parole_elgibility_gender <- setNames(all_sentence_parole_elgibility_gender, states)
all_sentence_parole_elgibility_gender$Georgia





################################################################################

# Most serious sentenced offenses for those in prison but not released in 2020

# Obtained from NCRP year end population

################################################################################

# Most serious sentenced offense for people eligible for parole but still in prison
# Year 2020
current_ped_2020_fbi_index_all <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(parelig_status == "Current")

# Count most serious sentenced offense for people in prison for new crime
current_ped_2020_fbi_index_new_crime <- current_ped_2020_fbi_index_all %>%
  filter(admtype == "New court commitment") %>%
  group_by(state) %>%
  count(fbi_index) %>%
  mutate(
    prop = n/sum(n)
    , yearendpop_ped = sum(n)
  ) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Most Serious Criminal Offense: <b>", fbi_index, "</b><br><br>",
                  "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         chart_label = paste0(fbi_index, " <b>", round(prop*100, 0), "%</b>"),
         prop_label = paste0(round(prop*100, 0), "%"))






################################################################################

# Sentence about most serious offense

# Obtained from NCRP year end population

################################################################################

# get list of states with data
states <- unique(current_ped_2020_fbi_index_new_crime$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_fbi_index_new_crime %>%
    filter(state == x) %>%
    group_by() %>%
    summarise(n = sum(n, na.rm = TRUE),
              prop = sum(prop, na.rm = TRUE)) %>%
    mutate(prop = round(prop*100, 0),
           prop_label = paste0(prop, "%"))
  sentences <- "TBD sentence"
  # sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
  #                     " people who were parole eligible but still in prison for non-violent offenses, accounting for ",
  #                     df1$prop_label, " of the parole-eligible prison population in prison for new crimes.")
  return(sentences)
})

all_sentence_parole_elgibility_fbi_index <- setNames(all_sentence_parole_elgibility_fbi_index, states)
all_sentence_parole_elgibility_fbi_index$Georgia







################################################################################

# Pie chart about most serious offense

# Obtained from NCRP year end population

################################################################################

# get list of states
states <- unique(current_ped_2020_fbi_index_new_crime$state)

# generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
all_pie_parole_elgibility_fbi_index_new_crime <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_fbi_index_new_crime %>% filter(state == x)
  highcharts <-
    df1 %>%
    hchart("pie",
           hcaes(x = fbi_index, y = prop),
           dataLabels = list(
             style = list(fontSize = "1em",
                          fontWeight = "regular",
                          alignTo = "connectors",
                          color = neutralBlackText),
             enabled = TRUE,
             format = paste("{point.fbi_index}: ", "<b>{point.prop_label}</b>")
           )) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(100, 0, 18, 0)
    ) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(animation = FALSE,
                    cursor = "pointer",
                    borderWidth = 3),
      accessibility = list(enabled = TRUE,
                           keyboardNavigation = list(enabled = TRUE),
                           linkedDescription = "TBD",
                           landmarkVerbosity = "one"),
      area = list(accessibility = list(description = "TBD")))
  return(highcharts)
})

all_pie_parole_elgibility_fbi_index_new_crime <- setNames(all_pie_parole_elgibility_fbi_index_new_crime, states)
all_pie_parole_elgibility_fbi_index_new_crime$Georgia

# generate bar chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
all_bar_parole_elgibility_fbi_index_new_crime <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_fbi_index_new_crime %>% filter(state == x)
  highcharts <-
    df1 %>%
    hchart("bar",
           hcaes(x = fbi_index, y = prop),
           dataLabels = list(enabled = TRUE,
                             format = "{point.prop_label}",
                             style = list(fontWeight = "regular",
                                          fontSize = "12px",
                                          fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = "")) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(c(teal, yellow, purple)) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = "TBD.", landmarkVerbosity = "one"),
      area = list(accessibility = list(description = "TBD.")))
  return(highcharts)
})

all_bar_parole_elgibility_fbi_index_new_crime <- setNames(all_bar_parole_elgibility_fbi_index_new_crime, states)
all_bar_parole_elgibility_fbi_index_new_crime$Georgia







################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_stackedbar_pe_type_2020,                   file = file.path(folder, "all_stackedbar_pe_type_2020.rds"))
  save(all_sentence_parole_elgibility_population,     file = file.path(folder, "all_sentence_parole_elgibility_population.rds"))

  save(current_ped_2020_race,                         file = file.path(folder, "current_ped_2020_race.rds"))
  save(all_sentence_parole_elgibility_race,           file = file.path(folder, "all_sentence_parole_elgibility_race.rds"))
  save(all_bar_parole_elgibility_race,                file = file.path(folder, "all_bar_parole_elgibility_race.rds"))

  save(all_sentence_parole_elgibility_ageyrend,       file = file.path(folder, "all_sentence_parole_elgibility_ageyrend.rds"))
  save(all_bar_parole_elgibility_ageyrend,            file = file.path(folder, "all_bar_parole_elgibility_ageyrend.rds"))

  save(all_sentence_parole_elgibility_gender,         file = file.path(folder, "all_sentence_parole_elgibility_gender.rds"))
  save(all_bar_parole_elgibility_gender,              file = file.path(folder, "all_bar_parole_elgibility_gender.rds"))

  save(all_sentence_parole_elgibility_fbi_index,      file = file.path(folder, "all_sentence_parole_elgibility_fbi_index.rds"))
  save(all_pie_parole_elgibility_fbi_index_new_crime, file = file.path(folder, "all_pie_parole_elgibility_fbi_index_new_crime.rds"))
  save(all_bar_parole_elgibility_fbi_index_new_crime, file = file.path(folder, "all_bar_parole_elgibility_fbi_index_new_crime.rds"))

}
