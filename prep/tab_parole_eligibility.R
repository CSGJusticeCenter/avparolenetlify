#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: September 26, 2023 (MAR)

# Description:
#    Parole eligibility tables and graphics for "Parole Eligibility" tab
#######################################

################################################################################

# Prison population by parole eligibility status
# Stacked bar chart
# Pie chart option

# Obtained from NCRP year end population

################################################################################

# Create long form based on perc variables (type and prop columns)
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

# get list of states
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

# Parole eligibility by demographic
# Highchart bar charts and sentences

# Obtained from NCRP year end population

################################################################################

##########
# Race
##########

# Currently parole eligible population but still in prison by race in select year
# Only in for people in prison most recently for a new crime
current_ped_race <- ncrp_yearendpop %>%
  filter(rptyear == select_year &
         parelig_status == "Current") %>%
  filter(admtype == "New court commitment") %>%
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") %>%
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
states <- unique(current_ped_race$state)

# generate bar chart showing parole eligible populations by race and state in select year
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  xaxis_order <- df1$race

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "bar",
                  hcaes(x = factor(race), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# generate sentence about parole eligible populations by race and state in select year
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
# Only in for people in prison most recently for a new crime
current_ped_ageyrend <- ncrp_yearendpop %>%
  filter(rptyear == select_year &
         parelig_status == "Current") %>%
  filter(admtype == "New court commitment") %>%
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") %>%
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
states <- unique(current_ped_ageyrend$state)

# generate bar chart showing parole eligible populations by ageyrend and state in select year
all_bar_parole_elgibility_ageyrend <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_ageyrend %>%
    filter(state == x)
  xaxis_order <- (df1$ageyrend)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "bar",
                  hcaes(x = factor(ageyrend), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# generate sentence about parole eligible populations by ageyrend and state in select year
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
# Only in for people in prison most recently for a new crime
current_ped_gender <- ncrp_yearendpop %>%
  filter(rptyear == select_year &
         parelig_status == "Current") %>%
  filter(admtype == "New court commitment") %>%
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") %>%
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
states <- unique(current_ped_gender$state)

# generate bar chart showing parole eligible populations by gender and state in select year
all_bar_parole_elgibility_gender <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_gender %>%
    filter(state == x)
  xaxis_order <- (df1$sex)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "bar",
                  hcaes(x = factor(sex), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

# generate sentence about parole eligible populations by gender and state in select year
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

# Currently parole eligible population but still in prison by sentence lenth in select year
# Only in for people in prison most recently for a new crime
current_ped_sentlgth_new_crime <- ncrp_yearendpop %>%
  filter(rptyear == select_year &
         parelig_status == "Current") %>%
  filter(admtype == "New court commitment") %>%
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") %>%
  group_by(state) %>%
  count(sentlgth) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          sentlgth, "</b><br>",
                          prop_label, "<br>"))

##########
# Sentence about sentence lengths
##########

# get list of states with data
states <- unique(current_ped_sentlgth_new_crime$state)

# generate sentence about most serious sentenced offense in select year by state
all_sentence_parole_elgibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth_new_crime %>%
    filter(state == x) %>%
    arrange(-prop) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", ", "the majority of the parole-eligible prison population serving sentences for new crimes with sentence lengths ranging from 1 to 25 years were incarcerated with an original sentence length of ",
                      df1$sentlgth, ", accounting for ", df1$prop_label, " of this group.")
  return(sentences)
})

all_sentence_parole_elgibility_sentlgth <- setNames(all_sentence_parole_elgibility_sentlgth, states)
all_sentence_parole_elgibility_sentlgth$Georgia


##########
# Bar chart about sentence lengths
##########

# get states with data
states <- unique(current_ped_sentlgth_new_crime$state)

# generate bar chart showing parole eligible populations by gender and state in select year
all_bar_parole_elgibility_sentlgth <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_sentlgth_new_crime %>%
    filter(state == x)
  xaxis_order <- (df1$sentlgth)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "bar",
                  hcaes(x = factor(sentlgth), y = prop),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
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

all_bar_parole_elgibility_sentlgth <- setNames(all_bar_parole_elgibility_sentlgth, states)
all_bar_parole_elgibility_sentlgth$Georgia











################################################################################

# Most serious sentenced offenses for people eligible for parole but in prison in select year

# Obtained from NCRP year end population

################################################################################

# Most serious sentenced offense for people eligible for parole but still in prison
# Year 2020
current_ped_fbi_index_all <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>%
  filter(parelig_status == "Current")

# Count most serious sentenced offense for people in prison for new crime
current_ped_fbi_index_new_crime <- current_ped_fbi_index_all %>%
  filter(admtype == "New court commitment") %>%
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") %>%
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

##########
# Sentence about most serious offense
##########

# get list of states with data
states <- unique(current_ped_fbi_index_new_crime$state)

# generate sentence about most serious sentenced offense in select year by state
all_sentence_parole_elgibility_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_fbi_index_new_crime %>%
    filter(state == x) %>%
    arrange(-prop) %>%
    slice(1)
  sentences <- paste0("In ", select_year, ", ", "the majority of the parole-eligible prison population serving sentences for new crimes with sentence lengths ranging from 1 to 25 years were incarcerated for offenses related to ",
                      tolower(df1$fbi_index), ", accounting for ", df1$prop_label, " of this group.")
  return(sentences)
})

all_sentence_parole_elgibility_fbi_index <- setNames(all_sentence_parole_elgibility_fbi_index, states)
all_sentence_parole_elgibility_fbi_index$Georgia

##########
# Bar chart
##########

# get list of states
states <- unique(current_ped_fbi_index_new_crime$state)

# generate bar chart about most serious sentenced offense in select year by state - NEW CRIME ONLY
all_bar_parole_elgibility_fbi_index_new_crime <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_fbi_index_new_crime %>% filter(state == x)
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
             title = list(text = ""),
             min = 0, max = 1) %>%
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

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_stackedbar_pe_type,                    file = file.path(folder, "all_stackedbar_pe_type.rds"))
  save(all_sentence_parole_elgibility_population, file = file.path(folder, "all_sentence_parole_elgibility_population.rds"))

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
  save(all_bar_parole_elgibility_fbi_index_new_crime, file = file.path(folder, "all_bar_parole_elgibility_fbi_index_new_crime.rds"))

}

