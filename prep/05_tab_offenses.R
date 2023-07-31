#######################################
# Project: AV Parole
# File: tab_los.R
# Authors: Mari Roberts
# Date last updated: July 17, 2023 (MAR)
# Description:
#    LOS tables and graphics for app
#######################################

################################################################################

# Most serious sentenced offenses for those in prison but not released in 2020

################################################################################

# most serious sentenced offense for people eligible for parole but still in prison
current_ped_2020_offenses_all <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(offgeneral)) %>%
  mutate(offgeneral = ifelse(
    offgeneral == "Other/unspecified", "Other or Unspecified", offgeneral))

# group by state
current_ped_2020_offenses <- current_ped_2020_offenses_all %>%
  group_by(state) %>%
  count(offgeneral) %>%
  mutate(
    prop = n/sum(n)
    , yearendpop_ped = sum(n)
  ) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
                  "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
                  scales::comma(n), "</b><br><br>",
                  "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
         prop_label = paste0(round(prop*100, 0), "%"))

# group by state for admission types that are parole revocations or returns
current_ped_2020_offenses_parole_return <- current_ped_2020_offenses_all %>%
  filter(admtype == "Parole return/revocation") %>%
  group_by(state) %>%
  count(offgeneral) %>%
  mutate(
    prop = n/sum(n)
    , yearendpop_ped = sum(n)
  ) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
                  "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
                  scales::comma(n), "</b><br><br>",
                  "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
         prop_label = paste0(round(prop*100, 0), "%"))

# group by state for admission types that are new crimes
current_ped_2020_offenses_new_crime <- current_ped_2020_offenses_all %>%
  filter(admtype == "New court commitment") %>%
  group_by(state) %>%
  count(offgeneral) %>%
  mutate(
    prop = n/sum(n)
    , yearendpop_ped = sum(n)
  ) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
                  "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
                  scales::comma(n), "</b><br><br>",
                  "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
         prop_label = paste0(round(prop*100, 0), "%"))






################################################################################

# Sentence about most serious offense

################################################################################

# get list of states
states <- unique(current_ped_2020_offenses$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>%
    filter(state == x) %>%
    filter(offgeneral != "Violent") %>%
    group_by() %>%
    summarise(n = sum(n, na.rm = TRUE),
              prop = sum(prop, na.rm = TRUE)) %>%
    mutate(prop = round(prop*100, 0),
           prop_label = paste0(prop, "%"))

  sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
                      " people who were parole eligible but still in prison for non-violent offenses, accounting for ",
                      df1$prop_label, " of the parole-eligible prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_offense <- setNames(all_sentence_parole_elgibility_offense, states)






################################################################################

# Pie chart about most serious offense

################################################################################

# get list of states
states <- unique(current_ped_2020_offenses$state)

# generate pie chart about most serious sentenced offense in 2020 by state
all_pie_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "offgeneral",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_parole_elgibility_offense <- setNames(all_pie_parole_elgibility_offense, states)

# get list of states
states <- unique(current_ped_2020_offenses_new_crime$state)

# generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
all_pie_parole_elgibility_offense_new_crime <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses_new_crime %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "offgeneral",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_parole_elgibility_offense_new_crime <- setNames(all_pie_parole_elgibility_offense_new_crime, states)

# get list of states
states <- unique(current_ped_2020_offenses_parole_return$state)

# generate pie chart about most serious sentenced offense in 2020 by state - PAROLE REVOCATION ONLY
all_pie_parole_elgibility_offense_parole_return <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses_parole_return %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "offgeneral",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_parole_elgibility_offense_parole_return <- setNames(all_pie_parole_elgibility_offense_parole_return, states)

####################
# Bar chart about most serious offense
# Same as above but in bar chart form
####################

# get list of states
states <- unique(current_ped_2020_offenses$state)

# generate bar chart about most serious sentenced offense in 2020 by state
all_bar_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>%
    filter(state == x) %>%
    arrange(desc(prop))
  xaxis_order <- df1$offgeneral
  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(offgeneral), y = prop*100, color = offgeneral),
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontSize = "14px",
                                                 fontWeight = "bold",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
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

all_bar_parole_elgibility_offense <- setNames(all_bar_parole_elgibility_offense, states)







################################################################################

# Data cleaning for:
# Proportion of time served by offense type, year, and state

################################################################################

# assign x axis order
desired_order <- c("Less than Sentence Length Served",
                   "Full Sentence Length Served"
                   #"More than Sentence Length Served"
                   )

ncrp_sentlgth_timesrvd_rel <- ncrp_sentlgth_timesrvd_rel %>%
filter(timesrvd_rel_vs_sentlgth!= "More than Sentence Length Served") # remove bc likely a data error

########
# Overview
########

# count and get prop of people by state, adm type, and report year
ncrp_proportion_served_2020 <- ncrp_sentlgth_timesrvd_rel %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  filter(!is.na(timesrvd_rel_vs_sentlgth)) %>%
  filter(!is.na(offgeneral)) %>%
  filter(rptyear == 2020) %>%
  group_by(state, admtype, rptyear) %>%
  count(timesrvd_rel_vs_sentlgth) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Sentence Duration: <b>",
                  timesrvd_rel_vs_sentlgth,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# get list of states
states <- ncrp_proportion_served_2020 %>%
  pull(state) %>%
  unique()

# create grouped bar chart showing proportion of sentence served by adm type
all_bar_sentence_overview_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_proportion_served_2020 %>%
    filter(state == x) %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_overview_2020 <- setNames(all_bar_sentence_overview_2020, states)


########
# Violent
########

# remove NA's and "other" releases which includes transfers and deaths
# count and get prop of people by state, offense type, adm type, and report year
ncrp_offense_proportion_served_2020 <- ncrp_sentlgth_timesrvd_rel %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  filter(!is.na(timesrvd_rel_vs_sentlgth)) %>%
  filter(!is.na(offgeneral)) %>%
  filter(rptyear == 2020) %>%
  group_by(state, offgeneral,admtype, rptyear) %>%
  count(timesrvd_rel_vs_sentlgth) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Sentence Duration: <b>",
                  timesrvd_rel_vs_sentlgth,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Violent") %>%
  pull(state) %>%
  unique()

# create grouped bar chart showing proportion of sentence served by adm type for violent offenses
all_bar_sentence_violent_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Violent") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_violent_2020 <- setNames(all_bar_sentence_violent_2020, states)


########
# Drugs
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Drugs") %>%
  pull(state) %>%
  unique()

all_bar_sentence_drugs_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Drugs") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_drugs_2020 <- setNames(all_bar_sentence_drugs_2020, states)


########
# Property
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Property") %>%
  pull(state) %>%
  unique()

all_bar_sentence_property_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Property") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_property_2020 <- setNames(all_bar_sentence_property_2020, states)


########
# Public order
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Public order") %>%
  pull(state) %>%
  unique()

all_bar_sentence_publicorder_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Public order") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_publicorder_2020 <- setNames(all_bar_sentence_publicorder_2020, states)


########
# Other
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Other/unspecified") %>%
  pull(state) %>%
  unique()

all_bar_sentence_other_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Other/unspecified") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_percent_bar_chart_sentence_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_sentence_other_2020 <- setNames(all_bar_sentence_other_2020, states)








################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(current_ped_2020_offenses,
       file=file.path(folder, "current_ped_2020_offenses.rds"))
  save(all_bar_parole_elgibility_offense,
       file=file.path(folder, "all_bar_parole_elgibility_offense.rds"))
  save(all_pie_parole_elgibility_offense,
       file=file.path(folder, "all_pie_parole_elgibility_offense.rds"))
  save(all_pie_parole_elgibility_offense_parole_return,
       file=file.path(folder, "all_pie_parole_elgibility_offense_parole_return.rds"))
  save(all_pie_parole_elgibility_offense_new_crime,
       file=file.path(folder, "all_pie_parole_elgibility_offense_new_crime.rds"))
  save(all_sentence_parole_elgibility_offense,
       file=file.path(folder, "all_sentence_parole_elgibility_offense.rds"))

  save(all_bar_sentence_overview_2020,
       file=file.path(folder, "all_bar_sentence_overview_2020.rds"))
  save(all_bar_sentence_publicorder_2020,
       file=file.path(folder, "all_bar_sentence_publicorder_2020.rds"))
  save(all_bar_sentence_property_2020,
       file=file.path(folder, "all_bar_sentence_property_2020.rds"))
  save(all_bar_sentence_other_2020,
       file=file.path(folder, "all_bar_sentence_other_2020.rds"))
  save(all_bar_sentence_drugs_2020,
       file=file.path(folder, "all_bar_sentence_drugs_2020.rds"))
  save(all_bar_sentence_violent_2020,
       file=file.path(folder, "all_bar_sentence_violent_2020.rds"))
}
