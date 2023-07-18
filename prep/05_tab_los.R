#######################################
# Project: AV Parole
# File: tab_los.R
# Authors: Mari Roberts
# Date last updated: July 17, 2023 (MAR)
# Description:
#    LOS tables and graphics for app
#######################################

##################

# Data cleaning for:
# Proportion of time served by offense type, year, and state

##################

# assign x axis order
desired_order <- c("Less than Sentence Length Served",
                   "Full Sentence Length Served",
                   "More than Sentence Length Served")

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








##################

# Save data

##################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

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
