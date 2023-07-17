#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: July 17, 2023 (MAR)
# Description:
#    Parole eligibility tables and graphics for shiny app
#######################################


################################################################################

# NCRP - Parole eligibility in 2020

################################################################################

# get number and percentage of eligibility statuses
parole_eligibility_counts <- ncrp_yearendpop %>%
  group_by(state, rptyear) %>%
  count(parelig_status) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n)
  ) %>%
  ungroup()

# reformat for table viewing
parole_eligibility_table <- parole_eligibility_counts %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, rptyear, yearendpop, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()

# filter to 2020
parole_eligibility_table_2020 <- parole_eligibility_table %>%
  filter(rptyear == 2020)

# missing data
# Arizona, Michigan, New Jersey, New Mexico
missing_states <- state.name[!state.name %in% parole_eligibility_table_2020$state]

# create a new dataframe with the missing states and NA values
missing_data <- tibble(state = missing_states)
missing_data <- missing_data %>% mutate(rptyear = 2020)

# combine the missing data with the original dataframe
parole_eligibility_table_2020 <- bind_rows(parole_eligibility_table_2020, missing_data) %>%
  arrange(state)







################################################################################

# NCRP - Offenses for those in prison but not released in 2020

################################################################################

# get most serious sentenced offense for people eligible for parole but still in prison
current_ped_2020_offenses <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(offgeneral)) %>%
  mutate(offgeneral = ifelse(
    offgeneral == "Other/unspecified", "Other or Unspecified", offgeneral
  )) %>%
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

# get parole eligible population by race in 2020
current_ped_2020_race <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(parelig_status == "Current") %>%
  filter(!is.na(race)) %>%
  group_by(state) %>%
  count(race) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%")
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          race, "</b><br>",
                          prop_label, "<br>"))





####################
# Bar chart about parole eligibility by race
####################

states <- unique(current_ped_2020_race$state)

# generate bar chart about most serious sentenced offense in 2020 by state
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(prop))
  xaxis_order <- df1$race

  # assign color for each race
  df1$color <- case_when(df1$race == "Black, non-Hispanic" ~ yellow,
                         df1$race == "White, non-Hispanic" ~ orange,
                         df1$race == "Hispanic, any race" ~ teal,
                         df1$race == "Other race(s), non-Hispanic" ~ purple)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(race), y = prop*100, color = color
                        ),
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

all_bar_parole_elgibility_race <- setNames(all_bar_parole_elgibility_race, states)






####################
# Sentence about race and parole eligibility
####################

# get list of states
states <- unique(current_ped_2020_race$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, ", df1$race,
                      " people constituted the most number of people eligible for parole but still in prison, accounting for ",
                      df1$prop_label, " (", formattable::comma(df1$n, digits = 0), " people) of the parole-eligible prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_race <- setNames(all_sentence_parole_elgibility_race, states)

# # get population by race in 2020
# pop_2020_race <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status != "Missing") %>%
#   filter(!is.na(race)) %>%
#   group_by(state) %>%
#   count(race) %>%
#   select(state, race, total_prison_pop_by_race = n)
#
# current_ped_2020_race1 <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status == "Current") %>%
#   filter(!is.na(race)) %>%
#   group_by(state, race) %>%
#   count(race) %>%
#   rename(currently_eligible_for_parole = n) %>%
#   left_join(pop_2020_race, by = c("state", "race")) %>%
#   mutate(
#     prop = currently_eligible_for_parole/total_prison_pop_by_race,
#     prop_label = paste0(round(prop*100, 0), "%")
#   )





####################
# Sentence about most serious offense
####################

# get list of states
states <- unique(current_ped_2020_offenses$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, ", tolower(df1$offgeneral),
                      " offenses constituted the most serious sentenced offense for individuals eligible for parole but still in prison, accounting for ",
                      df1$prop_label, " (", formattable::comma(df1$n, digits = 0), " people) of the parole-eligible prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_offense <- setNames(all_sentence_parole_elgibility_offense, states)





####################
# Pie chart about most serious offense
####################

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





####################
# Bar chart about most serious offense
# Same as above but in bar chart form
####################

states <- unique(current_ped_2020_offenses$state)

# generate bar chart about most serious sentenced offense in 2020 by state
all_bar_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>%
    filter(state == x) %>%
    arrange(desc(prop))
  xaxis_order <- df1$offgeneral
  highcharts <-
    highchart() %>%
    #hc_chart(margin = c(90, 0, 50, 0)) %>%
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

# NCRP - Parole eligibility by adm type and year

################################################################################

parole_eligibility_rate_by_admtype <- ncrp_yearendpop %>%
  filter(!is.na(parelig_status) & !is.na(admtype) &
           admtype != "Other admission (including unsentenced, transfer, AWOL/escapee return)") %>%
  group_by(state, rptyear, parelig_status) %>%
  count(admtype) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n),
    prop = prop*100) %>%
  ungroup() %>%
  mutate(tooltip =
           case_when(admtype == "New court commitment" ~
                       paste0("<b>", state, "</b><br>",
                              "New court commitment:<br>",
                              paste(round(prop, 1), "%</b>", sep = ""), "<br>"),
                     admtype == "Parole return/revocation" ~
                       paste0("<b>", state, "</b><br>",
                              "Parole return/revocation:<br>",
                              paste(round(prop, 1), "%</b>", sep = ""), "<br>")))

# get number and percentage of eligibility statuses by adm type (new court committment by parole eligibility status)
parole_eligibility_admtype_counts <- ncrp_yearendpop %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  group_by(state, rptyear, admtype) %>%
  count(parelig_status) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n)
  ) %>%
  ungroup()

# reformat for table viewing
parole_eligibility_admtype_table <- parole_eligibility_admtype_counts %>%
  pivot_longer(cols = c(n, prop), names_to = "type", values_to = "value") %>%
  mutate(name = case_when(
    type == "n"    ~ paste(parelig_status, "count"),
    type == "prop" ~ paste(parelig_status, "perc.")
  )) %>%
  select(state, rptyear, admtype, yearendpop, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  clean_names()

# filter to 2020
parole_eligibility_admtype_table_2020 <- parole_eligibility_admtype_table %>%
  filter(rptyear == 2020)

parole_eligibility_admtype_table_2020 <- parole_eligibility_admtype_table_2020 %>%
  pivot_wider(names_from = admtype,
              values_from = c(yearendpop,
                              missing_count,
                              missing_perc,
                              current_count,
                              current_perc,
                              future_1_5_years_count,
                              future_1_5_years_perc,
                              future_6_years_count,
                              future_6_years_perc),
              names_sep = "_")

# missing data
# Arizona, Michigan, New Jersey, New Mexico
missing_states <- state.name[!state.name %in% parole_eligibility_admtype_table_2020$state]

# create a new dataframe with the missing states and NA values
missing_data <- tibble(state = missing_states)
missing_data <- missing_data %>% mutate(rptyear = 2020)

# combine the missing data with the original dataframe
parole_eligibility_admtype_table_2020 <- bind_rows(parole_eligibility_admtype_table_2020, missing_data) %>%
  arrange(state)






##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table,               file=file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_2020,          file=file.path(folder, "parole_eligibility_table_2020.rds"))
  save(current_ped_2020_offenses,              file=file.path(folder, "current_ped_2020_offenses.rds"))
  save(current_ped_2020_race,                  file=file.path(folder, "current_ped_2020_race.rds"))

  save(all_bar_parole_elgibility_race,         file=file.path(folder, "all_bar_parole_elgibility_race.rds"))
  save(all_sentence_parole_elgibility_race,    file=file.path(folder, "all_sentence_parole_elgibility_race.rds"))

  save(all_bar_parole_elgibility_offense,      file=file.path(folder, "all_bar_parole_elgibility_offense.rds"))
  save(all_pie_parole_elgibility_offense,      file=file.path(folder, "all_pie_parole_elgibility_offense.rds"))
  save(all_sentence_parole_elgibility_offense, file=file.path(folder, "all_sentence_parole_elgibility_offense.rds"))

}
