#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: July 31, 2023 (MAR)
# Description:
#    Parole eligibility tables and graphics for shiny app
#######################################


################################################################################

# Reactable table in national trends page
# Parole eligibility in 2020

################################################################################

# get number and prop of people by eligibility statuses, by state and report year
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

# find missing data
# Arizona, Michigan, New Jersey, New Mexico
missing_data <- tibble(state = setdiff(state.name, parole_eligibility_table_2020$state),
                       rptyear = 2020)

# combine the missing data with the original dataframe
parole_eligibility_table_2020 <- bind_rows(parole_eligibility_table_2020, missing_data) %>%
  arrange(state)








################################################################################

# Parole eligibility by adm type and year

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







################################################################################

# Parole eligibility by adm type highcharts and sentence

################################################################################

# get list of states
states <- parole_eligibility_rate_by_admtype %>%
  ungroup() %>%
  filter(rptyear == 2020 &
         parelig_status == "Current" &
         admtype == "Parole return/revocation") %>%
  pull(state) %>%
  unique()

# pie chart
# currently eligible for parole and in prison for parole return
all_pie_ped_current <- map(.x = states,  .f = function(x) {

  df1 <- parole_eligibility_rate_by_admtype %>%
    filter(rptyear == 2020 &
           state == x &
           parelig_status == "Current") %>%
    mutate(prop_label = paste0(round(prop, 0), "%"))

  df2 <- df1 %>%
    filter(admtype == "Parole return/revocation")

  highcharts <- fnc_donut_chart(df = df1,
                                df_pct = df2,
                                x_variable = "admtype",
                                y_variable = "prop",
                                accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_chart(width = 250, height = 250) %>%
    hc_colors(colors = c(neutralBkgndMedium, teal))

  return(highcharts)
})

all_pie_ped_current <- setNames(all_pie_ped_current, states)

# get list of states
states <- parole_eligibility_rate_by_admtype %>%
  ungroup() %>%
  filter(rptyear == 2020 &
           parelig_status == "Future 1-5 Years" &
           admtype == "Parole return/revocation") %>%
  pull(state) %>%
  unique()

# eligible for parole in the next 1-5 years and in prison for parole return
all_pie_ped_future_1_5 <- map(.x = states,  .f = function(x) {

  df1 <- parole_eligibility_rate_by_admtype %>%
    filter(rptyear == 2020 &
             state == x &
             parelig_status == "Future 1-5 Years") %>%
    mutate(prop_label = paste0(round(prop, 0), "%"))

  df2 <- df1 %>%
    filter(admtype == "Parole return/revocation")

  highcharts <- fnc_donut_chart(df = df1,
                                df_pct = df2,
                                x_variable = "admtype",
                                y_variable = "prop",
                                accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_chart(width = 250, height = 250) %>%
    hc_colors(colors = c(neutralBkgndMedium, teal))

  return(highcharts)
})

all_pie_ped_future_1_5 <- setNames(all_pie_ped_future_1_5, states)

# # get states
# states <- parole_eligibility_rate_by_admtype %>%
#   ungroup() %>%
#   filter(rptyear == 2020 &
#          parelig_status == "Current") %>%
#   pull(state) %>%
#   unique()
#
# all_pie_ped_current <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_rate_by_admtype %>%
#     filter(rptyear == 2020 &
#            state == x &
#            parelig_status == "Current")
#
#   highcharts <-
#     fnc_pie_chart_highlight(df = df1,
#                             x_variable = "admtype",
#                             y_variable = "prop",
#                             point_format = "{point.admtype}: {point.prop:.0f}%",
#                             accessibility_text = "TBD.")
#   highcharts <- highcharts %>%
#     hc_colors(colors = c(neutralBkgndMedium, orange))
#
#   return(highcharts)
# })
#
# all_pie_ped_current <- setNames(all_pie_ped_current, states)
#
# # get states
# states <- parole_eligibility_rate_by_admtype %>%
#   ungroup() %>%
#   filter(rptyear == 2020 &
#          parelig_status == "Future 1-5 Years") %>%
#   pull(state) %>%
#   unique()
#
# all_pie_ped_future_1_5 <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_rate_by_admtype %>%
#     filter(rptyear == 2020 &
#            state == x &
#            parelig_status == "Future 1-5 Years")
#
#   highcharts <-
#     fnc_pie_chart_highlight(df = df1,
#                             x_variable = "admtype",
#                             y_variable = "prop",
#                             point_format = "{point.admtype}: {point.prop:.0f}%",
#                             accessibility_text = "TBD.")
#   highcharts <- highcharts %>%
#     hc_colors(colors = c(neutralBkgndMedium, orange))
#
#   return(highcharts)
# })
#
# all_pie_ped_future_1_5 <- setNames(all_pie_ped_future_1_5, states)
#
# # get states
# states <- parole_eligibility_rate_by_admtype %>%
#   ungroup() %>%
#   filter(rptyear == 2020 &
#          parelig_status == "Future 6+ Years") %>%
#   pull(state) %>%
#   unique()
#
# all_pie_ped_future_6 <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_rate_by_admtype %>%
#     filter(rptyear == 2020 &
#            state == x &
#            parelig_status == "Future 6+ Years")
#
#   highcharts <-
#     fnc_pie_chart_highlight(df = df1,
#                             x_variable = "admtype",
#                             y_variable = "prop",
#                             point_format = "{point.admtype}: {point.prop:.0f}%",
#                             accessibility_text = "TBD.")
#   highcharts <- highcharts %>%
#     hc_colors(colors = c(neutralBkgndMedium, orange))
#
#   return(highcharts)
# })
#
# all_pie_ped_future_6 <- setNames(all_pie_ped_future_6, states)









################################################################################

# Sentence about parole eligibility and adm type

################################################################################

# get list of states
states <- parole_eligibility_rate_by_admtype %>%
  ungroup() %>%
  filter(rptyear == 2020 &
           parelig_status == "Current") %>%
  pull(state) %>%
  unique()

# generate sentence about parole eligible populations by race and state in 2020
all_sentence_parole_elgibility_admtype <- map(.x = states,  .f = function(x) {
  df1 <- parole_eligibility_rate_by_admtype %>%
    filter(state == x &
           admtype == "Parole return/revocation" &
           rptyear == 2020 &
           parelig_status == "Current")
  sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
                      " people who were incarcerated for a parole revocation and were eligible for parole but not yet released from prison.")
  return(sentences)
})

all_sentence_parole_elgibility_admtype <- setNames(all_sentence_parole_elgibility_admtype, states)








################################################################################

# Parole eligibility by race

################################################################################

# parole eligible population but still in prison by race in 2020
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







################################################################################

# Bar chart about parole eligibility by race

################################################################################

# get states
states <- unique(current_ped_2020_race$state)

# generate bar chart showing parole eligible populations by race and state in 2020
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







################################################################################

# Sentence about parole eligibility and race

################################################################################

# get list of states
states <- unique(current_ped_2020_race$state)

# generate sentence about parole eligible populations by race and state in 2020
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








################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(parole_eligibility_table,               file=file.path(folder, "parole_eligibility_table.rds"))
  save(parole_eligibility_table_2020,          file=file.path(folder, "parole_eligibility_table_2020.rds"))

  save(all_sentence_parole_elgibility_admtype, file=file.path(folder, "all_sentence_parole_elgibility_admtype.rds"))
  save(all_pie_ped_current,                    file=file.path(folder, "all_pie_ped_current.rds"))
  save(all_pie_ped_future_1_5,                 file=file.path(folder, "all_pie_ped_future_1_5.rds"))

  save(current_ped_2020_race,                  file=file.path(folder, "current_ped_2020_race.rds"))
  save(all_sentence_parole_elgibility_race,    file=file.path(folder, "all_sentence_parole_elgibility_race.rds"))
  save(all_bar_parole_elgibility_race,         file=file.path(folder, "all_bar_parole_elgibility_race.rds"))

}
