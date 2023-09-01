#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: September 1, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#######################################

################################################################################

# Highchart - People in prison by race, age range, gender, sentence length, offenses

# Obtained from NCRP year end population

################################################################################

# Function to generate grouped data for stacked bar chart
fnc_generate_grouped_data <- function(df, year, admtype_col, group_by_col) {
  df %>%
    filter(rptyear == year & admtype == admtype_col) %>%
    group_by(state) %>%
    count(!!sym(group_by_col)) %>%
    mutate(
      prop = (n / sum(n)) * 100,
      prop_label = paste0(round(prop, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       group_by_col, ": <b>", !!sym(group_by_col),
                       "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b>", sep = "")
    )
}

states <- unique(ncrp_yearendpop$state)

###################
# Parole return/revocation
###################

# Race
ncrp_yearendpop_parole_return_race_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "race")

all_stackedbar_parole_return_race_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_parole_return_race_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    mutate(race = factor(race,
                         levels = c("Black, non-Hispanic",
                                    "Unknown",
                                    "Other race(s), non-Hispanic",
                                    "White, non-Hispanic",
                                    "Hispanic, any race"
                                    )))
  highcharts <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4)) %>%
    hc_add_series(name = "Unknown",
                  data = df1 %>%
                    filter(race == "Unknown") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = darkblue,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Other race(s), non-Hispanic",
                  data = df1 %>% filter(race == "Other race(s), non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = orange,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "White, non-Hispanic",
                  data = df1 %>% filter(race == "White, non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = yellow,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Hispanic, any race",
                  data = df1 %>% filter(race == "Hispanic, any race") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = purple,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Black, non-Hispanic",
                  data = df1 %>% filter(race == "Black, non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = teal,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}")))
  return(highcharts)
})


all_stackedbar_parole_return_race_2020 <- setNames(all_stackedbar_parole_return_race_2020, states)
all_stackedbar_parole_return_race_2020$California
all_stackedbar_parole_return_race_2020$Georgia























# Age
ncrp_yearendpop_parole_return_ageyrend_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "ageyrend")

all_stackedbar_parole_return_ageyrend_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_parole_return_ageyrend_2020 %>%
    filter(state == x) %>%
    mutate(ageyrend = factor(ageyrend,
                             levels = c("55+ years",
                                        "45-54 years",
                                        "35-44 years",
                                        "25-34 years",
                                        "18-24 years")))

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "ageyrend")
  return(highcharts)
})
all_stackedbar_parole_return_ageyrend_2020 <- setNames(all_stackedbar_parole_return_ageyrend_2020, states)
all_stackedbar_parole_return_ageyrend_2020$Georgia



























# Sex
ncrp_yearendpop_parole_return_sex_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "sex")

all_stackedbar_parole_return_sex_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_parole_return_sex_2020 %>% filter(state == x)

  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "sex")
  return(highcharts)
})
all_stackedbar_parole_return_sex_2020 <- setNames(all_stackedbar_parole_return_sex_2020, states)
all_stackedbar_parole_return_sex_2020$Georgia




###################
# New court commitment
###################

# Race
ncrp_yearendpop_new_crime_race_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "race")

all_stackedbar_new_crime_race_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_new_crime_race_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    mutate(race = factor(race,
                         levels = c("Black, non-Hispanic",
                                    "Unknown",
                                    "Other race(s), non-Hispanic",
                                    "White, non-Hispanic",
                                    "Hispanic, any race"
                         )))
  highcharts <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal",
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4)) %>%
    hc_add_series(name = "Unknown",
                  data = df1 %>%
                    filter(race == "Unknown") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = darkblue,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Other race(s), non-Hispanic",
                  data = df1 %>% filter(race == "Other race(s), non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = orange,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "White, non-Hispanic",
                  data = df1 %>% filter(race == "White, non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = yellow,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Hispanic, any race",
                  data = df1 %>% filter(race == "Hispanic, any race") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = purple,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}"))) %>%
    hc_add_series(name = "Black, non-Hispanic",
                  data = df1 %>% filter(race == "Black, non-Hispanic") %>% select(prop, prop_label) %>%
                    mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
                    pull(data_point),
                  color = teal,
                  dataLabels = list(enabled = TRUE,
                                    style = list(fontSize = '1em'),
                                    formatter = JS("function() {return this.point.prop_label;}")))
  return(highcharts)
})


all_stackedbar_new_crime_race_2020 <- setNames(all_stackedbar_new_crime_race_2020, states)
all_stackedbar_new_crime_race_2020$California
all_stackedbar_new_crime_race_2020$Georgia




























# Age
ncrp_yearendpop_new_crime_ageyrend_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "ageyrend")

all_stackedbar_new_crime_ageyrend_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_new_crime_ageyrend_2020 %>%
    filter(state == x) %>%
    mutate(ageyrend = factor(ageyrend,
                             levels = c("55+ years",
                                        "45-54 years",
                                        "35-44 years",
                                        "25-34 years",
                                        "18-24 years")))

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "ageyrend")
  return(highcharts)
})
all_stackedbar_new_crime_ageyrend_2020 <- setNames(all_stackedbar_new_crime_ageyrend_2020, states)
all_stackedbar_new_crime_ageyrend_2020$Georgia




# Sex
ncrp_yearendpop_new_crime_sex_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "sex")

all_stackedbar_new_crime_sex_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_new_crime_sex_2020 %>% filter(state == x)

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "sex")
  return(highcharts)
})
all_stackedbar_new_crime_sex_2020 <- setNames(all_stackedbar_new_crime_sex_2020, states)
all_stackedbar_new_crime_sex_2020$Georgia












################################################################################

# Highchart - Trend line graph
# Line graph data showing the change in prison population
#     and change in people released to parole

# Obtained from NCRP year end population and APS Surveys from 2000-2018
# NEED TO CHANGE NCRP YEAR END POPULATION TO BJS CORRECTIONAL STATISTICS????????????

################################################################################

state_names_abb <-
  data.frame(abbreviation = state.abb,
             name = state.name,
             stringsAsFactors = FALSE)

state_names_abb <- state_names_abb %>%
  rename(state = name,
         stateid = abbreviation)

aps_parole_2018 <- da38058.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2018) %>%
  fnc_aps_prepare()

aps_parole_2017 <- da37471.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2017) %>%
  fnc_aps_prepare()

aps_parole_2016 <- da37441.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2016) %>%
  fnc_aps_prepare()

aps_parole_2015 <- da36619.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2015) %>%
  fnc_aps_prepare()

aps_parole_2014 <- da36320.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2014) %>%
  fnc_aps_prepare()

aps_parole_2013 <- da35629.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2013) %>%
  fnc_aps_prepare()

aps_parole_2012 <- da35257.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 5, -1),
         rptyear = 2012) %>%
  mutate(state = str_trim(state)) %>%
  fnc_aps_prepare()

aps_parole_2011 <- da34718.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2011) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2010 <- da34382.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2010) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2009 <- da34381.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2009) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2008 <- da34380.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2008) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2007 <- da31332.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2007) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2006 <- da31331.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2006) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2005 <- da31330.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2005) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2004 <- da31329.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2004) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2003 <- da31328.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2003) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2002 <- da31327.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2002) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2001 <- da31326.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2001) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2000 <- da31325.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2000) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2000_2018 <- rbind(aps_parole_2018,
                              aps_parole_2017,
                              aps_parole_2016,
                              aps_parole_2015,
                              aps_parole_2014,
                              aps_parole_2013,
                              aps_parole_2012,
                              aps_parole_2011,
                              aps_parole_2010,
                              aps_parole_2009,
                              aps_parole_2008,
                              aps_parole_2007,
                              aps_parole_2006,
                              aps_parole_2005,
                              aps_parole_2004,
                              aps_parole_2003,
                              aps_parole_2002,
                              aps_parole_2001,
                              aps_parole_2000)

aps_parole_2000_2018 <- aps_parole_2000_2018 %>%
  filter(state != "District of Columbia" &
           state != "Federal" &
           !is.na(state))

# get prison population by report year and state
# merge with APS data for releases to parole and entries to parole from prison
# create prison population variable if people who are PE were released
# aps_parole_2000_2018 table created in parole_aps.R
# parole_eligibility_table table created in parole_eligibility_ncrp.R
all_ncrp_aps_pop_released_to_parole_by_year <- ncrp_yearendpop %>%
  filter(rptyear >= 2000) %>%
  group_by(rptyear, state) %>%
  summarise(total_prison_population = n()) %>%
  ungroup() %>%
  left_join(aps_parole_2000_2018,
            by = c("state", "rptyear")) %>%
  left_join(parole_eligibility_table,
            by = c("state", "rptyear")) %>%
  mutate(prison_population_without_pe = coalesce(total_prison_population, 0) - coalesce(current_new_crime_count, 0),
         prison_populations_same =
           ifelse(prison_population_without_pe == total_prison_population, TRUE, FALSE))


# get list of states
states <- unique(all_ncrp_aps_pop_released_to_parole_by_year$state)

all_line_pop_released_to_parole <- map(.x = states,  .f = function(x) {

  df1 <- all_ncrp_aps_pop_released_to_parole_by_year %>%
    filter(state == x)

  highcharts <-

    highchart() %>%
    hc_xAxis(categories = df1$rptyear,
             labels = list(format = "{value}")) %>%
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%

    hc_series(list(name = "Prison Population",
                   data = df1$total_prison_population),
              # list(name = "Projected Prison Population if People who were Parole Eligible were Released",
              #      data = df1$prison_population_without_pe,
              #      dashStyle = "Dash"),
              list(name = "Released from Prison to Parole",
                   data = df1$released_to_parole),
              list(name = "Parole Eligible but not Released from Prison for a New Crime",
                   data = df1$current_new_crime_count)) %>%

    hc_add_theme(hc_theme_jc_line) %>%
    # hc_add_theme(hc_theme_jc) %>%

    hc_colors(colors = c(teal, "#75d9d4", yellow, orange)) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))


  return(highcharts)
})

all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)













################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_stackedbar_new_crime_sex_2020,          file = file.path(folder, "all_stackedbar_new_crime_sex_2020.rds"))
  save(all_stackedbar_new_crime_ageyrend_2020,     file = file.path(folder, "all_stackedbar_new_crime_ageyrend_2020.rds"))
  save(all_stackedbar_new_crime_race_2020,         file = file.path(folder, "all_stackedbar_new_crime_race_2020.rds"))
  save(all_stackedbar_parole_return_sex_2020,      file = file.path(folder, "all_stackedbar_parole_return_sex_2020.rds"))
  save(all_stackedbar_parole_return_ageyrend_2020, file = file.path(folder, "all_stackedbar_parole_return_ageyrend_2020.rds"))
  save(all_stackedbar_parole_return_race_2020,     file = file.path(folder, "all_stackedbar_parole_return_race_2020.rds"))

  save(all_line_pop_released_to_parole,            file = file.path(folder, "all_line_pop_released_to_parole.rds"))

}
