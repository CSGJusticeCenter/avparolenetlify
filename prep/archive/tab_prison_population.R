#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: September 5, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#######################################

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

# Function to create stacked bar
fnc_generate_horzstackedbar_chart <- function(df, group_by_col) {
  hchart(df, "bar",
         hcaes(x = state,
               y = prop,
               group = !!sym(group_by_col)
         ),
         dataLabels = list(enabled = TRUE,
                           format = "{point.prop_label}",
                           style = list(fontWeight = "bold",
                                        fontSize = "16px",
                                        fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(
      series = list(
        stacking = "normal", animation = FALSE, cursor = "pointer",
        borderWidth = 3, minPointLength = 4),
      accessibility = list(
        enabled = TRUE, keyboardNavigation = list(enabled = TRUE),
        linkedDescription = "TBD.", landmarkVerbosity = "one"),
      area = list(accessibility = list(description = "TBD.")))
}

################################################################################

# Section: New crime vs Parole Revocations

# Highchart - Prison population by admission type (new crime vs parole return)
# Obtained from NCRP year end population

################################################################################

# Get number/prop of people by admission type and state in 2020
ncrp_yearendpop_admtype_2020 <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" | admtype == "Parole return/revocation") %>%
  filter(rptyear == 2020) %>%
  group_by(state) %>%
  count(admtype) %>%
  mutate(
    prop = (n / sum(n)) * 100,
    prop_label = paste0(round(prop, 0), "%"),
    n_label = formattable::comma(n, 0),
    tooltip = paste0("<b>", state, "</b><br><br>",
                     "<b>", admtype,"</b><br><br>",
                     "Percentage of People: <b>", prop_label, "</b>", sep = ""))

# Get list of states
states <- unique(ncrp_yearendpop_admtype_2020$state)

# Generate highchart for each state showing prison pop by admission type
all_stackedbar_admtype_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_admtype_2020 %>%
    ungroup() %>%
    filter(state == x)

  highcharts <- hchart(df1, "bar",
                       hcaes(x = state,
                             y = prop,
                             group = admtype),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "bold",
                                                      fontSize = "16px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(format = "{value}%",
                           enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_colors(c(teal, yellow)) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
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

# Assign state names
all_stackedbar_admtype_2020 <- setNames(all_stackedbar_admtype_2020, states)






################################################################################

# Section Prison Population Trends

# Highchart - Trend line graph
# Line graph data showing the change in prison population
#     and change in people released to parole

# Obtained from NCRP year end population and APS Surveys from 2000-2018
# NEED TO CHANGE NCRP YEAR END POPULATION TO BJS CORRECTIONAL STATISTICS????????????

################################################################################

# Get state abb
state_names_abb <-
  data.frame(abbreviation = state.abb,
             name = state.name,
             stringsAsFactors = FALSE)

state_names_abb <- state_names_abb %>%
  rename(state = name,
         stateid = abbreviation)

# Clean APS data y year
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

# Add each APS survey data together
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

# Remove DC
aps_parole_2000_2018 <- aps_parole_2000_2018 %>%
  filter(state != "District of Columbia" &
           state != "Federal" &
           !is.na(state))

# Get prison population by report year and state
# Merge with APS data for releases to parole and entries to parole from prison
# Create prison population variable if people who are PE were released
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

# List of states
states <- unique(all_ncrp_aps_pop_released_to_parole_by_year$state)

# Create highcharts of trend in prison population and people released from prison from 2000-2020
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

# Assign state names
all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)






################################################################################

# Section: Who's in Prison?

# Highchart - People in prison by race, age range, gender, sentence length, offenses
# Obtained from NCRP year end population

################################################################################

###################
# Parole return/revocation
###################

# RACE
# Get number/prop people by race
ncrp_yearendpop_parole_return_race_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "race")

# List of states
states <- unique(ncrp_yearendpop_parole_return_race_2020$state)

# Create highchart of people in prison by race in 2020
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

# Assign state names
all_stackedbar_parole_return_race_2020 <- setNames(all_stackedbar_parole_return_race_2020, states)




# AGE
# Get number/prop people by age
ncrp_yearendpop_parole_return_ageyrend_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "ageyrend")

# List of states
states <- unique(ncrp_yearendpop_parole_return_ageyrend_2020$state)

# Create highchart of people in prison by age
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

# Assign state names
all_stackedbar_parole_return_ageyrend_2020 <- setNames(all_stackedbar_parole_return_ageyrend_2020, states)




# SEX
# Get number/prop people by sex
ncrp_yearendpop_parole_return_sex_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "Parole return/revocation", "sex")

# List of states
states <- unique(ncrp_yearendpop_parole_return_sex_2020$state)

# Create highchart of people in prison by sex
all_stackedbar_parole_return_sex_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_parole_return_sex_2020 %>% filter(state == x)

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "sex")
  highcharts <- highcharts %>% hc_colors(c(yellow, teal))
  return(highcharts)
})
all_stackedbar_parole_return_sex_2020 <- setNames(all_stackedbar_parole_return_sex_2020, states)






###################
# New court commitment
###################

# RACE
# Get number/prop of people in prison for a new crime by race
ncrp_yearendpop_new_crime_race_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "race")

# List of states
states <- unique(ncrp_yearendpop_new_crime_race_2020$state)

# Create highchart of people in prison for a new crime by race
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

# Assign state names
all_stackedbar_new_crime_race_2020 <- setNames(all_stackedbar_new_crime_race_2020, states)





# AGE
# Get number/prop of people in prison for a new crime by age
ncrp_yearendpop_new_crime_ageyrend_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "ageyrend")

# List of states
states <- unique(ncrp_yearendpop_new_crime_ageyrend_2020$state)

# Create highcharts of people in prison for a new crime by age
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

# Assign state names
all_stackedbar_new_crime_ageyrend_2020 <- setNames(all_stackedbar_new_crime_ageyrend_2020, states)






# SEX
# Get number/prop of people in prison for a new crime by sex
ncrp_yearendpop_new_crime_sex_2020 <-
  fnc_generate_grouped_data(ncrp_yearendpop, 2020, "New court commitment", "sex")

# Get list of states
states <- unique(ncrp_yearendpop_new_crime_sex_2020$state)

# Create highcharts of people in prison for a new crime by sex
all_stackedbar_new_crime_sex_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_new_crime_sex_2020 %>% filter(state == x)

  highcharts <- fnc_generate_horzstackedbar_chart(df1, "sex")
  highcharts <- highcharts %>% hc_colors(c(yellow, teal))

  return(highcharts)
})

# Assign state names
all_stackedbar_new_crime_sex_2020 <- setNames(all_stackedbar_new_crime_sex_2020, states)






################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_stackedbar_admtype_2020,                file = file.path(folder, "all_stackedbar_admtype_2020.rds"))

  save(all_stackedbar_new_crime_sex_2020,          file = file.path(folder, "all_stackedbar_new_crime_sex_2020.rds"))
  save(all_stackedbar_new_crime_ageyrend_2020,     file = file.path(folder, "all_stackedbar_new_crime_ageyrend_2020.rds"))
  save(all_stackedbar_new_crime_race_2020,         file = file.path(folder, "all_stackedbar_new_crime_race_2020.rds"))
  save(all_stackedbar_parole_return_sex_2020,      file = file.path(folder, "all_stackedbar_parole_return_sex_2020.rds"))
  save(all_stackedbar_parole_return_ageyrend_2020, file = file.path(folder, "all_stackedbar_parole_return_ageyrend_2020.rds"))
  save(all_stackedbar_parole_return_race_2020,     file = file.path(folder, "all_stackedbar_parole_return_race_2020.rds"))

  save(all_line_pop_released_to_parole,            file = file.path(folder, "all_line_pop_released_to_parole.rds"))

}
