#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: August 29, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#######################################


################################################################################

# Highchart - People in prison by race, age range, gender, sentence length, offenses

# Obtained from NCRP year end population
# NEED TO CHANGE NCRP YEAR END POPULATION TO BJS CORRECTIONAL STATISTICS????????????

################################################################################

# Get proportion and number of people in prison by race and admission type
ncrp_yearendpop_admtype_race_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(admtype == "New court commitment" | admtype == "Parole return/revocation") %>%
  group_by(state) %>%
  count(race, admtype) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Race and Ethnicity: <b>",
                  race,
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# Create grouped bar chart showing proportion of people in prison by race and admission type










ncrp_yearendpop_admtype_age_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(admtype == "New court commitment" | admtype == "Parole return/revocation") %>%
  group_by(state) %>%
  count(ageyrend_category, admtype) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Age Range: <b>",
                  ageyrend_category,
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

ncrp_yearendpop_admtype_gender_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(admtype == "New court commitment" | admtype == "Parole return/revocation") %>%
  group_by(state) %>%
  count(sex, admtype) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Gender: <b>",
                  sex,
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

## change to FBI 10 crime types????
ncrp_yearendpop_admtype_offense_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  filter(admtype == "New court commitment" | admtype == "Parole return/revocation") %>%
  group_by(state) %>%
  count(offgeneral, admtype) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Offense Type: <b>",
                  offgeneral,
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))








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

  save(all_line_pop_released_to_parole, file=file.path(folder, "all_line_pop_released_to_parole.rds"))

}
