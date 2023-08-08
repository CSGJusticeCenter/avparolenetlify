#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: July 10, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#######################################


################################################################################

# Annual Parole Survey Series in 2018 data for analysis

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







################################################################################

# Line graph data showing the change in prison population
# and change in people released to parole

################################################################################

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
  mutate(prison_population_without_pe = coalesce(total_prison_population, 0) - coalesce(current_count, 0),
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
              list(name = "Parole Eligible but not Released from Prison",
                   data = df1$current_count)) %>%

    hc_add_theme(hc_theme_jc_line) %>%
    hc_colors(colors = c(teal, "#75d9d4", yellow, orange)) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))


  return(highcharts)
})

all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)







################################################################################

# Proportion of prison population who are parole eligible

################################################################################

ncrp_pe_type_2020 <- all_ncrp_aps_pop_released_to_parole_by_year %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count
  ) %>%
  filter(rptyear == 2020) %>%
  pivot_longer(cols = c(current_count,
                        future_1_5_years_count,
                        future_6_years_count,
                        missing_count),
               names_to = "count_type",
               values_to = "n") %>%
  mutate(count_type = case_when(
    count_type == "current_count"          ~ "Currently Eligible<br>for Parole",
    count_type == "future_1_5_years_count" ~ "Eligible for Parole<br>in 1-5 Years",
    count_type == "future_6_years_count"   ~ "Eligible for Parole<br>in 6+ Years",
    count_type == "missing_count"          ~ "Missing Data or Not<br>Eligible for Parole" # WILL NEED TO CHANGE FOR STATES THAT ABOLISHED PAROLE
  )) %>%
  group_by(state) %>%
  mutate(prop = ifelse(sum(!is.na(n)) == 1 & !is.na(n), 1, n / sum(n, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "<b>", count_type, "</b><br><br>",
                  "Number of People: <br><b>",
                  formattable::comma(n, digits = 0), "</b><br><br>",
                  "Percentage of Prison Population: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         prop_label = paste0(round(prop*100, 0), "%"),
         new_label = paste0(
           "<b>", count_type, "</b><br><br>",
           prop_label
         ))

# Reorder the levels of count_type
ncrp_pe_type_2020$count_type <-
  factor(ncrp_pe_type_2020$count_type, levels
                         = c("Missing Data or Not<br>Eligible for Parole",
                             "Eligible for Parole<br>in 6+ Years",
                             "Eligible for Parole<br>in 1-5 Years",
                             "Currently Eligible<br>for Parole"))


# get list of states
states <- unique(ncrp_pe_type_2020$state)

all_stackedbar_pe_type_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
    filter(state == x)

  highcharts <- hchart(df1, "bar",
                       hcaes(x = state,
                             y = prop,
                             group = count_type),
                       # dataLabels = list(enabled = TRUE,
                       #                   format = "{point.new_label}",
                       #                   y = -140,
                       #                   style = list(fontWeight = "bold",
                       #                                fontFamily = "Graphik"))
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
    hc_legend(reversed = TRUE
              #enabled = FALSE
              ) %>%
    hc_colors(c("gray", yellow, purple, teal)) %>%
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

# Sentence about parole eligible prison population

################################################################################

# get list of states
states <- unique(ncrp_pe_type_2020$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_population <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
    filter(state == x &
           count_type == "Currently Eligible<br>for Parole")

  sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
                      " people who were eligible for parole but not released from prison, constituting ",
                      df1$prop_label, " of the parole-eligible prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_population <- setNames(all_sentence_parole_elgibility_population, states)






################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_line_pop_released_to_parole,           file=file.path(folder, "all_line_pop_released_to_parole.rds"))
  save(all_stackedbar_pe_type_2020,               file=file.path(folder, "all_stackedbar_pe_type_2020.rds"))
  save(all_sentence_parole_elgibility_population, file=file.path(folder, "all_sentence_parole_elgibility_population.rds"))

}
