#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: September 26, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#    All figures and tables are for select year
#######################################

################################################################################

# Section: New crime vs Parole Revocations

# Prison population by admission type (new crime vs parole return)
# Obtained from NCRP year end population

################################################################################

##########
# Stacked single bar chart
##########

# Get number/prop of people by admission type and state
# Remove "other" admissions and NA's
# Use custom function to calculate n, prop and create prop_label and tooltip
ncrp_yearendpop_admtype <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" |
         admtype == "Parole return/revocation") %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state) %>%
  fnc_values_tooltip(admtype)

# Highchart showing prison pop by admission type
states <- unique(ncrp_yearendpop_admtype$state)
all_stackedbar_admtype <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_yearendpop_admtype %>%
    ungroup() %>%
    filter(state == x)

  highcharts <- hchart(df1, "bar",
                       hcaes(x = state,
                             y = prop,
                             group = admtype),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "bold",
                                                      fontSize = "12px",
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
all_stackedbar_admtype <- setNames(all_stackedbar_admtype, states)


##########
# Pie chart
##########

# Highchart showing prison pop by admission type
all_pie_admtype <- map(.x = states, .f = function(x) {

  df1 <- ncrp_yearendpop_admtype %>%
    ungroup() %>%
    filter(state == x) %>%
    select(admtype, prop, prop_label)

  highcharts <- hchart(df1, "pie",
                       hcaes(x = admtype, y = prop),
                       dataLabels = list(
                         style = list(fontSize = "1em",
                                      fontWeight = "regular",
                                      alignTo = "connectors",
                                      color = neutralBlackText),
                         enabled = TRUE,
                         format = paste("{point.admtype}: ", "<b>{point.prop_label}</b>"))) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(30, 0, 10, 0)) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_colors(c(teal, yellow)) %>%
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
all_pie_admtype <- setNames(all_pie_admtype, states)
all_pie_admtype$Georgia








################################################################################

# Section: Prison Population Trends

# Change in prison population, parole-eligible prison population, and people released to parole

# Obtained from NCRP year end population and APS Surveys from 2000-2018
# NEED TO CHANGE NCRP YEAR END POPULATION TO BJS CORRECTIONAL STATISTICS????????????

################################################################################

# Get state abb
state_names_abb <- data.frame(abbreviation = state.abb,
                              name = state.name,
                              stringsAsFactors = FALSE) %>%
  rename(state = name, stateid = abbreviation)

# List of data frames and years
aps_data_list <- list(da38058.0001,
                      da37471.0001,
                      da37441.0001,
                      da36619.0001,
                      da36320.0001,
                      da35629.0001,
                      da35257.0001,
                      da34718.0001,
                      da34382.0001,
                      da34381.0001,
                      da34380.0001,
                      da31332.0001,
                      da31331.0001,
                      da31330.0001,
                      da31329.0001,
                      da31328.0001,
                      da31327.0001,
                      da31326.0001,
                      da31325.0001)
aps_years <- 2018:2000
aps_pre_2008 <- rep(FALSE, 7) %>% c(rep(TRUE, 12))

# Process and combine APS data
aps_parole_combined <- lapply(seq_along(aps_data_list), function(i) {
  fnc_prepare_aps_data(aps_data_list[[i]], aps_years[i], aps_pre_2008[i])
})
aps_parole_2000_2018 <- do.call(rbind, aps_parole_combined)

# Remove DC
aps_parole_2000_2018 <- aps_parole_2000_2018 %>%
  filter(!state %in% c("District of Columbia", "Federal") & !is.na(state))

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
  mutate(prison_population_without_pe = coalesce(total_prison_population, 0) - coalesce(current_count, 0),
         prison_populations_same =
           ifelse(prison_population_without_pe == total_prison_population, TRUE, FALSE))

# Highchart of trend in prison population, people released from prison to parole, and parole eligible prison population from 2000-2020
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
              list(name = "Released from Prison to Parole",
                   data = df1$released_to_parole),
              list(name = "Parole Eligible but not Released from Prison (1-25 Year Sentences)",
                   data = df1$current_count)) %>%
    hc_add_theme(hc_theme_jc_line) %>%
    hc_colors(colors = c(teal, purple, yellow)) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
  return(highcharts)
})
all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)
all_line_pop_released_to_parole$Georgia







################################################################################

# Section: Who's in Prison?

# People in prison by race, age range, gender

# Obtained from NCRP year end population

################################################################################

##########
# Race
##########

# Get number/prop people by race and admission type
ncrp_yearendpop_race <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state, admtype) %>%
  fnc_values_tooltip(race)

# Highchart
states <- unique(ncrp_yearendpop_race$state)
all_stackedbar_prison_race <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_race %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_stackedbar_admtype_chart(df1, "race")
  highcharts <- highcharts %>% hc_chart(marginBottom = 45) %>%
  return(highcharts)
})
all_stackedbar_prison_race <- setNames(all_stackedbar_prison_race, states)
all_stackedbar_prison_race$Georgia


##########
# Age
##########

# Get number/prop people by ageyrend
ncrp_yearendpop_ageyrend <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state, admtype) %>%
  fnc_values_tooltip(ageyrend)

# Highchart
states <- unique(ncrp_yearendpop_ageyrend$state)
all_stackedbar_prison_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_ageyrend %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_stackedbar_admtype_chart(df1, "ageyrend")
  return(highcharts)
})
all_stackedbar_prison_ageyrend <- setNames(all_stackedbar_prison_ageyrend, states)
all_stackedbar_prison_ageyrend$Georgia


##########
# Gender
##########

# Get number/prop people by gender
ncrp_yearendpop_gender <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state, admtype) %>%
  fnc_values_tooltip(sex)

# Highchart
states <- unique(ncrp_yearendpop_gender$state)
all_stackedbar_prison_gender <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_gender %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_stackedbar_admtype_chart(df1, "sex")
  highcharts <- highcharts %>% hc_colors(c(yellow, teal))
  return(highcharts)
})

all_stackedbar_prison_gender <- setNames(all_stackedbar_prison_gender, states)
all_stackedbar_prison_gender$Georgia







################################################################################

# Section: Offense Types

# People in prison by most serious sentenced offense

# Obtained from NCRP year end population

################################################################################

##########
# FBI Index
##########

# Get number/prop people by fbi_index
ncrp_yearendpop_fbi_index <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state, admtype) %>%
  fnc_values_tooltip(fbi_index)

# Highchart
states <- unique(ncrp_yearendpop_fbi_index$state)
all_groupedbar_prison_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_fbi_index %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- hchart(df1, "bar",
                       hcaes(x = fbi_index,
                             y = prop,
                             group = admtype
                       ),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(c(teal, yellow, purple)) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_chart(events = list(
      load = JS("function() {
                var series = this.series;
                for (var i = 0; i < series.length; i++) {
                    if (series[i].name === 'Other or Unknown') {
                        series[i].setVisible(false, false);
                    }
                }
                this.redraw();
              }"))) %>%
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
all_groupedbar_prison_fbi_index <- setNames(all_groupedbar_prison_fbi_index, states)
all_groupedbar_prison_fbi_index$Georgia


##########
# FBI INDEX - not grouped
##########

# Get number/prop people by fbi_index
ncrp_yearendpop_fbi_index <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state) %>%
  fnc_values_tooltip(fbi_index)

# Highchart
states <- unique(ncrp_yearendpop_fbi_index$state)
all_bar_prison_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_fbi_index %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- hchart(df1, "bar",
                       hcaes(x = fbi_index,
                             y = prop
                       ),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
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
all_bar_prison_fbi_index <- setNames(all_bar_prison_fbi_index, states)
all_bar_prison_fbi_index$Georgia








################################################################################

# Section: Sentence Length

# People in prison by sentence length

# Obtained from NCRP year end population

################################################################################

# Get number/prop people by sentlgth
ncrp_yearendpop_sentlgth <-  ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state, admtype) %>%
  fnc_values_tooltip(sentlgth)

# Highchart
states <- unique(ncrp_yearendpop_sentlgth$state)
all_groupedbar_prison_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_sentlgth %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- hchart(df1, "bar",
                       hcaes(x = sentlgth,
                             y = prop,
                             group = admtype
                       ),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
    hc_legend(enabled = TRUE,
              reversed = FALSE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(c(teal, yellow, purple)) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_chart(events = list(
      load = JS("function() {
                var series = this.series;
                for (var i = 0; i < series.length; i++) {
                    if (series[i].name === 'Other or Unknown') {
                        series[i].setVisible(false, false);
                    }
                }
                this.redraw();
              }")
    )) %>%
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

all_groupedbar_prison_sentlgth <- setNames(all_groupedbar_prison_sentlgth, states)
all_groupedbar_prison_sentlgth$Georgia


##########
# SENTENCE LENGTH - not grouped
##########

# Get number/prop people by sentlgth
ncrp_yearendpop_sentlgth <- ncrp_yearendpop %>%
  filter(rptyear == select_year) %>% ####### FILTER YEAR
  group_by(state) %>%
  fnc_values_tooltip(sentlgth)

# Highchart
states <- unique(ncrp_yearendpop_sentlgth$state)
all_bar_prison_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_sentlgth %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- hchart(df1, "bar",
                       hcaes(x = sentlgth,
                             y = prop
                       ),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "regular",
                                                      fontSize = "12px",
                                                      fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
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
all_bar_prison_sentlgth <- setNames(all_bar_prison_sentlgth, states)
all_bar_prison_sentlgth$Georgia









################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(all_stackedbar_admtype,          file = file.path(folder, "all_stackedbar_admtype.rds"))
  save(all_pie_admtype,                 file = file.path(folder, "all_pie_admtype.rds"))

  save(all_line_pop_released_to_parole, file = file.path(folder, "all_line_pop_released_to_parole.rds"))

  save(all_stackedbar_prison_race,      file = file.path(folder, "all_stackedbar_prison_race.rds"))
  save(all_stackedbar_prison_gender,    file = file.path(folder, "all_stackedbar_prison_gender.rds"))
  save(all_stackedbar_prison_ageyrend,  file = file.path(folder, "all_stackedbar_prison_ageyrend.rds"))

  save(all_groupedbar_prison_sentlgth,  file = file.path(folder, "all_groupedbar_prison_sentlgth.rds"))
  save(all_groupedbar_prison_fbi_index, file = file.path(folder, "all_groupedbar_prison_fbi_index.rds"))
  save(all_bar_prison_sentlgth,         file = file.path(folder, "all_bar_prison_sentlgth.rds"))
  save(all_bar_prison_fbi_index,        file = file.path(folder, "all_bar_prison_fbi_index.rds"))

}

