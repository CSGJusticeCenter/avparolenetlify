#######################################
# Project: AV Parole
# File: tab_prison_population.R
# Authors: Mari Roberts
# Date last updated: September 26, 2023 (MAR)
# Description:
#    Prison population and graphics for shiny app
#######################################

# Function to generate grouped data for stacked bar chart
fnc_generate_grouped_adm_data <- function(df, year, group_by_col) {
  df %>%
    filter(rptyear == year) %>%
    group_by(state, admtype) %>%
    count(!!sym(group_by_col)) %>%
    mutate(
      prop = (n / sum(n)) * 100,
      prop_label = paste0(round(prop, 0), "%"),
      n_label = formattable::comma(n, 0),
      tooltip = paste0("<b>", state, "</b><br><br>",
                       group_by_col, ": <b>", !!sym(group_by_col),
                       "</b><br><br>",
                       "Percentage of People: <b>", prop_label, "</b><br>",
                       "Number of People: <b>", formattable::comma(n, digits = 0), "</b>",
                       sep = "")
    )
}

# Function to create stacked bar
fnc_generate_horzstackedbar_admtype_chart <- function(df, group_by_col) {
  hchart(df, "bar",
         hcaes(x = admtype,
               y = prop,
               group = !!sym(group_by_col)
               ),
         dataLabels = list(enabled = TRUE,
                           format = "{point.prop_label}",
                           style = list(fontWeight = "bold",
                                        fontSize = "12px",
                                        fontFamily = "Graphik"))) %>%
    hc_yAxis(labels = list(enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 100) %>%
    hc_xAxis(categories = c("New court commitment",
                            "Parole return/revocation",
                            "Other or Unknown"),
             title = list(text = ""),
             labels = list(enabled = TRUE)) %>%
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

# Assign state names
all_stackedbar_admtype_2020 <- setNames(all_stackedbar_admtype_2020, states)
all_stackedbar_admtype_2020$Georgia
all_stackedbar_admtype_2020$California

# PIE CHART - Generate highchart for each state showing prison pop by admission type
all_pie_admtype_2020 <- map(.x = states, .f = function(x) {

  df1 <- ncrp_yearendpop_admtype_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    select(admtype, prop) %>%
    mutate(prop_label = paste0(round(prop, 0), "%"))

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
             margin = c(100, 0, 18, 0)) %>%
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

# Assign state names
all_pie_admtype_2020 <- setNames(all_pie_admtype_2020, states)
all_pie_admtype_2020$Georgia
all_pie_admtype_2020$California








################################################################################

# Section Prison Population Trends

# Highchart - Trend line graph
# Line graph data showing the change in prison population
#     and change in people released to parole

# Obtained from NCRP year end population and APS Surveys from 2000-2018
# NEED TO CHANGE NCRP YEAR END POPULATION TO BJS CORRECTIONAL STATISTICS????????????

################################################################################

# Create a function to prepare APS data
fnc_prepare_aps_data <- function(data, year, pre_2008 = FALSE) {
  data <- data %>%
    clean_names() %>%
    mutate(rptyear = year)

  if (pre_2008) {
    data <- data %>%
      select(-stateid) %>%
      rename(stateid = state) %>%
      mutate(stateid = str_trim(stateid)) %>%
      left_join(state_names_abb, by = "stateid") %>%
      fnc_aps_prepare_pre2008()
  } else {
    data <- data %>%
      mutate(state = str_sub(stateid, 6, -1)) %>%
      fnc_aps_prepare()
  }
  return(data)
}

# Get state abb
state_names_abb <- data.frame(abbreviation = state.abb, name = state.name, stringsAsFactors = FALSE) %>%
  rename(state = name, stateid = abbreviation)

# List of data frames and years
aps_data_list <- list(da38058.0001, da37471.0001, da37441.0001, da36619.0001, da36320.0001, da35629.0001, da35257.0001, da34718.0001, da34382.0001, da34381.0001, da34380.0001, da31332.0001, da31331.0001, da31330.0001, da31329.0001, da31328.0001, da31327.0001, da31326.0001, da31325.0001)
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

# Assign state names
all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)
all_line_pop_released_to_parole$Georgia
all_line_pop_released_to_parole$California




################################################################################

# Section: Who's in Prison?

# Highchart - People in prison by race, age range, gender, sentence length, offenses
# Obtained from NCRP year end population

################################################################################

##########
# RACE
##########

# Get number/prop people by race
ncrp_yearendpop_race_2020 <-
  fnc_generate_grouped_adm_data(ncrp_yearendpop, 2020, "race")

# List of states
states <- unique(ncrp_yearendpop_race_2020$state)

all_stackedbar_prison_race_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_race_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_generate_horzstackedbar_admtype_chart(df1, "race")
  return(highcharts)
})

all_stackedbar_prison_race_2020 <- setNames(all_stackedbar_prison_race_2020, states)
all_stackedbar_prison_race_2020$Georgia

##########
# AGE
##########

# Get number/prop people by ageyrend
ncrp_yearendpop_ageyrend_2020 <-
  fnc_generate_grouped_adm_data(ncrp_yearendpop, 2020, "ageyrend")

# List of states
states <- unique(ncrp_yearendpop_ageyrend_2020$state)

all_stackedbar_prison_ageyrend_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_ageyrend_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_generate_horzstackedbar_admtype_chart(df1, "ageyrend")
  return(highcharts)
})

all_stackedbar_prison_ageyrend_2020 <- setNames(all_stackedbar_prison_ageyrend_2020, states)
all_stackedbar_prison_ageyrend_2020$Georgia

##########
# gender
##########

# Get number/prop people by gender
ncrp_yearendpop_gender_2020 <-
  fnc_generate_grouped_adm_data(ncrp_yearendpop, 2020, "sex")

# List of states
states <- unique(ncrp_yearendpop_gender_2020$state)

all_stackedbar_prison_gender_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_gender_2020 %>%
    ungroup() %>%
    filter(state == x) %>%
    distinct()
  highcharts <- fnc_generate_horzstackedbar_admtype_chart(df1, "sex")
  highcharts <- highcharts %>% hc_colors(c(yellow, teal))
  return(highcharts)
})

all_stackedbar_prison_gender_2020 <- setNames(all_stackedbar_prison_gender_2020, states)
all_stackedbar_prison_gender_2020$Georgia

##########
# FBI INDEX
##########

# Get number/prop people by fbi_index
ncrp_yearendpop_fbi_index_2020 <-
  fnc_generate_grouped_adm_data(ncrp_yearendpop, 2020, "fbi_index")

# List of states
states <- unique(ncrp_yearendpop_fbi_index_2020$state)

all_groupedbar_prison_fbi_index_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_fbi_index_2020 %>%
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

all_groupedbar_prison_fbi_index_2020 <- setNames(all_groupedbar_prison_fbi_index_2020, states)
all_groupedbar_prison_fbi_index_2020$Georgia

##########
# FBI INDEX - not grouped
##########

# Get number/prop people by fbi_index
ncrp_yearendpop_fbi_index_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  group_by(state) %>%
  count(fbi_index) %>%
  mutate(
    prop = (n / sum(n)) * 100,
    prop_label = paste0(round(prop, 0), "%"),
    n_label = formattable::comma(n, 0),
    tooltip = paste0("<b>", state, "</b><br><br>",
                     "<b>Criminal Offense: ", fbi_index,
                     "</b><br><br>",
                     "Percentage of People: <b>", prop_label, "</b><br>",
                     "Number of People: <b>", formattable::comma(n, digits = 0), "</b>",
                     sep = "")
  )

# List of states
states <- unique(ncrp_yearendpop_fbi_index_2020$state)

all_bar_prison_fbi_index_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_fbi_index_2020 %>%
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

all_bar_prison_fbi_index_2020 <- setNames(all_bar_prison_fbi_index_2020, states)
all_bar_prison_fbi_index_2020$Georgia

##########
# SENTENCE LENGTH
##########

# Get number/prop people by sentlgth
ncrp_yearendpop_sentlgth_2020 <-
  fnc_generate_grouped_adm_data(ncrp_yearendpop, 2020, "sentlgth")

# List of states
states <- unique(ncrp_yearendpop_sentlgth_2020$state)

all_groupedbar_prison_sentlgth_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_sentlgth_2020 %>%
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

all_groupedbar_prison_sentlgth_2020 <- setNames(all_groupedbar_prison_sentlgth_2020, states)
all_groupedbar_prison_sentlgth_2020$Georgia

##########
# SENTENCE LENGTH - not grouped
##########

# Get number/prop people by sentlgth
ncrp_yearendpop_sentlgth_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  group_by(state) %>%
  count(sentlgth) %>%
  mutate(
    prop = (n / sum(n)) * 100,
    prop_label = paste0(round(prop, 0), "%"),
    n_label = formattable::comma(n, 0),
    tooltip = paste0("<b>", state, "</b><br><br>",
                     "<b>Sentence Length: ", sentlgth,
                     "</b><br><br>",
                     "Percentage of People: <b>", prop_label, "</b><br>",
                     "Number of People: <b>", formattable::comma(n, digits = 0), "</b>",
                     sep = "")
  )

# List of states
states <- unique(ncrp_yearendpop_sentlgth_2020$state)

all_bar_prison_sentlgth_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_yearendpop_sentlgth_2020 %>%
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

all_bar_prison_sentlgth_2020 <- setNames(all_bar_prison_sentlgth_2020, states)
all_bar_prison_sentlgth_2020$Georgia









################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_stackedbar_admtype_2020,          file = file.path(folder, "all_stackedbar_admtype_2020.rds"))
  save(all_pie_admtype_2020,                 file = file.path(folder, "all_pie_admtype_2020.rds"))

  save(all_line_pop_released_to_parole,      file = file.path(folder, "all_line_pop_released_to_parole.rds"))

  save(all_stackedbar_prison_race_2020,      file = file.path(folder, "all_stackedbar_prison_race_2020.rds"))
  save(all_stackedbar_prison_gender_2020,       file = file.path(folder, "all_stackedbar_prison_gender_2020.rds"))
  save(all_stackedbar_prison_ageyrend_2020,  file = file.path(folder, "all_stackedbar_prison_ageyrend_2020.rds"))

  save(all_groupedbar_prison_sentlgth_2020,  file = file.path(folder, "all_groupedbar_prison_sentlgth_2020.rds"))
  save(all_groupedbar_prison_fbi_index_2020, file = file.path(folder, "all_groupedbar_prison_fbi_index_2020.rds"))
  save(all_bar_prison_sentlgth_2020,         file = file.path(folder, "all_bar_prison_sentlgth_2020.rds"))
  save(all_bar_prison_fbi_index_2020,        file = file.path(folder, "all_bar_prison_fbi_index_2020.rds"))

}
