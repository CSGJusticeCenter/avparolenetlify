#######################################
# Project: AV Parole
# File: highchart.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Pre-generate highchart graphics for app
#######################################

# set options so that y axis has comma separator
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)




# # create sample data
# prison_data <- data.frame(
#   year = c(2000, 2005, 2010, 2015, 2020),
#   population = c(1400000, 1600000, 1800000, 2000000, 2200000),
#   admissions = c(80000, 90000, 100000, 110000, 120000)
# )
#
# # create chart using highcharter
# highchart() %>%
#   hc_chart(type = "line") %>%
#   hc_title(text = "Change in Prison Populations and Admissions Over Time") %>%
#   hc_xAxis(categories = prison_data$year) %>%
#   hc_yAxis_multiples(list(title = list(text = "Population")),
#                      list(title = list(text = "Admissions"), opposite = TRUE)) %>%
#   hc_series(list(name = "Population", data = prison_data$population),
#             list(name = "Admissions", data = prison_data$admissions, yAxis = 1))











########################################

# Donuts at top of page showing currently and future PED percentages

########################################

# # get list of states
# states <- unique(parole_eligibility_table_2020$state)
#
# all_donut_currently_eligible <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_table_2020 %>%
#     filter(state == x) %>%
#     select(state, current_perc) %>%
#     mutate(rest = 1 - current_perc) %>%
#     pivot_longer(cols      = c(current_perc:rest),
#                  names_to  = "type",
#                  values_to = "prop") %>%
#     mutate(tooltip =
#              case_when(type == "current_perc" ~
#                          paste0("<b>", state, "</b><br>",
#                                 "Percentage of People Eligible for Release:<br>",
#                                 paste(round(prop*100, 0), "%</b>", sep = ""), "<br>"),
#                        type == "rest" ~
#                          paste0("<b>", state, "</b><br>",
#                                 "Percentage of People Not Eligible for Release:<br>",
#                                 paste(round(prop*100, 0), "%</b>", sep = ""), "<br>")))
#
#   df2 <- df1 %>%
#     filter(type == "current_perc") %>%
#     mutate(chart_label = paste0(round(prop*100,0), "%"))
#
#   highcharts <- fnc_donut_chart(df = df1,
#                                 df_pct = df2,
#                                 x_variable = "state",
#                                 y_variable = "prop",
#                                 point_format = "{point.chart_label}",
#                                 accessibility_text = "TBD.")
#   return(highcharts)
# })
#
# all_donut_currently_eligible <- setNames(all_donut_currently_eligible, states)
#
# all_donut_future_eligible <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_table_2020 %>%
#     filter(state == x) %>%
#     select(state, future_perc) %>%
#     mutate(rest = 1 - future_perc) %>%
#     pivot_longer(cols      = c(future_perc:rest),
#                  names_to  = "type",
#                  values_to = "prop") %>%
#     mutate(tooltip =
#              case_when(type == "future_perc" ~
#                          paste0("<b>", state, "</b><br>",
#                                 "Percentage of People Eligible for Release:<br>",
#                                 paste(round(prop*100, 0), "%</b>", sep = ""), "<br>"),
#                        type == "rest" ~
#                          paste0("<b>", state, "</b><br>",
#                                 "Percentage of People Not Eligible for Release:<br>",
#                                 paste(round(prop*100, 0), "%</b>", sep = ""), "<br>")))
#
#   df2 <- df1 %>%
#     filter(type == "future_perc") %>%
#     mutate(chart_label = paste0(round(prop*100,0), "%"))
#
#   highcharts <- fnc_donut_chart(df = df1,
#                                 df_pct = df2,
#                                 x_variable = "state",
#                                 y_variable = "prop",
#                                 point_format = "{point.chart_label}",
#                                 accessibility_text = "TBD.")
#   return(highcharts)
# })
#
# all_donut_future_eligible <- setNames(all_donut_future_eligible, states)
#




########################################

# Parole eligibility rate by admtype

########################################



# Get list of states
states <- unique(parole_eligibility_rate_by_admtype$state)

# # How many people are being released at first eligibility?
# all_line_parole_eligibility_rate_by_admtype <- map(.x = states,  .f = function(x) {
#
#   df1 <- parole_eligibility_rate_by_admtype %>% filter(state == x) %>%
#     filter(parelig_status == "Current") %>%
#     select(rptyear, admtype, prop)
#
#   highcharts <-
#
#     highchart() %>%
#     hc_xAxis(categories = df1$rptyear,
#              labels = list(format = "{value}")) %>%
#     hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
#
#     hc_series(list(name = "New court commitment",
#                    data = df1 %>%
#                      filter(admtype == "New court commitment") %>%
#                      pull(prop)),
#               list(name = "Parole return/revocation",
#                    data = df1 %>%
#                      filter(admtype == "Parole return/revocation") %>%
#                      pull(prop))) %>%
#
#     hc_add_theme(hc_theme_jc) %>%
#     hc_colors(colors = c(teal, yellow)) %>%
#     hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
#
#     hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))
#
#
#   return(highcharts)
# })
#
# all_line_parole_eligibility_rate_by_admtype <- setNames(all_line_parole_eligibility_rate_by_admtype, states)
# all_line_parole_eligibility_rate_by_admtype$California

all_bar_parole_eligibility_rate_by_admtype <- map(.x = states, .f = function(x) {

  df1 <- parole_eligibility_rate_by_admtype %>%
    filter(state == x) %>%
    filter(parelig_status == "Current") %>%
    filter(rptyear >= 2010) %>%
    group_by(rptyear) %>%
    summarize(prop_new_court_commitment = sum(prop[admtype == "New court commitment"]),
              prop_parole_return = sum(prop[admtype == "Parole return/revocation"]))

  highcharts <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_xAxis(categories = df1$rptyear) %>%
    hc_yAxis(labels = list(format = "{value}%")) %>%
    hc_tooltip(formatter = JS("function() {
              var yValue = (this.point.y).toFixed(1);
              return '<b>' + this.series.name + '</b><br/>' +
                     this.point.category + ': ' + yValue + '%';
              }") ) %>%
    hc_plotOptions(series = list(stacking = "percent")) %>%
    hc_add_series(name = "New court commitment", data = df1$prop_new_court_commitment) %>%
    hc_add_series(name = "Parole return/revocation", data = df1$prop_parole_return,
                  dataLabels = list(enabled = TRUE,
                                    format = "{y:.1f}%",
                                    inside = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(colors = c(neutralBkgndMedium, teal))

})

all_bar_parole_eligibility_rate_by_admtype <- setNames(all_bar_parole_eligibility_rate_by_admtype, states)
















########################################

# Most serious sentenced offense for people eligible for parole but not yet released

########################################

# get list of states
states <- unique(current_ped_2020_offenses$state)

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

states <- unique(current_ped_2020_offenses$state)

all_bar_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_offenses %>%
    filter(state == x) %>%
    arrange(desc(prop))
  xaxis_order <- df1$offgeneral
  highcharts <-
    highchart() %>%
    hc_chart(margin = c(90, 0, 50, 0)) %>%
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
all_bar_parole_elgibility_offense$Georgia




########################################

# Released to Parole Over Time

########################################

# Get list of states
# all_ncrp_aps_pop_released_to_parole_by_year created in parole_findings_ncrp_aps.R
states <- unique(all_ncrp_aps_pop_released_to_parole_by_year$state)

# How many people are being released at first eligibility?
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
              # list(name = "Returned to Prison with Revocation",
              #      data = df1$incarcerated_from_parole),
              list(name = "Released from Prison to Parole",
                   data = df1$released_to_parole),
              list(name = "Parole Eligible but not Released from Prison",
                   data = df1$current_count)) %>%

    hc_add_theme(hc_theme_jc) %>%
    hc_colors(colors = c(teal, yellow, orange)) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))


  return(highcharts)
})

all_line_pop_released_to_parole <- setNames(all_line_pop_released_to_parole, states)









########################################

# Releases in 2020

# How many people are being released at first eligibility?
# How long after eligibility does release occur?
# How does release vary by the person's demographic and criminal history characteristics?
# What is the mean and median time between parole eligibility and release for those released after the PED, by maximum sentence length?

########################################

# How many people are being released at first eligibility?

# Get list of states
states <- unique(ncrp_released_at_ped_2020$state)

all_pie_released_at_ped_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_2020 %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_2020 <- setNames(all_pie_released_at_ped_2020, states)
all_pie_released_at_ped_2020$Georgia

all_bar_released_at_ped_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_2020 %>% filter(state == x) %>%
    mutate(released_at_ped_status =
             factor(released_at_ped_status,
                    levels = c("Released After Parole Eligibility Year",
                               "Released on Parole Eligibility Year",
                               "Released Before Parole Eligibility Year"
                               )))
  highcharts <-
    df1 %>%
    hchart(
      'bar',
      hcaes(x = 'state',
            y = 'prop',
            group = 'released_at_ped_status'),
      stacking = "percent",
      dataLabels = list(
        style = list(fontSize = "1.25em",
                     fontWeight = "bold",
                     color = neutralBlackText),
        enabled = TRUE,
        format = "{point.prop_label}")
    ) %>%
    hc_add_theme(hc_theme_jc_minimal) %>%
    hc_colors(c(yellow, teal, orange)) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "")) %>%
    #hc_legend(enabled = FALSE) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD")))
  return(highcharts)
})

all_bar_released_at_ped_2020 <- setNames(all_bar_released_at_ped_2020, states)









# What is the mean and median time between parole eligibility and release for
# those released after the PED, by maximum sentence length?

# Get list of states
states <- unique(ncrp_time_between_release_ped_2020$state)

all_time_between_release_ped_2020 <- map(.x = states, .f = function(x) {

  df1 <- ncrp_time_between_release_ped_2020 %>% filter(state == x)

  # Modify labels for "More than 10 years before PED" and "More than 10 years after PED"
  df1$time_between_release_ped_combined <-
    gsub("More than 10", "More than 10\n", df1$time_between_release_ped_combined)

  highcharts <- df1 %>%
    hchart(
      hcaes(x = time_between_release_ped_combined, y = n,
            color = ifelse(time_between_release_ped_combined %in% c("More than 10\n Years Before PED",
                                                                    "-5", "-4", "-3", "-2", "-1", "0"),
                           teal, orange)),
      type = "column",
    ) %>%
    hc_xAxis(
      title = list(text = "Years Between Parole Eligibility Year and Release Year"),
      labels = list(
        style = list(width = "100px"),
        formatter = JS("function() { return this.value.replace(/\\n/g, '<br/>'); }")
      )
    ) %>%
    hc_yAxis(title = list(text = "Number of People")) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(
      series = list(
        animation = FALSE,
        cursor = "pointer",
        borderWidth = 3,
        minPointLength = 4  # manually show bar bc of low values
      ),
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE),
        linkedDescription = "TBD",
        landmarkVerbosity = "one"
      ),
      area = list(accessibility = list(description = "TBD"))
    )

  return(highcharts)
})

all_time_between_release_ped_2020 <- setNames(all_time_between_release_ped_2020, states)






# Bar graph of proportion of population by demographic released year of PED

# Get list of states
states <- unique(ncrp_time_between_release_ped_2020_by_race$state)

all_time_between_release_ped_2020_by_race <- map(.x = states, .f = function(x) {
  df1 <- ncrp_time_between_release_ped_2020_by_race %>% filter(state == x)

  highcharts <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) %>%
    hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
    hc_add_series(data = subset(df1, time_between_release_ped_overall == "Released Before or on Year of PED"),
                  name = "Released Before or on Year of PED",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = race, y = prop * 100)) %>%
    hc_add_series(data = subset(df1, time_between_release_ped_overall == "Released After Year of PED"),
                  name = "Released After Year of PED",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = race, y = prop * 100)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(colors = c(teal, orange)) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD"))
    )

  return(highcharts)
})

all_time_between_release_ped_2020_by_race <- setNames(all_time_between_release_ped_2020_by_race, states)






# ncrp_sentences created in sentences_ncrp.R

# Get list of states
states <- unique(ncrp_sentences$state)

all_state_bar_prop_sentence_length <- map(.x = states, .f = function(x) {

  df1 <- ncrp_sentences %>%
    filter(state == x) %>%
    filter(rptyear == "2020")

  # define the desired order of the X-axis categories
  x_axis_order <- c(
    "< 1 year",
    "1-1.9 years",
    "2-4.9 years",
    "5-9.9 years",
    "10-24.9 years",
    ">=25 years",
    "Life, LWOP, Life plus additional years, Death"
  )

  # Modify labels for "More than 10 years before PED" and "More than 10 years after PED"
  df1$sentlgth <-
    gsub("Life, LWOP, Life plus additional years, Death",
         "Life, LWOP, Life plus\nadditional years, Death", df1$sentlgth)

  highcharts <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = x_axis_order,
             labels = list(
               style = list(width = "100px"),
               formatter = JS("function() { return this.value.replace(/\\n/g, '<br/>'); }")
             )) %>%
    hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
    hc_title(text = "Distribution of Sentence Lengths in Prison in 2020") %>%
    hc_add_series(data = df1,
                  name = "Released Before or on Year of PED",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = sentlgth, y = prop * 100,
                        color = ifelse(sentlgth %in% c("Life, LWOP, Life plus\nadditional years, Death"),
                                       red, purple))) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(animation = FALSE, cursor = "pointer", borderWidth = 3,
                                 minPointLength = 1),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD")))

  return(highcharts)
})

all_state_bar_prop_sentence_length <- setNames(all_state_bar_prop_sentence_length, states)









ncrp_sentlgth_timesrvd_rel



















##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  # save(all_donut_currently_eligible,         file=file.path(folder, "all_donut_currently_eligible.rds"))
  # save(all_donut_future_eligible,            file=file.path(folder, "all_donut_future_eligible.rds"))

  save(all_pie_parole_elgibility_offense,          file=file.path(folder, "all_pie_parole_elgibility_offense.rds"))
  save(all_bar_parole_elgibility_offense,          file=file.path(folder, "all_bar_parole_elgibility_offense.rds"))
  save(all_pie_released_at_ped_2020,               file=file.path(folder, "all_pie_released_at_ped_2020.rds"))
  save(all_bar_released_at_ped_2020,               file=file.path(folder, "all_bar_released_at_ped_2020.rds"))
  save(all_line_pop_released_to_parole,            file=file.path(folder, "all_line_pop_released_to_parole.rds"))
  save(all_bar_parole_eligibility_rate_by_admtype, file=file.path(folder, "all_bar_parole_eligibility_rate_by_admtype.rds"))

  save(all_time_between_release_ped_2020,          file=file.path(folder, "all_time_between_release_ped_2020.rds"))
  save(all_time_between_release_ped_2020_by_race,  file=file.path(folder, "all_time_between_release_ped_2020_by_race.rds"))

  save(all_state_bar_prop_sentence_length,         file=file.path(folder, "all_state_bar_prop_sentence_length.rds"))
}

