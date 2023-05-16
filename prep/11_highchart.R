#######################################
# Project: AV Parole
# File: highchart.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Pre-generate highchart graphics for shiny app
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

# get list of states
states <- unique(parole_eligibility_table_2020$state)

all_donut_currently_eligible <- map(.x = states,  .f = function(x) {

  df1 <- parole_eligibility_table_2020 %>%
    filter(state == x) %>%
    select(state, current_perc) %>%
    mutate(rest = 1 - current_perc) %>%
    pivot_longer(cols      = c(current_perc:rest),
                 names_to  = "type",
                 values_to = "prop") %>%
    mutate(tooltip =
             case_when(type == "current_perc" ~
                         paste0("<b>", state, "</b><br>",
                                "Percentage of People Eligible for Release:<br>",
                                paste(round(prop*100, 0), "%</b>", sep = ""), "<br>"),
                       type == "rest" ~
                         paste0("<b>", state, "</b><br>",
                                "Percentage of People Not Eligible for Release:<br>",
                                paste(round(prop*100, 0), "%</b>", sep = ""), "<br>")))

  df2 <- df1 %>%
    filter(type == "current_perc") %>%
    mutate(chart_label = paste0(round(prop*100,0), "%"))

  highcharts <- fnc_donut_chart(df = df1,
                                df_pct = df2,
                                x_variable = "state",
                                y_variable = "prop",
                                point_format = "{point.chart_label}",
                                accessibility_text = "TBD.")
  return(highcharts)
})

all_donut_currently_eligible <- setNames(all_donut_currently_eligible, states)

all_donut_future_eligible <- map(.x = states,  .f = function(x) {

  df1 <- parole_eligibility_table_2020 %>%
    filter(state == x) %>%
    select(state, future_perc) %>%
    mutate(rest = 1 - future_perc) %>%
    pivot_longer(cols      = c(future_perc:rest),
                 names_to  = "type",
                 values_to = "prop") %>%
    mutate(tooltip =
             case_when(type == "future_perc" ~
                         paste0("<b>", state, "</b><br>",
                                "Percentage of People Eligible for Release:<br>",
                                paste(round(prop*100, 0), "%</b>", sep = ""), "<br>"),
                       type == "rest" ~
                         paste0("<b>", state, "</b><br>",
                                "Percentage of People Not Eligible for Release:<br>",
                                paste(round(prop*100, 0), "%</b>", sep = ""), "<br>")))

  df2 <- df1 %>%
    filter(type == "future_perc") %>%
    mutate(chart_label = paste0(round(prop*100,0), "%"))

  highcharts <- fnc_donut_chart(df = df1,
                                df_pct = df2,
                                x_variable = "state",
                                y_variable = "prop",
                                point_format = "{point.chart_label}",
                                accessibility_text = "TBD.")
  return(highcharts)
})

all_donut_future_eligible <- setNames(all_donut_future_eligible, states)





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
              list(name = "Returned to Prison with Revocation",
                   data = df1$incarcerated_from_parole),
              list(name = "Released from Prison to Parole",
                   data = df1$released_to_parole),
              list(name = "Parole Eligible but not Released from Prison",
                   data = df1$current_count)) %>%

    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%

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
states <- unique(ncrp_released_at_ped$state)

all_pie_released_at_ped <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped <- setNames(all_pie_released_at_ped, states)





# How long after eligibility does release occur?

# Get list of states
states <- unique(ncrp_releases_clean$state)

data1 <- ncrp_releases_clean %>% filter(state == "Georgia") %>%
  select(names())

# Calculate time difference
time_diff <- data1$time_between_release_ped

# Create ordered time difference variable
time_diff_ordered <-
  factor(time_diff,
         levels = c(sort(unique(time_diff[time_diff < 0]),
                         decreasing = FALSE), 0,
                    sort(unique(time_diff[time_diff > 0]))))

# Calculate count and percentages
count <- table(time_diff_ordered)
percent <- count / sum(count) * 100

# Create Highchart
library(highcharter)

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Distribution of release years relative to parole eligibility") %>%
  hc_xAxis(categories = levels(time_diff_ordered), title = list(text = "Year(s) Delay")) %>%
  hc_add_series(name = "Percentage", data = percent) %>%
  hc_plotOptions(column = list(color = "#00aba0")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.y}%</b> releases with <b>{point.category} year(s)</b> delay") %>%
  hc_yAxis(title = list(text = "Percentage of all releases"), labels = list(format = "{value}%"))




all_bar_release_vs_ped <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped %>% filter(state == x)
  highcharts <-
    fnc_pct_bar_chart()
  return(highcharts)
})

all_bar_release_vs_ped <- setNames(all_bar_release_vs_ped, states)










##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_donut_currently_eligible,      file=file.path(folder, "all_donut_currently_eligible.rds"))
  save(all_donut_future_eligible,         file=file.path(folder, "all_donut_future_eligible.rds"))

  save(all_pie_parole_elgibility_offense, file=file.path(folder, "all_pie_parole_elgibility_offense.rds"))
  save(all_pie_released_at_ped,           file=file.path(folder, "all_pie_released_at_ped.rds"))
  save(all_line_pop_released_to_parole,   file=file.path(folder, "all_line_pop_released_to_parole.rds"))

}
