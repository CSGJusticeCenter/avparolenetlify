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
  highcharts1 <- highchart() %>%
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
        minPointLength = 4))

  all_races <- c("Black, non-Hispanic", "Unknown", "Other race(s), non-Hispanic",
                 "White, non-Hispanic", "Hispanic, any race")

  for (race in all_races) {
    if (nrow(df1 %>% filter(race == race)) > 0) {
      data_point <- df1 %>% filter(race == race) %>% select(prop, prop_label) %>%
        mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
        pull(data_point)

      highcharts <- highcharts %>%
        hc_add_series(name = race,
                      data = data_point,
                      color = switch(race,
                                     "Black, non-Hispanic" = teal,
                                     "Unknown" = darkblue,
                                     "Other race(s), non-Hispanic" = orange,
                                     "White, non-Hispanic" = yellow,
                                     "Hispanic, any race" = purple),
                      dataLabels = list(enabled = TRUE,
                                        style = list(fontSize = '1em'),
                                        formatter = JS("function() {return this.point.prop_label;}")))
    }
  }
  return(highcharts)
})
Note that I used a loop to iterate through all the race categories and conditionally added a series if data for that race category exists. This should handle cases where some race categories are missing.




User
not working, # Race
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
  highcharts1 <- highchart() %>%
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
        minPointLength = 4))

  all_races <- c("Black, non-Hispanic", "Unknown", "Other race(s), non-Hispanic",
                 "White, non-Hispanic", "Hispanic, any race")

  for (race in all_races) {
    if (nrow(df1 %>% filter(race == race)) > 0) {
      data_point <- df1 %>% filter(race == race) %>% select(prop, prop_label) %>%
        mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
        pull(data_point)

      highcharts2 <- highcharts1 %>%
        hc_add_series(name = race,
                      data = data_point,
                      color = switch(race,
                                     "Black, non-Hispanic" = teal,
                                     "Unknown" = darkblue,
                                     "Other race(s), non-Hispanic" = orange,
                                     "White, non-Hispanic" = yellow,
                                     "Hispanic, any race" = purple),
                      dataLabels = list(enabled = TRUE,
                                        style = list(fontSize = '1em'),
                                        formatter = JS("function() {return this.point.prop_label;}")))
    }
  }

  return(highcharts2)
})


all_stackedbar_parole_return_race_2020 <- setNames(all_stackedbar_parole_return_race_2020, states)
all_stackedbar_parole_return_race_2020$California
all_stackedbar_parole_return_race_2020$Georgia

all_stackedbar_parole_return_race_2020 <- map(.x = states, .f = function(x) {

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

  highcharts1 <- highchart() %>%
    # ... (the rest of your highchart initialization code remains the same)

  all_races <- c("Black, non-Hispanic", "Unknown", "Other race(s), non-Hispanic",
                 "White, non-Hispanic", "Hispanic, any race")

  for (race in all_races) {
    if (nrow(df1 %>% filter(race == race)) > 0) {
      data_point <- df1 %>% filter(race == race) %>% select(prop, prop_label) %>%
        mutate(data_point = map2(prop, prop_label, ~ list(y = .x, prop_label = .y))) %>%
        pull(data_point)

      highcharts1 <- highcharts1 %>%
        hc_add_series(name = race,
                      data = data_point,
                      color = switch(race,
                                     "Black, non-Hispanic" = "teal",
                                     "Unknown" = "darkblue",
                                     "Other race(s), non-Hispanic" = "orange",
                                     "White, non-Hispanic" = "yellow",
                                     "Hispanic, any race" = "purple"),
                      dataLabels = list(enabled = TRUE,
                                        style = list(fontSize = '1em'),
                                        formatter = JS("function() {return this.point.prop_label;}")))
    }
  }
  return(highcharts1)
})



