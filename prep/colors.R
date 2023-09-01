df1 <- ncrp_yearendpop_parole_return_race_2020 %>%
  filter(state == "Georgia") %>%
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "Unknown",
                                  "Other race(s), non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race"
                       ))) %>%
  select(state, race, prop, prop_label)

highchart() %>%
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
  hc_add_series(name = "Black, non-Hispanic",
                data = new_data,
                color = "teal",
                dataLabels = list(enabled = TRUE,formatter = JS("function() {return this.point.prop_label;}"))

























