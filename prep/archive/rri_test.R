all_bar_rri_sentence_length_black_1 <- map(.x = states,  .f = function(x) {

  df1 <- all_census_ncrp_rri_prep %>%
    filter(state == x) %>%
    filter(race_eth == "Black, non-Hispanic") %>%
    filter(!is.na(rri)) %>%
    filter(!is.infinite(rri))

  min_value <- 0
  max_value_black <- max(all_census_ncrp_rri_prep %>%
                           filter(state == x) %>%
                           filter(race_eth == "Black, non-Hispanic") %>%
                           filter(!is.na(rri)) %>%
                           filter(!is.infinite(rri)) %>%
                           pull(rri),
                         na.rm = TRUE)

  max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                              filter(state == x) %>%
                              filter(race_eth == "Hispanic, any race") %>%
                              filter(!is.na(rri)) %>%
                              filter(!is.infinite(rri)) %>%
                              pull(rri),
                            na.rm = TRUE)

  # Determine the larger max value
  max_value <- max(max_value_black, max_value_hispanic)
  max_value <- ceiling(max_value) + 3

  # get y axis labels - option 2
  categories_list <- list()
  for (i in 0:max_value) {
    if (i == 1) {
      categories_list[[as.character(i)]] <- "1 = White<br>Reference Line"
    } else {
      categories_list[[as.character(i)]] <- " "
    }
  }

  highcharts <- df1 %>%
    hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
    hc_title(text = "Black People, Non-Hispanic") %>%
    hc_subtitle(text = "Relative Rate Index") %>%
    hc_xAxis(title = "",
             style = list(fontSize = "12px"),
             categories = c(
               "In Prison",
               "Sentence Length < 1 year",
               "Sentence Length 1-1.9 years",
               "Sentence Length 2-4.9 years",
               "Sentence Length 5-9.9 years",
               "Sentence Length 10-24.9 years",
               "Sentence Length >=25 years",
               "Sentence Length Life, LWOP, Death"
             )) %>%
    hc_yAxis(title = "",
             categories = categories_list,
             labels = list(rotation = 0,
                           step = 1),
             min = 0,
             max = max_value,
             plotBands = list(
               list(
                 color = "rgba(0, 0, 0, 0.2)",
                 from = 0,
                 to = 1))) %>%
    hc_legend(enabled = TRUE) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_plotOptions(series = list(stacking = "normal"),
                   bar = list(
                     dataLabels = list(enabled = TRUE,
                                       inside = TRUE,
                                       x = 290,
                                       # y = -1,
                                       useHTML = TRUE,
                                       format = "{point.rri_label}",
                                       style = list(fontWeight = "regular")
                                       )
                   )) %>%
    hc_chart(marginTop = 100, marginBottom = 80, spacingBottom = 80)

  unique_types <- unique(df1$type)

  if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff6400"))

  } else if ("Underrepresented" %in% unique_types && "Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray", "#ff640080"))

  } else if ("Overrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff6400"))

  } else if ("Underrepresented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("#ff640080"))

  } else if ("Equally Represented" %in% unique_types) {
    highcharts <- highcharts %>% hc_colors(colors = c("gray"))
  }

  return(highcharts)
})

all_bar_rri_sentence_length_black_1 <- setNames(all_bar_rri_sentence_length_black_1, states)
all_bar_rri_sentence_length_black_1$Georgia


