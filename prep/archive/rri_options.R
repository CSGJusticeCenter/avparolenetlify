
df1 <- all_census_ncrp_rri_prep %>%
  filter(state == "Georgia") %>%
  filter(race_eth == "Black, non-Hispanic") %>%
  filter(!is.na(rri)) %>%
  filter(!is.infinite(rri))

min_value <- 0
max_value_black <- max(all_census_ncrp_rri_prep %>%
                         filter(state == "Georgia") %>%
                         filter(race_eth == "Black, non-Hispanic") %>%
                         filter(!is.na(rri)) %>%
                         filter(!is.infinite(rri)) %>%
                         pull(rri),
                       na.rm = TRUE)

max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                            filter(state == "Georgia") %>%
                            filter(race_eth == "Hispanic, any race") %>%
                            filter(!is.na(rri)) %>%
                            filter(!is.infinite(rri)) %>%
                            pull(rri),
                          na.rm = TRUE)

max_value <- max(max_value_black, max_value_hispanic)
max_value <- ceiling(max_value)

categories_list <- list()
for (i in 0:max_value) {
  if (i == 1) {
    categories_list[[as.character(i)]] <- "Just as likely"
  } else if (i == 0) {
    categories_list[[as.character(i)]] <- "Less likely"
  } else {
    categories_list[[as.character(i)]] <- paste0(i, "x as likely")
  }
}


df2 <- df1 %>% filter(sample != "In Prison") %>%
  mutate(type = ifelse(type == "Overrepresented", "More Likely than White People", type))

highcharts <- df2 %>%
  hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
  hc_title(text = "Black People, Non-Hispanic") %>%
  hc_subtitle(text = "Relative Rate Index") %>%
  hc_xAxis(title = "Likelihood Compared to White People",
           categories = c(
             "Sentence Length < 1 year",
             "Sentence Length 1-1.9 years",
             "Sentence Length 2-4.9 years",
             "Sentence Length 5-9.9 years",
             "Sentence Length 10-24.9 years",
             "Sentence Length >=25 years",
             "Sentence Length Life, LWOP, Death"
           )) %>%
  hc_yAxis(title = list(
    text = "Likelihood Compared to White People",  # Add the y-axis title
    margin = 15,
    style = list(
      color = "black",
      fontSize = "12px",
      fontWeight = "bold"
    )
  ),
  categories = categories_list,
  labels = list(rotation = 0,
                step = 1),
  min = 0,
  max = max_value,
  plotBands = list(
    list(
      color = neutralBlackText,
      from = 1,
      to = 1))) %>%
  hc_legend(enabled = TRUE, marginBottom = 30) %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  hc_plotOptions(series = list(stacking = "normal"),
                 bar = list(
                   dataLabels = list(enabled = TRUE, format = "{point.rri}x", style = list(fontSize = "12px"))
                 )) %>%
  hc_chart(marginTop = 120, marginBottom = 80, spacingBottom = 80, marginRight = 100) %>%
  hc_annotations(list(labelOptions = list(backgroundColor = 'rgba(0, 0, 0, 0)',
                                          borderColor = "rgba(0,0,0,0)"),
                      labels = list(list(point = list(x = 0, y = 1, xAxis = 0.5, yAxis = 0),
                                         style = list(color = "black", fontSize = 8),
                                         useHTML = TRUE,
                                         text = paste("White Reference Line", sep = "")),
                                    list(point = list(x = -.1, y = 4.5, xAxis = 0, yAxis = 0),
                                         style = list(color = "black", fontSize = 8),
                                         useHTML = TRUE,
                                         text = paste("Black People are <b>2.3 times</b> more likely<br>to be in prison for an original sentence<br>length of < 1 year than White people", sep = "")
                                    ))))

highcharts













df1 <- all_census_ncrp_rri_prep %>%
  filter(state == "Georgia") %>%
  filter(race_eth == "Hispanic, any race") %>%
  filter(!is.na(rri)) %>%
  filter(!is.infinite(rri))

min_value <- 0
max_value_black <- max(all_census_ncrp_rri_prep %>%
                         filter(state == "Georgia") %>%
                         filter(race_eth == "Black, non-Hispanic") %>%
                         filter(!is.na(rri)) %>%
                         filter(!is.infinite(rri)) %>%
                         pull(rri),
                       na.rm = TRUE)

max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                            filter(state == "Georgia") %>%
                            filter(race_eth == "Hispanic, any race") %>%
                            filter(!is.na(rri)) %>%
                            filter(!is.infinite(rri)) %>%
                            pull(rri),
                          na.rm = TRUE)

max_value <- max(max_value_black, max_value_hispanic)
max_value <- ceiling(max_value)

categories_list <- list()
for (i in 0:max_value) {
  if (i == 1) {
    categories_list[[as.character(i)]] <- "Just as likely"
  } else if (i == 0) {
    categories_list[[as.character(i)]] <- "Less likely"
  } else {
    categories_list[[as.character(i)]] <- paste0(i, "x as likely")
  }
}


df2 <- df1 %>% filter(sample != "In Prison") %>%
  mutate(type = ifelse(type == "Underrepresented", "Less Likely than White People", type))


highcharts <- df2 %>%
  hchart(type = "bar", hcaes(x = "sample", y = "rri", group = "type")) %>%
  hc_title(text = "Hispanic People, Any Race") %>%
  hc_subtitle(text = "Relative Rate Index") %>%
  hc_xAxis(title = "Likelihood Compared to White People",
           categories = c(
             "Sentence Length < 1 year",
             "Sentence Length 1-1.9 years",
             "Sentence Length 2-4.9 years",
             "Sentence Length 5-9.9 years",
             "Sentence Length 10-24.9 years",
             "Sentence Length >=25 years",
             "Sentence Length Life, LWOP, Death"
           )) %>%
  hc_yAxis(title = list(
    text = "Likelihood Compared to White People",  # Add the y-axis title
    margin = 15,
    style = list(
      color = "black",
      fontSize = "12px",
      fontWeight = "bold"
    )
  ),
  categories = categories_list,
  labels = list(rotation = 0,
                step = 1),
  min = 0,
  max = max_value,
  plotBands = list(
    list(
      color = neutralBlackText,
      from = 1,
      to = 1))) %>%
  hc_legend(enabled = TRUE, marginBottom = 30) %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  hc_plotOptions(series = list(stacking = "normal"),
                 bar = list(
                   dataLabels = list(enabled = TRUE, format = "{point.rri}x", style = list(fontSize = "12px"))
                 )) %>%
  hc_chart(marginTop = 120, marginBottom = 80, spacingBottom = 80, marginRight = 100) %>%
  hc_annotations(list(labelOptions = list(backgroundColor = 'rgba(0, 0, 0, 0)',
                                          borderColor = "rgba(0,0,0,0)"),
                      labels = list(list(point = list(x = 0, y = 1, xAxis = 0.5, yAxis = 0),
                                         style = list(color = "black", fontSize = 8),
                                         useHTML = TRUE,
                                         text = paste("White Reference Line", sep = "")),
                                    list(point = list(x = -.1, y = 4.5, xAxis = 0, yAxis = 0),
                                         style = list(color = "black", fontSize = 8),
                                         useHTML = TRUE,
                                         text = paste("Hispanic People are <b>0.3 times</b><br>less likely, or 70% less likely,<br> to be in prison for an original<br>sentence length of < 1 year<br>than White people", sep = "")
                                    ))))

highcharts <- highcharts %>% hc_colors(colors = c("#ff640080"))
highcharts

