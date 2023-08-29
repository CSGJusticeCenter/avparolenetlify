################################################################################

# Percentage/number of people who were released with conditions or without conditions

# Obtained from NCRP releases (ncrp_releases)

################################################################################

# Filter to people in prison for a new crime and with release type information
# Remove "other releases" - although Alabama has 30% other releases #######################?
release_type_2020 <- ncrp_releases_2020 %>%
  filter(admtype == "New court commitment") %>%
  filter(reltype == "Conditional release" | reltype == "Unconditional release") %>%
  group_by(state) %>%
  count(reltype) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0),
         chart_label =
           ifelse(reltype == "Conditional release", paste0("Conditional Release: <b>", prop_label, "</b>"),
                  paste0("Unconditional release: <b>", prop_label, "</b>"))
         ) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Release Type: <b>",
                  reltype,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# get list of states
states <- unique(release_type_2020$state)

# generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
all_pie_release_type_2020 <- map(.x = states,  .f = function(x) {

  df1 <- release_type_2020 %>%
    filter(state == x)

  df1$color <- case_when(df1$reltype == "Conditional release" ~ purple,
                         df1$reltype == "Unconditional release" ~ teal)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    df1 %>%
    hchart("pie",
           hcaes(x = reltype, y = prop, color = color),
           dataLabels = list(
             style = list(fontSize = "1em",
                          fontWeight = "regular",
                          alignTo = "connectors",
                          color = neutralBlackText),
             enabled = TRUE,
             format = paste("{point.chart_label}")
           )) %>%
    hc_chart(plotBackgroundColor = "none",
             plotBorderWidth = 0,
             plotShadow = FALSE,
             margin = c(100, 0, 18, 0)
    ) %>%
    hc_yAxis(maxPadding = 0) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = FALSE) %>%
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

all_pie_release_type_2020 <- setNames(all_pie_release_type_2020, states)
all_pie_release_type_2020$Georgia




################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_pie_release_type_2020, file=file.path(folder, "all_pie_release_type_2020.rds"))

}

