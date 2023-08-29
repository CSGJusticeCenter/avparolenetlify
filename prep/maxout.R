################################################################################

# Percentage/number of people who maxed out even though they were parole eligible

# Obtained from NCRP releases (ncrp_releases)

################################################################################

# Flag whether people maxed out and the type (i.e., Maxed out and Parole Eligible Before Release Year)
# Filter to people in prison for a new crime
# Remove rows where mand_prisrel_year, parelig_year, and relyr are NA
ncrp_releases_maxout_2020 <- ncrp_releases_2020 %>%
  filter(admtype == "New court commitment") %>%
  filter(!is.na(mand_prisrel_year) &
         !is.na(parelig_year) &
         !is.na(relyr) &
         !is.na(released_at_ped_status)) %>%
  mutate(maxout = case_when(
           mand_prisrel_year == relyr ~ "Released On/After Mandatory Release Year",
           mand_prisrel_year > relyr  ~ "Released Before Mandatory Release Year",
           mand_prisrel_year < relyr  ~ "Released On/After Mandatory Release Year" ################################## remove? Is it an error to be released after maxout year?
         ),

         maxout_type = paste0(maxout, " and ", released_at_ped_status)
  ) %>%
  select(mand_prisrel_year, parelig_year, relyr, maxout, maxout_type, everything())



# # Get number and proportion of people who maxed out and release date in relation to PE date
# releases_maxout_type_2020 <- ncrp_releases_maxout_2020 %>%
#   group_by(state) %>%
#   count(maxout, released_at_ped_status) %>%
#   mutate(prop = (n/sum(n))*100,
#          prop_label = paste0(round(prop, 0), "%"),
#          chart_label = paste0(maxout, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "<b>",
#                   maxout,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))

# Get number and proportion of people who maxed out
pe_releases_maxout_2020 <- ncrp_releases_maxout_2020 %>%
  group_by(state) %>%
  count(maxout) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label =
           ifelse(maxout == "Released On/After Mandatory Release Year", paste0("<b>Maxed Out</b><br>", maxout, " <b>", prop_label, "</b>"),
                  paste0("<b>Didn't Maxed Out</b><br>", maxout, " <b>", prop_label, "</b>"))
         ) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "<b>",
                  maxout,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))


# get list of states
states <- unique(pe_releases_maxout_2020$state)

# generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
all_pie_maxout_2020 <- map(.x = states,  .f = function(x) {

  df1 <- pe_releases_maxout_2020 %>%
    filter(state == x)

  df1$color <- case_when(df1$maxout == "Released Before Mandatory Release Year" ~ orange,
                         df1$maxout == "Released On/After Mandatory Release Year" ~ yellow)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    df1 %>%
    hchart("pie",
           hcaes(x = maxout, y = prop, color = color),
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

all_pie_maxout_2020 <- setNames(all_pie_maxout_2020, states)
all_pie_maxout_2020$Georgia


# # get list of states
# states <- unique(current_ped_2020_offenses_new_crime$state)
#
# # generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
# all_grouped_bar_maxout <- map(.x = states,  .f = function(x) {
#
#   df <- releases_maxout_type_2020 %>%
#     filter(state == "Georgia")
#
#   highcharts <- highchart() %>%
#     hc_chart(type = "column") %>%
#     hc_xAxis(categories = c("Maxed Out",
#                             "Released Before Max Out Year")) %>%
#     hc_yAxis(labels = list(format = "{value}%") #, min = 0, max = 100
#              ) %>%
#     hc_add_series(data = subset(df, released_at_ped_status == "Released Before Parole Eligibility Year"),
#                   name = "Released Before Parole Eligibility Year",
#                   type = "column",
#                   dataLabels = list(enabled = TRUE, format = "{prop_label}",
#                                     style = list(fontWeight = "regular")),
#                   hcaes(x = maxout, y = prop)) %>%
#     hc_add_series(data = subset(df, released_at_ped_status == "Released on Parole Eligibility Year"),
#                   name = "Released on Parole Eligibility Year",
#                   type = "column",
#                   dataLabels = list(enabled = TRUE, format = "{prop_label}",
#                                     style = list(fontWeight = "regular")),
#                   hcaes(x = maxout, y = prop)) %>%
#     hc_add_series(data = subset(df, released_at_ped_status == "Released After Parole Eligibility Year"),
#                   name = "Released After Parole Eligibility Year",
#                   type = "column",
#                   dataLabels = list(enabled = TRUE, format = "{prop_label}",
#                                     style = list(fontWeight = "regular")),
#                   hcaes(x = maxout, y = prop)) %>%
#     hc_add_theme(hc_theme_jc) %>%
#     hc_colors(colors = c(purple, teal, orange)) %>%
#     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#     hc_exporting(enabled = FALSE) %>%
#     hc_plotOptions(series = list(animation = FALSE,
#                                  cursor = "pointer",
#                                  borderWidth = 3,
#                                  minPointLength = 4),
#                    accessibility = list(enabled = TRUE,
#                                         keyboardNavigation = list(enabled = TRUE),
#                                         linkedDescription = "TBD",
#                                         landmarkVerbosity = "one"),
#                    area = list(accessibility = list(description = "TBD"))
#     )
#
#   return(highcharts)
# })
#
# all_grouped_bar_maxout <- setNames(all_grouped_bar_maxout, states)
#
# all_grouped_bar_maxout$Georgia















################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_pie_maxout_2020, file=file.path(folder, "all_pie_maxout_2020.rds"))

}



