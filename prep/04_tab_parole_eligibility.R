#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts

# Date last updated: August 24, 2023 (MAR)

# Description:
#    Parole eligibility tables and graphics for "Parole Eligibility" tab
#######################################


################################################################################

# Highchart - horizontal stacked bar chart
# Prison population by parole eligibility status

# Obtained from NCRP year end population

################################################################################

ncrp_prison_population_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020) %>%
  group_by(state) %>%
  summarise(yearendpop = n())

ncrp_pe_type_2020 <- parole_eligibility_table %>%
  filter(rptyear == 2020) %>%
  select(state,
         rptyear,
         current_count,
         future_1_5_years_count,
         future_6_years_count,
         missing_count
  )

# ncrp_pe_type_2020 <- parole_eligibility_table %>%
  # select(state,
  #        rptyear,
  #        current_count,
  #        future_1_5_years_count,
  #        future_6_years_count,
  #        missing_count
  # ) %>%
#   filter(rptyear == 2020) %>%
#   pivot_longer(cols = c(current_count,
#                         future_1_5_years_count,
#                         future_6_years_count,
#                         missing_count),
#                names_to = "count_type",
#                values_to = "n") %>%
#   mutate(count_type = case_when(
#     count_type == "current_count"          ~ "Currently Eligible<br>for Parole",
#     count_type == "future_1_5_years_count" ~ "Eligible for Parole<br>in 1-5 Years",
#     count_type == "future_6_years_count"   ~ "Eligible for Parole<br>in 6+ Years",
#     count_type == "missing_count"          ~ "Missing Data or Not<br>Eligible for Parole" # WILL NEED TO CHANGE FOR STATES THAT ABOLISHED PAROLE
#   )) %>%
#   group_by(state) %>%
#   mutate(prop = ifelse(sum(!is.na(n)) == 1 & !is.na(n), 1, n / sum(n, na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "<b>", count_type, "</b><br><br>",
#                   "Number of People: <br><b>",
#                   formattable::comma(n, digits = 0), "</b><br><br>",
#                   "Percentage of the Prison Population: <br><b>",
#                   paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
#          prop_label = paste0(round(prop*100, 0), "%"),
#          new_label = paste0(
#            "<b>", count_type, "</b><br><br>",
#            prop_label
#          ))

# Reorder the levels of count_type
ncrp_pe_type_2020$count_type <-
  factor(ncrp_pe_type_2020$count_type, levels
         = c("Missing Data or Not<br>Eligible for Parole",
             "Eligible for Parole<br>in 6+ Years",
             "Eligible for Parole<br>in 1-5 Years",
             "Currently Eligible<br>for Parole"))


# get list of states
states <- unique(ncrp_pe_type_2020$state)

all_stackedbar_pe_type_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
    filter(state == x)

  highcharts <- hchart(df1, "bar",
                       hcaes(x = state,
                             y = prop,
                             group = count_type),
                       dataLabels = list(enabled = TRUE,
                                         format = "{point.prop_label}",
                                         style = list(fontWeight = "bold",
                                                      fontSize = "16px",
                                                      fontFamily = "Graphik"))
  ) %>%
    hc_yAxis(labels = list(format = "{value}%",
                           enabled = FALSE),
             title = list(text = ""),
             min = 0, max = 1) %>%
    hc_xAxis(title = list(text = ""),
             labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(reversed = TRUE
              #enabled = FALSE
    ) %>%
    hc_colors(c("gray", yellow, purple, teal)) %>%
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

all_stackedbar_pe_type_2020 <- setNames(all_stackedbar_pe_type_2020, states)







################################################################################

# Sentence about parole eligible prison population

# Obtained from NCRP year end population

################################################################################

# get list of states
states <- unique(ncrp_pe_type_2020$state)

# generate sentence about most serious sentenced offense in 2020 by state
all_sentence_parole_elgibility_population <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pe_type_2020 %>%
    filter(state == x &
             count_type == "Currently Eligible<br>for Parole")

  sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
                      " people who qualified for parole but remained incarcerated for a new court commitment.
                      This group made up ", df1$prop_label, "% of the prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_population <- setNames(all_sentence_parole_elgibility_population, states)








################################################################################

# Highchart - bar chart
# Parole eligibility by race

# Obtained from NCRP year end population

################################################################################

# Currently parole eligible population but still in prison by race in 2020
# Only in for people in prison most recently for a new court commitment
current_ped_2020_race <- ncrp_yearendpop %>%
  filter(admtype == "New court commitment" &
           rptyear == 2020 &
           parelig_status == "Current" &
           !is.na(race)) %>%
  group_by(state) %>%
  count(race) %>%
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) %>%
  ungroup() %>%
  mutate(tooltip = paste0("<b>", state, " - ",
                          race, "</b><br>",
                          n_label, "<br>"))

# get states with data
states <- unique(current_ped_2020_race$state)

# generate bar chart showing parole eligible populations by race and state in 2020
all_bar_parole_elgibility_race <- map(.x = states,  .f = function(x) {

  # filter data
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(n))
  xaxis_order <- df1$race

  # assign color for each race
  df1$color <- case_when(df1$race == "Black, non-Hispanic" ~ yellow,
                         df1$race == "White, non-Hispanic" ~ orange,
                         df1$race == "Hispanic, any race" ~ teal,
                         df1$race == "Other race(s), non-Hispanic" ~ purple)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(race), y = n, color = color
                  ),
                  dataLabels = list(enabled = TRUE, format = "{point.n_label:,.0f}",
                                    style = list(fontWeight = "bold",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = xaxis_order) %>%
    hc_yAxis(labels = list(enabled = FALSE)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
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

all_bar_parole_elgibility_race <- setNames(all_bar_parole_elgibility_race, states)








################################################################################

# Sentence about parole eligibility and race

# Obtained from NCRP year end population

################################################################################

# get list of states
states <- unique(current_ped_2020_race$state)

# generate sentence about parole eligible populations by race and state in 2020
all_sentence_parole_elgibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_2020_race %>%
    filter(state == x) %>%
    arrange(desc(n)) %>%
    slice(1)
  sentences <- paste0("In 2020, ", df1$race,
                      " people constituted the most number of people eligible for parole but still in prison for a new court commitment, accounting for ",
                      df1$prop_label, " (", formattable::comma(df1$n, digits = 0), " people) of the parole-eligible prison population.")
  return(sentences)
})

all_sentence_parole_elgibility_race <- setNames(all_sentence_parole_elgibility_race, states)









################################################################################

# Save data

################################################################################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_stackedbar_pe_type_2020,               file=file.path(folder, "all_stackedbar_pe_type_2020.rds"))
  save(all_sentence_parole_elgibility_population, file=file.path(folder, "all_sentence_parole_elgibility_population.rds"))

  save(current_ped_2020_race,                  file=file.path(folder, "current_ped_2020_race.rds"))
  save(all_sentence_parole_elgibility_race,    file=file.path(folder, "all_sentence_parole_elgibility_race.rds"))
  save(all_bar_parole_elgibility_race,         file=file.path(folder, "all_bar_parole_elgibility_race.rds"))

}
