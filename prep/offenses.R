# #######################################
# # Project: AV Parole
# # File: tab_offenses.R
# # Authors: Mari Roberts
# # Date last updated: July 17, 2023 (MAR)
# # Description:
# #    LOS tables and graphics for app
# #######################################
#
# ################################################################################
#
# # Most serious sentenced offenses for those in prison but not released in 2020
#
# # Obtained from NCRP year end population
#
# ################################################################################
#
# # most serious sentenced offense for people eligible for parole but still in prison
# current_ped_2020_offenses_all <- ncrp_yearendpop %>%
#   filter(rptyear == 2020) %>%
#   filter(parelig_status == "Current") %>%
#   filter(!is.na(offgeneral)) %>%
#   mutate(offgeneral = ifelse(
#     offgeneral == "Other/unspecified", "Other or Unspecified", offgeneral))
#
# # count by state
# # create tooltip
# current_ped_2020_offenses <- current_ped_2020_offenses_all %>%
#   group_by(state) %>%
#   count(offgeneral) %>%
#   mutate(
#     prop = n/sum(n)
#     , yearendpop_ped = sum(n)
#   ) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
#                   "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
#                   scales::comma(n), "</b><br><br>",
#                   "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
#                   paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
#          chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
#          prop_label = paste0(round(prop*100, 0), "%"))
#
# # count by state for admission types that are parole revocations or returns only
# current_ped_2020_offenses_parole_return <- current_ped_2020_offenses_all %>%
#   filter(admtype == "Parole return/revocation") %>%
#   group_by(state) %>%
#   count(offgeneral) %>%
#   mutate(
#     prop = n/sum(n)
#     , yearendpop_ped = sum(n)
#   ) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
#                   "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
#                   scales::comma(n), "</b><br><br>",
#                   "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
#                   paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
#          chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
#          prop_label = paste0(round(prop*100, 0), "%"))
#
# # count by state for admission types that are new crimes only
# current_ped_2020_offenses_new_crime <- current_ped_2020_offenses_all %>%
#   filter(admtype == "New court commitment") %>%
#   group_by(state) %>%
#   count(offgeneral) %>%
#   mutate(
#     prop = n/sum(n)
#     , yearendpop_ped = sum(n)
#   ) %>%
#   ungroup() %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Most Serious Sentence Offense: <b>", offgeneral, "</b><br><br>",
#                   "Number of People with Parole<br>Eligibility but not yet Released: <br><b>",
#                   scales::comma(n), "</b><br><br>",
#                   "Percentage of Prison Population with Parole<br>Eligibility but not yet Released: <br><b>",
#                   paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
#          chart_label = paste0(offgeneral, " <b>", round(prop*100, 0), "%</b>"),
#          prop_label = paste0(round(prop*100, 0), "%"))
#
#
#
#
#
#
# ################################################################################
#
# # Sentence about most serious offense
#
# # Obtained from NCRP year end population
#
# ################################################################################
#
# # get list of states with data
# states <- unique(current_ped_2020_offenses$state)
#
# # generate sentence about most serious sentenced offense in 2020 by state
# all_sentence_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
#   df1 <- current_ped_2020_offenses %>%
#     filter(state == x) %>%
#     filter(offgeneral != "Violent") %>%
#     group_by() %>%
#     summarise(n = sum(n, na.rm = TRUE),
#               prop = sum(prop, na.rm = TRUE)) %>%
#     mutate(prop = round(prop*100, 0),
#            prop_label = paste0(prop, "%"))
#
#   sentences <- paste0("In 2020, there were ", formattable::comma(df1$n, digits = 0),
#                       " people who were parole eligible but still in prison for non-violent offenses, accounting for ",
#                       df1$prop_label, " of the parole-eligible prison population.")
#   return(sentences)
# })
#
# all_sentence_parole_elgibility_offense <- setNames(all_sentence_parole_elgibility_offense, states)
#
#
#
#
#
#
# ################################################################################
#
# # Pie chart about most serious offense
#
# # Obtained from NCRP year end population
#
# ################################################################################
#
# # get list of states
# states <- unique(current_ped_2020_offenses$state)
#
# # generate pie chart about most serious sentenced offense in 2020 by state
# all_pie_parole_elgibility_offense <- map(.x = states,  .f = function(x) {
#   df1 <- current_ped_2020_offenses %>% filter(state == x)
#   highcharts <- fnc_pie_chart(df = df1,
#                               x_variable = "offgeneral",
#                               y_variable = "prop",
#                               point_format = "{point.chart_label}",
#                               accessibility_text = "TBD.")
#   return(highcharts)
# })
#
# all_pie_parole_elgibility_offense <- setNames(all_pie_parole_elgibility_offense, states)
#
# # get list of states
# states <- unique(current_ped_2020_offenses_new_crime$state)
#
# # generate pie chart about most serious sentenced offense in 2020 by state - NEW CRIME ONLY
# all_pie_parole_elgibility_offense_new_crime <- map(.x = states,  .f = function(x) {
#   df1 <- current_ped_2020_offenses_new_crime %>% filter(state == x)
#   highcharts <- fnc_pie_chart(df = df1,
#                               x_variable = "offgeneral",
#                               y_variable = "prop",
#                               point_format = "{point.chart_label}",
#                               accessibility_text = "TBD.")
#   return(highcharts)
# })
#
# all_pie_parole_elgibility_offense_new_crime <- setNames(all_pie_parole_elgibility_offense_new_crime, states)
#
#
#
#
#
#
#
#
# ################################################################################
#
# # Save data
#
# ################################################################################
#
# theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))
#
# for (folder in theseFOLDERS){
#
#   save(current_ped_2020_offenses,
#        file=file.path(folder, "current_ped_2020_offenses.rds"))
#   save(all_pie_parole_elgibility_offense_new_crime,
#        file=file.path(folder, "all_pie_parole_elgibility_offense_new_crime.rds"))
#   save(all_sentence_parole_elgibility_offense,
#        file=file.path(folder, "all_sentence_parole_elgibility_offense.rds"))
#
# }
