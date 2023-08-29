
################################################################################

# Relative Rate Index

# Obtained from NCRP year end population

################################################################################

##########################
# Get census data
##########################

# Weighted estimate of percentage of race from 2020 census
# Pulled estimated counts and construct percent estimate
# These are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P3_003N",
               estimate_black              = "P3_004N",
               estimate_asian              = "P3_006N",
               estimate_native_hawaiian_pi = "P3_007N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P1_005N")

# Get list of states
states <- state.name

# Define  function to retrieve and process census data for a given state
fnc_get_census_data <- function(state) {
  census_race_data <- tidycensus::get_decennial(
    geography = "state",
    state = state,
    variables = race_vars,
    summary_var = "P3_001N",
    year = 2020,
    geometry = FALSE
  ) %>%
    clean_names() %>%
    select(-geoid) %>%
    mutate(
      race_eth = case_when(
        variable %in% c("estimate_american_indian", "estimate_asian", "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
        variable == "estimate_black" ~ "Black, non-Hispanic",
        variable == "estimate_hispanic" ~ "Hispanic, any race",
        variable == "estimate_white" ~ "White, non-Hispanic",
        TRUE ~ "NA"
      )
    ) %>%
    filter(race_eth != "Other race(s), non-Hispanic") %>%
    group_by(race_eth) %>%
    summarise(race_eth_pop = sum(value)) %>%
    ungroup() %>%
    mutate(total_pop = sum(race_eth_pop)
           # estimate = (race_eth_pop / total_pop) * 100
    )

  return(census_race_data)
}

# Use lapply to retrieve and process data for each state
census_data_list <- lapply(states, fnc_get_census_data)

# Convert the list of tibbles into a dataframe
census_data_df <- bind_rows(census_data_list)

# Add the "state" column to the final dataframe
census_data_df$state <- rep(states, each = nrow(census_data_df) / length(states))




################### TESTING

# Filter to state
census_race_2020 <- census_data_df %>%
  filter(state == "Georgia") %>%
  ungroup() %>%
  dplyr::select(race_eth, race_eth_pop)

# Filter to rptyear 2020 and to state
ncrp_race_2020 <- ncrp_yearendpop %>%
  filter(rptyear == 2020 &
           state == "Georgia" &
           race != "Other race(s), non-Hispanic") %>%
  mutate(unique_id = row_number()) %>%
  rename(race_eth = race) %>%
  ungroup() %>%
  distinct(unique_id, .keep_all = TRUE)

# Join two files
ncrp_census_prelim_rri <- left_join(ncrp_race_2020, census_race_2020, by = "race_eth")

# white, non-hispanic only for rri calculation
rri_analytic_white <- ncrp_census_prelim_rri %>%
  filter(!is.na(race_eth) == TRUE,
         race_eth == "White, non-Hispanic") %>%
  summarise(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
            people_in_prison_rri                 = NA,

            sentence_length_1_year_per10kadult = round(n_distinct(unique_id[sentlgth == "< 1 year"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_1_year_rri         = NA,

            sentence_length_1_1_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "1-1.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_1_1_9_years_rri         = NA,

            sentence_length_2_4_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_2_4_9_years_rri         = NA,

            sentence_length_5_9_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_5_9_9_years_rri         = NA,

            sentence_length_10_24_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "10-24.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_10_24_9_years_rri         = NA,

            sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_25_years_rri         = NA,

            sentence_length_lwop_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_lwop_rri         = NA
  ) %>%
  dplyr::mutate(race_eth = "White, non-Hispanic")

# now calculate rates and RRIs using above dataframe (for White individuals) as reference
rri_analytic <- ncrp_census_prelim_rri %>%
  dplyr::filter(!is.na(race_eth) == TRUE,
                race_eth %in% c("Black, non-Hispanic","Hispanic, any race")) %>%
  group_by(race_eth) %>%
  summarise(people_in_prison_per10kadult         = round(n_distinct(unique_id)/mean(race_eth_pop)*10000, digits = 1),
            people_in_prison_rri                 = round(people_in_prison_per10kadult/rri_analytic_white$people_in_prison_per10kadult, digits = 2),

            sentence_length_1_year_per10kadult = round(n_distinct(unique_id[sentlgth == "< 1 year"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_1_year_rri         = round(sentence_length_1_year_per10kadult/rri_analytic_white$sentence_length_1_year_per10kadult, digits = 2),

            sentence_length_1_1_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "1-1.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_1_1_9_years_rri         = round(sentence_length_1_1_9_years_per10kadult/rri_analytic_white$sentence_length_1_1_9_years_per10kadult, digits = 2),

            sentence_length_2_4_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_2_4_9_years_rri         = round(sentence_length_2_4_9_years_per10kadult/rri_analytic_white$sentence_length_2_4_9_years_per10kadult, digits = 2),

            sentence_length_5_9_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "2-4.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_5_9_9_years_rri         = round(sentence_length_5_9_9_years_per10kadult/rri_analytic_white$sentence_length_5_9_9_years_per10kadult, digits = 2),

            sentence_length_10_24_9_years_per10kadult = round(n_distinct(unique_id[sentlgth == "10-24.9 years"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_10_24_9_years_rri         = round(sentence_length_10_24_9_years_per10kadult/rri_analytic_white$sentence_length_10_24_9_years_per10kadult, digits = 2),

            sentence_length_25_years_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_25_years_rri         = round(sentence_length_25_years_per10kadult/rri_analytic_white$sentence_length_25_years_per10kadult, digits = 2),

            sentence_length_lwop_per10kadult = round(n_distinct(unique_id[sentlgth == "Life, LWOP, Life plus additional years, Death"])/mean(race_eth_pop)*10000, digits = 1),
            sentence_length_lwop_rri         = round(sentence_length_lwop_per10kadult/rri_analytic_white$sentence_length_lwop_per10kadult, digits = 2)
  ) %>%
  ungroup()

# reformat table
rri_analytic_table <- rri_analytic %>%
  select(race_eth,
         people_in_prison_rri,
         sentence_length_1_year_rri,
         sentence_length_1_1_9_years_rri,
         sentence_length_2_4_9_years_rri,
         sentence_length_5_9_9_years_rri,
         sentence_length_10_24_9_years_rri,
         sentence_length_25_years_rri,
         sentence_length_lwop_rri) %>%
  gather(sample, rri, people_in_prison_rri:sentence_length_lwop_rri) %>%
  mutate(state = "Georgia")

# prep for graph
all_census_ncrp_rri_prep <- rri_analytic_table %>%
  mutate(color = ifelse(rri < 1, "#ff640080", "#ff6400"),
         sample = case_when(
           sample == "people_in_prison_rri" ~ "In Prison",
           sample == "sentence_length_1_year_rri"        ~ "Sentence Length < 1 year",
           sample == "sentence_length_1_1_9_years_rri"   ~ "Sentence Length 1-1.9 years",
           sample == "sentence_length_2_4_9_years_rri"   ~ "Sentence Length 2-4.9 years",
           sample == "sentence_length_5_9_9_years_rri"   ~ "Sentence Length 5-9.9 years",
           sample == "sentence_length_10_24_9_years_rri" ~ "Sentence Length 10-24.9 years",
           sample == "sentence_length_25_years_rri"      ~ "Sentence Length >=25 years",
           sample == "sentence_length_lwop_rri"          ~ "Sentence Length Life, LWOP, Death"
         ),
         rri = round(rri, 1),
         tooltip = case_when(
           sample == "In Prison" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to be in prison than White people."),
           sample == "In Prison" & rri == 1 ~ paste0(race_eth, " people are equally as likely to be in prison as White people."),
           sample == "In Prison" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to be in prison than White people."),

           sample == "Sentence Length < 1 year" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of < 1 year than White people."),
           sample == "Sentence Length < 1 year" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of < 1 year as White people."),
           sample == "Sentence Length < 1 year" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of < 1 year than White people."),

           sample == "Sentence Length 1-1.9 years" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of 1-1.9 years than White people."),
           sample == "Sentence Length 1-1.9 years" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of 1-1.9 years as White people."),
           sample == "Sentence Length 1-1.9 years" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of 1-1.9 years than White people."),

           sample == "Sentence Length 2-4.9 years" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of 2-4.9 years than White people."),
           sample == "Sentence Length 2-4.9 years" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of 2-4.9 years as White people."),
           sample == "Sentence Length 2-4.9 years" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of 2-4.9 years than White people."),

           sample == "Sentence Length 5-9.9 years" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of 5-9.9 years than White people."),
           sample == "Sentence Length 5-9.9 years" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of 5-9.9 years as White people."),
           sample == "Sentence Length 5-9.9 years" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of 5-9.9 years than White people."),

           sample == "Sentence Length 10-24.9 years" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of 10-24.9 years than White people."),
           sample == "Sentence Length 10-24.9 years" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of 10-24.9 years as White people."),
           sample == "Sentence Length 10-24.9 years" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of 10-24.9 years than White people."),

           sample == "Sentence Length >=25 years" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of >=25 years than White people."),
           sample == "Sentence Length >=25 years" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of >=25 years as White people."),
           sample == "Sentence Length >=25 years" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of >=25 years than White people."),

           sample == "Sentence Length Life, LWOP, Death" & rri < 1  ~ paste0(race_eth, " people are ", round((1 - rri)*100, 0), "% less likely <br>to have a sentence length of life, life without parole, or death than White people."),
           sample == "Sentence Length Life, LWOP, Death" & rri == 1 ~ paste0(race_eth, " people are equally as likely to have a sentence length of life, life without parole, or death as White people."),
           sample == "Sentence Length Life, LWOP, Death" & rri > 1  ~ paste0(race_eth, " people are ", rri, " times more likely <br>to have a sentence length of life, life without parole, or death than White people."),

           TRUE ~ NA_character_
         )) %>%
  mutate(color = case_when(rri < 1 ~ "#ff640080",
                           rri == 1 ~ "gray",
                           rri > 1 ~ "#ff6400"),

         type = case_when(rri < 1 ~ "Underrepresented",
                          rri == 1 ~ "Equally Represented",
                          rri > 1 ~ "Overrepresented"),
         rri_pct = case_when(
           rri < 1  ~ round((1 - rri)*100, 0),
           rri == 1 ~ 100, ############################################################?
           rri > 1  ~ round((rri - 1)*100, 0)
         ),
         rri_label = case_when(
           rri < 1  ~ paste0(round((1 - rri)*100, 0), "% less likely"),
           rri == 1 ~ paste0("Equally as likely"),
           rri > 1  ~ paste0(round((rri - 1)*100, 0), "% more likely")
         )
  ) %>%

  mutate_all(~ ifelse(is.nan(.), NA, .)) %>%
  mutate_all(~ ifelse(is.infinite(.), NA, .))

df1 <- all_census_ncrp_rri_prep %>%
  filter(state == "Georgia") %>%
  filter(race_eth == "Black, non-Hispanic") %>%
  filter(!is.na(rri)) %>%
  filter(!is.infinite(rri))

df1

min_value <- 0
# calculate the common max value
max_value_black <- max(all_census_ncrp_rri_prep %>%
                         filter(state == "Georgia") %>%
                         filter(race_eth == "Black, non-Hispanic") %>%
                         filter(!is.na(rri)) %>%
                         filter(!is.infinite(rri)) %>%
                         pull(rri_pct),
                       na.rm = TRUE)

max_value_hispanic <- max(all_census_ncrp_rri_prep %>%
                            filter(state == "Georgia") %>%
                            filter(race_eth == "Hispanic, any race") %>%
                            filter(!is.na(rri)) %>%
                            filter(!is.infinite(rri)) %>%
                            pull(rri_pct),
                          na.rm = TRUE)

# Determine the larger max value
max_value <- max(max_value_black, max_value_hispanic)
max_value <- ceiling(max_value)

highcharts <- df1 %>%
  hchart(type = "bar", hcaes(x = "sample", y = "rri_pct", group = "type")) %>%
  hc_title(text = "Black People, Non-Hispanic") %>%
  hc_subtitle(text = "Relative Rate Index") %>%
  hc_xAxis(title = "",
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
           min = 0,
           max = max_value) %>%
  hc_legend(enabled = TRUE) %>%
  hc_add_theme(hc_theme_jc) %>%
  hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
  hc_plotOptions(series = list(stacking = "normal"),
                 bar = list(
                   dataLabels = list(enabled = TRUE, format = "{point.rri_label}", style = list(fontSize = "12px"))
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

highcharts


# ########################################
#
# # Bar graph of proportion of people/demographic released on or after their PED
#
# ########################################
#
# # prepare data for graphs
# ncrp_time_between_release_ped_2020_by_race <-
#   ncrp_releases_2020 %>%
#   filter(!is.na(time_between_release_ped)) %>%
#   filter(race == "Hispanic, any race" |
#            race == "White, non-Hispanic" |
#            race == "Black, non-Hispanic") %>%
#   mutate(time_between_release_ped_overall =
#            case_when(
#              time_between_release_ped > 1 ~ "Released After Year of PED",
#              time_between_release_ped <= 1 ~ "Released Before or on Year of PED",
#              is.na(time_between_release_ped) ~ "No PED Data"
#            )
#   ) %>%
#   group_by(state, race) %>%
#   count(time_between_release_ped_overall) %>%
#   mutate(
#     prop = n / sum(n),
#     prop_label = paste0(round(prop * 100, 0), "%"),
#     tooltip = paste0("<b>", state, "</b><br><br><b>",
#                      time_between_release_ped_overall,
#                      "</b><br><br>",
#                      "Number of People: <b>",
#                      scales::comma(n),
#                      "</b><br><br>",
#                      "Percentage of Prison Population: <b>",
#                      prop_label, "</b></b>", sep = ""))
#
# # get list of states with data
# states <- unique(ncrp_time_between_release_ped_2020_by_race$state)
#
# # create graph by state
# all_time_between_release_ped_2020_by_race <- map(.x = states, .f = function(x) {
#   df1 <- ncrp_time_between_release_ped_2020_by_race %>% filter(state == x)
#
#   highcharts <- highchart() %>%
#     hc_chart(type = "column") %>%
#     hc_xAxis(categories = c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) %>%
#     hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
#     hc_add_series(data = subset(df1, time_between_release_ped_overall == "Released Before or on Year of PED"),
#                   name = "Released Before or on Year of PED",
#                   type = "column",
#                   dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
#                                     style = list(fontWeight = "regular")),
#                   hcaes(x = race, y = prop * 100)) %>%
#     hc_add_series(data = subset(df1, time_between_release_ped_overall == "Released After Year of PED"),
#                   name = "Released After Year of PED",
#                   type = "column",
#                   dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
#                                     style = list(fontWeight = "regular")),
#                   hcaes(x = race, y = prop * 100)) %>%
#     hc_add_theme(hc_theme_jc) %>%
#     hc_colors(colors = c(teal, orange)) %>%
#     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
#     hc_exporting(enabled = TRUE) %>%
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
# all_time_between_release_ped_2020_by_race <- setNames(all_time_between_release_ped_2020_by_race, states)









#######################################
# Project: AV Parole
# File: predicted_probabilities.R
# Authors: Mari Roberts
# Date last updated: May 4, 2023 (MAR)
# Description:
#    Calculate predicted probabilities for each state
#    by race, sex, admtype, offgeneral, sentlngth

# Input:
# ncrp_releases - created in releases_ncrp.R

# Output
# all_pp_by_variable - used for app
#######################################


library(dplyr)
library(broom)
library(broom.helpers)

# prepare data for analysis
# ncrp_releases created in releases_ncrp.R
ncrp_pp_model_data <- ncrp_releases %>%
  # filter to 2020 report year
  # remove releases that were classified as "other"
  filter(rptyear == 2020) %>%
  # filter(state == ) %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  filter(admtype != "Other admission (including unsentenced, transfer, AWOL/escapee return)") %>%
  filter(race != "Other race(s), non-Hispanic") %>%
  # create binary variable for being released within 1 year of parole eligibility
  mutate(release_within_1yr_ped =
           case_when(time_between_release_ped <= 1 ~ 1,
                     time_between_release_ped > 1  ~ 0)) %>%

  # remove other offenses
  filter(offgeneral != "Other/unspecified") %>%

  # factor variables
  mutate(release_within_1yr_ped = factor(release_within_1yr_ped),
         state      = factor(state),
         sex        = factor(sex),
         race       = factor(race),
         admtype    = factor(admtype),
         offgeneral = factor(offgeneral, ordered = TRUE,
                             levels = c("Other/unspecified",
                                        "Public order",
                                        "Drug",
                                        "Property",
                                        "Violent")),
         sentlgth   = factor(sentlgth, ordered = TRUE,
                             levels = c("< 1 year",
                                        "1-1.9 years",
                                        "2-4.9 years",
                                        "5-9.9 years",
                                        "10-24.9 years",
                                        ">=25 years")))  %>%
  # set reference levels
  mutate(sex     = relevel(sex,     ref = "Male"),
         race    = relevel(race,    ref = "White, non-Hispanic"),
         admtype = relevel(admtype, ref = "New court commitment")) %>%

  # create id
  mutate(release_id = row_number()) %>%

  # remove missing data and remove offenses in the general category.
  filter(!is.na(release_within_1yr_ped) &
           !is.na(sex) &
           !is.na(race) &
           !is.na(admtype) &
           !is.na(offgeneral) &
           !is.na(sentlgth)) %>%

  # select variables
  select(state,
         release_within_1yr_ped,
         sex,
         race,
         admtype,
         offgeneral,
         sentlgth) %>%

  droplevels()

# https://druedin.com/2016/01/16/predicted-probabilities-in-r/
# save the losgitic regression formula
fmla <- release_within_1yr_ped ~ sex + race + admtype + offgeneral + sentlgth

# define the list of states to loop through, excluding multiple states
excluded_states <- c("California",
                     "Florida",
                     "Illinois",
                     "Louisiana" ,
                     "Minnesota",
                     "Missouri",
                     "Montana",
                     "Nebraska",
                     "Nevada",
                     "New York",
                     "New Hampshire",
                     "Wyoming")

states_list <- setdiff(unique(ncrp_pp_model_data$state), excluded_states)

# create an empty list to store the results for each state
state_pp_list <- list()

# loop through each state and calculate the predicted probabilities
for (state in states_list) {
  state_data <- ncrp_pp_model_data[ncrp_pp_model_data$state == state, ]

  # run logistic regression
  glm_model <- glm(fmla,
                   family = "binomial", data = state_data)

  # calculate the predicted probabilities for each combination of predictor variables
  new_data <- expand.grid(sex        = levels(state_data$sex),
                          race       = levels(state_data$race),
                          admtype    = levels(state_data$admtype),
                          offgeneral = levels(state_data$offgeneral),
                          sentlgth   = levels(state_data$sentlgth))
  new_data$release_within_1yr_ped <- predict(glm_model,
                                             newdata = new_data,
                                             type = "response")

  # save the predicted probabilities for each race
  pp_by_race <- aggregate(release_within_1yr_ped ~ race,
                          data = new_data, FUN = mean)

  # save the predicted probabilities for each sex
  pp_by_sex <- aggregate(release_within_1yr_ped ~ sex,
                         data = new_data, FUN = mean)

  # save the predicted probabilities for each admtype
  pp_by_admtype <- aggregate(release_within_1yr_ped ~ admtype,
                             data = new_data, FUN = mean)

  # save the predicted probabilities for each offgeneral
  pp_by_offgeneral <- aggregate(release_within_1yr_ped ~ offgeneral,
                                data = new_data, FUN = mean)

  # save the predicted probabilities for each sentlgth
  pp_by_sentlgth <- aggregate(release_within_1yr_ped ~ sentlgth,
                              data = new_data, FUN = mean)

  # add all results to the list for this state
  state_pp_list[[state]] <- list(pp_by_race,
                                 pp_by_sex,
                                 pp_by_admtype,
                                 pp_by_offgeneral,
                                 pp_by_sentlgth)
}

# create an empty dataframe to store the results for each state
all_pp_by_variable <- data.frame()

# loop through each state and create a row in the dataframe
# This code loops through each state, extracts the results for that state from
# state_pp_list, and adds a row to the all_pp_by_variable dataframe with the results
# for that state. The resulting dataframe has columns for the state, race,
# sex, admtype, offgeneral, sentlgth, and the corresponding predicted probabilities.
for (state in states_list) {
  # extract the results for this state from the list
  pp_by_race <- state_pp_list[[state]][[1]]
  pp_by_sex <- state_pp_list[[state]][[2]]
  pp_by_admtype <- state_pp_list[[state]][[3]]
  pp_by_offgeneral <- state_pp_list[[state]][[4]]
  pp_by_sentlgth <- state_pp_list[[state]][[5]]

  # add a row to the dataframe with the results for this state
  all_pp_by_variable <- rbind(all_pp_by_variable,
                              data.frame(state = state,
                                         race = pp_by_race$race,
                                         pp_by_race = pp_by_race$release_within_1yr_ped,
                                         sex = pp_by_sex$sex,
                                         pp_by_sex = pp_by_sex$release_within_1yr_ped,
                                         admtype = pp_by_admtype$admtype,
                                         pp_by_admtype = pp_by_admtype$release_within_1yr_ped,
                                         offgeneral = pp_by_offgeneral$offgeneral,
                                         pp_by_offgeneral = pp_by_offgeneral$release_within_1yr_ped,
                                         sentlgth = pp_by_sentlgth$sentlgth,
                                         pp_by_sentlgth = pp_by_sentlgth$release_within_1yr_ped))
}












library(tidycensus)
library(tidyverse)
library(dplyr)
library(scales)
options(dplyr.summarise.inform = FALSE)

# Make sure you have a Census API Key
# http://api.census.gov/data/key_signup.html and then supply the key to the
# `census_api_key("YOUR KEY HERE", install = TRUE)` which will add it to your Renviron

# pull census/acs API key
readRenviron("~/.Renviron")




##########################

# Get prison population data by race and state
# Using prisoner data instead of NCRP because there is more race/ethnicity information

##########################

# # warning message: NA's will be created for text that isnt numeric
# state_prison_pop <- prison_pop_by_race_state_2020 %>%
#   pivot_longer(cols = c(total:did_not_report), names_to = "race", values_to = "prison_pop") %>%
#   mutate(prison_pop = as.numeric(prison_pop)) %>%
#   filter(race != "total" &
#            race != "did_not_report" &
#            race != "other_a" &
#            race != "unknown" &
#            race != "two_or_more_races_a") %>%
#   mutate(race = case_when(
#     race == "white_a"                                  ~ "White",
#     race == "black_a"                                  ~ "Black",
#     race == "hispanic"                                 ~ "Hispanic",
#     race == "american_indian_alaska_native_a"          ~ "American Indian/Alaska Native",
#     race == "asian_a"                                  ~ "Asian/Pacific Islander",
#     race == "native_hawaiian_other_pacific_islander_a" ~ "Asian/Pacific Islander"
#   )) %>%
#   group_by(state, race) %>%
#   summarise(prison_pop = sum(prison_pop)) %>%
#   mutate(state = case_when(
#     state == "Maryland/d"      ~ "Maryland",
#     state == "Michigan/d"      ~ "Michigan",
#     state == "Montana/e"       ~ "Montana",
#     state == "Nevada/d"        ~ "Nevada",
#     state == "New Mexico/f"    ~ "New Mexico",
#     state == "Ohio/g"          ~ "Ohio",
#     state == "Pennsylvania/d"  ~ "Pennsylvania",
#     state == "Rhode Island/dh" ~ "Rhode Island",
#     state == "Virginia/ci"     ~ "Virginia",
#     TRUE ~ state
#   ))

# Alabama looks weird
state_prison_pop <- ncrp_yearendpop %>%
  filter(rptyear == 2020 &
           !is.na(race)) %>%
  group_by(state) %>%
  count(race) %>%
  rename(prison_pop = n)


##########################
# Get census data
##########################

# weighted estimate of percentage of race from 2020 census
# pulled estimated counts and construct percent estimate
# these are the ids of race variables that we want to pull
race_vars <- c(estimate_white              = "P3_003N",
               estimate_black              = "P3_004N",
               estimate_asian              = "P3_006N",
               estimate_native_hawaiian_pi = "P3_007N",
               estimate_hispanic           = "P4_002N",
               estimate_american_indian    = "P1_005N")

# create empty dataframe
race_eth_rri_table <- data.frame()

# get states
state_loop <- unique(state_prison_pop$state)


state_race_2020 <-
  tidycensus::get_decennial(geography = "state",
                            state = "Georgia",
                            variables = race_vars,
                            # total population for 18+ population from race table
                            summary_var = "P3_001N",
                            year = 2020,
                            geometry = FALSE) %>%
  clean_names() %>%
  select(-geoid) %>%

  # rename race levels
  mutate(race = case_when(
    variable == "estimate_american_indian"         ~ "Other race(s), non-Hispanic",
    variable %in% c("estimate_asian",
                    "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
    variable == "estimate_black"                   ~ "Black, non-Hispanic",
    variable == "estimate_hispanic"                ~ "Hispanic, any race",
    variable == "estimate_white"                   ~ "White, non-Hispanic",
    TRUE ~ "NA"))

# # get rii by state and add all state rii data together
# for(i in state_loop) {
#
#   state_race_2020 <-
#     tidycensus::get_decennial(geography = "state",
#                               state = i,
#                               variables = race_vars,
#                               # total population for 18+ population from race table
#                               summary_var = "P3_001N",
#                               year = 2020,
#                               geometry = FALSE) %>%
#     clean_names() %>%
#     select(-geoid) %>%
#
#     # rename race levels
#     mutate(race = case_when(
#       variable == "estimate_american_indian"         ~ "Other race(s), non-Hispanic",
#       variable %in% c("estimate_asian",
#                       "estimate_native_hawaiian_pi") ~ "Other race(s), non-Hispanic",
#       variable == "estimate_black"                   ~ "Black, non-Hispanic",
#       variable == "estimate_hispanic"                ~ "Hispanic, any race",
#       variable == "estimate_white"                   ~ "White, non-Hispanic",
#       TRUE ~ "NA")) %>%
#     group_by(race) %>%
#     mutate(statewide_overall_value = sum(value)) %>%
#     ungroup() %>%
#     group_by(variable) %>%
#
#     # create statewide count estimate
#     mutate(statewide_summary_value = sum(summary_value)) %>%
#     ungroup() %>%
#     distinct(race, .keep_all = TRUE) %>%
#
#     #create percent estimate from estimated counts
#     mutate(estimate = (statewide_overall_value/statewide_summary_value)*100) %>%
#     ungroup() %>%
#     rename(state_race_pop         = statewide_overall_value,
#            total_pop              = statewide_summary_value,
#            state_race_pop_percent = estimate) %>%
#     select(-c(variable,value)) %>%
#     distinct()
#
#   # prep census data for join and rri calculation
#   state_race_2020 <- state_race_2020 %<>%
#     ungroup() %>%
#     dplyr::select(state = name,
#                   race,
#                   state_race_pop)
#
#   state_prison_pop_sub <- state_prison_pop %>%
#     filter(state == i)
#
#   # join two files
#   state_rii <- left_join(state_prison_pop_sub, state_race_2020, by = c("state", "race"))
#
#   # white, non-hispanic only for rri calculation
#   race_eth_rri_white <- state_rii %>%
#     filter(!is.na(race) == TRUE,
#            race == "White") %>%
#     dplyr::summarise(individuals_prison_per_10k = round(prison_pop/mean(state_race_pop)*10000, digits = 1),
#                      individuals_prison_rii_to_white = NA) %>%
#     dplyr::mutate(race_ethnicity = "White")
#
#   # now calculate rates and RRIs using above dataframe (for White individuals) as reference
#   race_eth_rri_poc <- state_rii %>%
#     dplyr::filter(!is.na(race)==TRUE,
#                   race %in% c("Other race(s), non-Hispanic",
#                               "Black, non-Hispanic",
#                               "Hispanic")) %>%
#     dplyr::group_by(race, state) %>%
#     dplyr::summarise(individuals_prison_per_10k =
#                        round(prison_pop/mean(state_race_pop)*10000, digits = 1),
#
#                      individuals_prison_rii_to_white =
#                        round(individuals_prison_per_10k/race_eth_rri_white$individuals_prison_per_10k,
#                              digits = 2)) %>%
#     ungroup() %>%
#     dplyr::rename(race_ethnicity = race)
#
#   # bind tables together
#   output <- rbind(race_eth_rri_poc, race_eth_rri_white)
#
#   # add output to previous outputs of states into one dataframe
#   race_eth_rri_table = rbind(race_eth_rri_table, output)
#
# }






##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_time_between_release_ped_2020_by_race, file=file.path(folder, "all_time_between_release_ped_2020_by_race.rds"))
  save(all_pp_by_variable, file=file.path(folder, "all_pp_by_variable.rds"))
  save(race_eth_rri_table, file=file.path(folder, "race_eth_rri_table.rds"))

}
