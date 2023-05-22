#######################################
# Project: AV Parole
# File: sentences_ncrp.R
# Authors: Mari Roberts
# Date last updated: May 22, 2023 (MAR)
# Description:
#     Sentencing tables and graphics for app
#######################################


########################################

# Proportion of prison terms by sentence length

########################################

ncrp_sentences <- ncrp_admissions %>%
  filter(!is.na(sentlgth)) %>%
  group_by(state, rptyear) %>%
  count(sentlgth) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
         chart_label = paste0(sentlgth, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Sentence Length: <b>",
                  sentlgth,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))





########################################

# Proportion of Black, Hispanic, and White People who have served at least 10 years by offense type

########################################

# Subset to 2020 report
ncrp_10yrs_2020 <- ncrp_releases_clean %>%
  filter(rptyear == 2020) %>%
  filter(reltype == "Conditional release" |
         reltype == "Unconditional release") %>%
  filter(!is.na(time_between_release_admissions)) %>%
  filter(!is.na(race)) %>%
  filter(race != "Other race(s), non-Hispanic") %>%
  # filter(offgeneral == "Violent") %>%
  mutate(served_10plus_years =
           case_when(time_between_release_admissions >= 10 ~ "Served at Least 10 Years",
                     time_between_release_admissions < 10 ~ "Served Less Than 10 Years")) %>%
  group_by(state, race) %>%
  count(served_10plus_years) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
         chart_label = paste0(served_10plus_years, " <b>", prop_label, "</b>"))






########################################

# Change in Life/LWOP over time

########################################


