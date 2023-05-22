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











