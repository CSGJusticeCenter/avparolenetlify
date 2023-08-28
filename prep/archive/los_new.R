
################################################################################

# Length of stay by offense type, year, and state

################################################################################

# assign x axis order
desired_order <- c("Less than Sentence Length Served",
                   "Full Sentence Length Served")

ncrp_releases <- ncrp_releases %>%
  filter(timesrvd_rel_vs_sentlgth!= "More than Sentence Length Served") # remove bc likely a data error

########
# Overview
########

ncrp_proportion_served_2020 <- ncrp_releases %>%
  filter(rptyear == 2020 &
           !is.na(offgeneral) &
           !is.na(timesrvd_rel_vs_sentlgth)) %>%
  filter(admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  group_by(state, rptyear) %>%
  count(timesrvd_rel_vs_sentlgth) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Sentence Duration: <b>",
                  timesrvd_rel_vs_sentlgth,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# get list of states with data
states <- ncrp_proportion_served_2020 %>%
  pull(state) %>%
  unique()


########
# Overall
########





########
# Other/unspecified
########

ncrp_proportion_served_offenses_2020 <- ncrp_releases %>%
  filter(rptyear == 2020 &
           !is.na(offgeneral) &
           !is.na(timesrvd_rel_vs_sentlgth)) %>%
  filter(admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  group_by(state, rptyear, offgeneral) %>%
  count(timesrvd_rel_vs_sentlgth) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         n_label = formattable::comma(n, 0)) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Sentence Duration: <b>",
                  timesrvd_rel_vs_sentlgth,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

all_bar_los_other_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Other/unspecified",
                           accessibility_text = "TBD")
all_bar_los_other_2020$Georgia

########
# Property
########
all_bar_los_property_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Property",
                                       accessibility_text = "TBD")
all_bar_los_property_2020$Georgia

########
# Violent
########
all_bar_los_violent_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Violent",
                                       accessibility_text = "TBD")
all_bar_los_violent_2020$Georgia

########
# Public Order
########
all_bar_los_publicorder_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Public order",
                                       accessibility_text = "TBD")
all_bar_los_publicorder_2020$Georgia

########
# Drugs
########
all_bar_los_drugs_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Drugs",
                                       accessibility_text = "TBD")
all_bar_los_drugs_2020$Georgia
