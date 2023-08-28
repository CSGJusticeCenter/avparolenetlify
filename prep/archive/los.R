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

# count and get prop of people by state, adm type, and report year
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
         prop_label = paste0(round(prop, 0), "%")) %>%
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

# create grouped bar chart showing proportion of sentence served by adm type
all_bar_los_overview_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_proportion_served_2020 %>%
    filter(state == x) %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                  point_format = "{point.prop_label}",
                                  accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_overview_2020 <- setNames(all_bar_los_overview_2020, states)
all_bar_los_overview_2020$Georgia

########
# Violent
########

# remove NA's and "other" releases which includes transfers and deaths
# count and get prop of people by state, offense type, adm type, and report year
ncrp_offense_proportion_served_2020 <- ncrp_releases %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  filter(!is.na(timesrvd_rel_vs_sentlgth)) %>%
  filter(!is.na(offgeneral)) %>%
  filter(rptyear == 2020) %>%
  group_by(state, offgeneral,admtype, rptyear) %>%
  count(timesrvd_rel_vs_sentlgth) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%")) %>%
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

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Violent") %>%
  pull(state) %>%
  unique()

# create grouped bar chart showing proportion of sentence served by adm type for violent offenses
all_bar_los_violent_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Violent") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_violent_2020 <- setNames(all_bar_los_violent_2020, states)


########
# Drugs
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Drugs") %>%
  pull(state) %>%
  unique()

all_bar_los_drugs_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Drugs") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_drugs_2020 <- setNames(all_bar_los_drugs_2020, states)


########
# Property
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Property") %>%
  pull(state) %>%
  unique()

all_bar_los_property_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Property") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_property_2020 <- setNames(all_bar_los_property_2020, states)


########
# Public order
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Public order") %>%
  pull(state) %>%
  unique()

all_bar_los_publicorder_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Public order") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_publicorder_2020 <- setNames(all_bar_los_publicorder_2020, states)


########
# Other
########

# Get list of states
states <- ncrp_offense_proportion_served_2020 %>%
  filter(offgeneral == "Other/unspecified") %>%
  pull(state) %>%
  unique()

all_bar_los_other_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_offense_proportion_served_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Other/unspecified") %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))
  highcharts <- fnc_bar_chart_los(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_los_other_2020 <- setNames(all_bar_los_other_2020, states)
