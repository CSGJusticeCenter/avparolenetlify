#######################################
# Project: AV Parole
# File: tab_releases_from_prison.R
# Authors: Mari Roberts
# Date last updated: July 10, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for app
#######################################

##################

# Data cleaning for:
# Timing of release overall, by adm type, and by offense type

##################

# Subset to 2020 report
ncrp_releases_2020 <- ncrp_sentlgth_timesrvd_rel %>%
  filter(rptyear == 2020)

# How many people are being released at first eligibility?
ncrp_released_at_ped_2020 <- ncrp_releases_2020 %>%
  # remove states with NA's
  filter(!is.na(released_at_ped_status)) %>%
  filter(admtype == "Parole return/revocation" |
         admtype == "New court commitment") %>%
  group_by(state, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Timing of Release: <b>",
                  released_at_ped_status,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# How many people are being released at first eligibility by adm type?
ncrp_released_at_ped_admtype_2020 <- ncrp_releases_2020 %>%
  # remove states with NA's
  dplyr::filter(!is.na(released_at_ped_status) &
           !is.na(admtype)) %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  group_by(state, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Timing of Release: <b>",
                  released_at_ped_status,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

# How many people are being released at first eligibility by offgeneral?
ncrp_released_at_ped_offgeneral_2020 <- ncrp_releases_2020 %>%
  # remove states with NA's
  filter(!is.na(released_at_ped_status) &
           !is.na(offgeneral)) %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  group_by(state, offgeneral, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Timing of Release: <b>",
                  released_at_ped_status,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))


##################

# Highcharts for:
# Timing of release overall, by adm type, and by offense type

##################


########
# Overall
########

# assign x axis order
desired_order <- c("Released Before Parole Eligibility Year",
                   "Released on Parole Eligibility Year",
                   "Released After Parole Eligibility Year")

# Get list of states
states <- unique(ncrp_released_at_ped_2020$state)

all_bar_released_at_ped_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_2020 %>% filter(state == x) %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <-
    fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                           point_format = "{point.prop_label}",
                                           accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_2020 <- setNames(all_bar_released_at_ped_2020, states)
all_bar_released_at_ped_2020$Georgia





########
# Drugs
########

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Drugs") %>%
  pull(state) %>%
  unique()

all_bar_released_at_ped_drugs_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Drugs") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                         point_format = "{point.prop_label}",
                                         accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_drugs_2020 <- setNames(all_bar_released_at_ped_drugs_2020, states)
all_bar_released_at_ped_drugs_2020$California





########
# Other
########

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Other/unspecified") %>%
  pull(state) %>%
  unique()

all_bar_released_at_ped_other_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Other/unspecified") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                         point_format = "{point.prop_label}",
                                         accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_other_2020 <- setNames(all_bar_released_at_ped_other_2020, states)
all_bar_released_at_ped_other_2020$Georgia





########
# Property
########

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Property") %>%
  pull(state) %>%
  unique()

all_bar_released_at_ped_property_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Property") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                         point_format = "{point.prop_label}",
                                         accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_property_2020 <- setNames(all_bar_released_at_ped_property_2020, states)
all_bar_released_at_ped_property_2020$Georgia





########
# Public Order
########

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Public order") %>%
  pull(state) %>%
  unique()

all_bar_released_at_ped_publicorder_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Public order") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                         point_format = "{point.prop_label}",
                                         accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_publicorder_2020 <- setNames(all_bar_released_at_ped_publicorder_2020, states)
all_bar_released_at_ped_publicorder_2020$Georgia





########
# Violent
########

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Violent") %>%
  pull(state) %>%
  unique()
all_bar_released_at_ped_violent_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Violent") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart_pestatus_admtype(df = df1,
                                                       point_format = "{point.prop_label}",
                                                       accessibility_text = "TBD.")
  return(highcharts)
})

all_bar_released_at_ped_violent_2020 <- setNames(all_bar_released_at_ped_violent_2020, states)
all_bar_released_at_ped_violent_2020$Georgia





















########################################

# Released to Parole Over Time

########################################

# count number of people released to parole by year and state
ncrp_released_to_parole <- ncrp_sentlgth_timesrvd_rel %>%
  filter(timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served") %>%
  filter(state != "Alabama") %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  group_by(rptyear, state) %>%
  summarise(releases_to_parole = n())

















# Subset to 2020 report
ncrp_release_type <- ncrp_sentlgth_timesrvd_rel %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  filter(!is.na(proportion_served))

df1 <- ncrp_los %>%
  group_by(state, rptyear) %>%
  count(reltype) %>%
  mutate(
    prop = n/sum(n),
    yearendpop = sum(n),
    prop = prop*100,
    prop_label = paste0(round(prop, 0), "%")) %>%
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

df1 <- df1 %>% filter(state == "California") %>%
  filter(rptyear >= 2015) %>%
  select(reltype, rptyear, n)
df1

# df1 <- ncrp_los %>%
#   group_by(state, admtype, rptyear) %>%
#   count(reltype) %>%
#   mutate(
#     prop = n/sum(n),
#     yearendpop = sum(n),
#     prop = prop*100,
#     prop_label = paste0(round(prop, 0), "%")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Release Type: <b>",
#                   reltype,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))
#
# df1 <- df1 %>% filter(state == "Alabama") %>%
#   filter(rptyear >= 2018) %>%
#   select(reltype, rptyear, prop)
# df1
# highchart() %>%
#   hc_chart(type = "column", polar = FALSE) %>%
#   hc_xAxis(categories = df1$rptyear) %>%
#   hc_add_series(
#     name = "Conditional Release",
#     data = subset(df1, reltype == "Conditional release" & admtype == "New court commitment")$prop,
#     stack = "A",
#     color = yellow
#   ) %>%
#   hc_add_series(
#     name = "Conditional Release",
#     data = subset(df1, reltype == "Conditional release" & admtype == "Parole return/revocation")$prop,
#     stack = "B",
#     linkedTo = "previous",
#     color = yellow
#   ) %>%
#   hc_add_series(
#     name = "Unconditional Release",
#     data = subset(df1, reltype == "Unconditional release" & admtype == "New court commitment")$prop,
#     stack = "A",
#     color = teal
#   ) %>%
#   hc_add_series(
#     name = "Unconditional Release",
#     data = subset(df1, reltype == "Unconditional release" & admtype == "Parole return/revocation")$prop,
#     stack = "B",
#     linkedTo = "previous",
#     color = teal
#   ) %>%
#   hc_plotOptions(column = list(
#     stacking = "normal"))



##################

# Save data

##################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_bar_released_at_ped_2020,
       file=file.path(folder, "all_bar_released_at_ped_2020.rds"))
  save(all_bar_released_at_ped_publicorder_2020,
       file=file.path(folder, "all_bar_released_at_ped_publicorder_2020.rds"))
  save(all_bar_released_at_ped_property_2020,
       file=file.path(folder, "all_bar_released_at_ped_property_2020.rds"))
  save(all_bar_released_at_ped_other_2020,
       file=file.path(folder, "all_bar_released_at_ped_other_2020.rds"))
  save(all_bar_released_at_ped_drugs_2020,
       file=file.path(folder, "all_bar_released_at_ped_drugs_2020.rds"))
  save(all_bar_released_at_ped_violent_2020,
       file=file.path(folder, "all_bar_released_at_ped_violent_2020.rds"))
}
