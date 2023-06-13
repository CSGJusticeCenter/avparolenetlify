#######################################
# Project: AV Parole
# File: ncrp_releases.R
# Authors: Mari Roberts
# Date last updated: June 12, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for app
#######################################

##################

# Data cleaning for:
# Timing of release overall, by adm type, and by offense type

##################

ncrp_sentlgth_timesrvd_rel <- ncrp_releases %>%

  # create order for sentence length and time served length
  # for example, <1 is 1 and 1-1.9 is 2, and so on
  mutate(
    sentlgth_order = case_when(
      sentlgth == "< 1 year"      ~ 1,
      sentlgth == "1-1.9 years"   ~ 2,
      sentlgth == "2-4.9 years"   ~ 3,
      sentlgth == "5-9.9 years"   ~ 4,
      sentlgth == "10-24.9 years" ~ 5,
      sentlgth == ">=25 years"    ~ 5,
      sentlgth == "Life, LWOP, Life plus additional years, Death" ~ 5,
      TRUE ~ NA),
    timesrvd_rel_order = case_when(
      timesrvd_rel == "< 1 year"      ~ 1,
      timesrvd_rel == "1-1.9 years"   ~ 2,
      timesrvd_rel == "2-4.9 years"   ~ 3,
      timesrvd_rel == "5-9.9 years"   ~ 4,
      timesrvd_rel == ">=10 years"    ~ 5,
      TRUE ~ NA)) %>%

  # determine differences between time served and sentenced length
  # calculate actual time served
  mutate(
    timesrvd_rel_vs_sentlgth = case_when(
      is.na(timesrvd_rel_order) | is.na(sentlgth_order) ~ NA,
      timesrvd_rel_order == sentlgth_order ~ "Full Sentence Length Served",
      timesrvd_rel_order > sentlgth_order  ~ "More than Sentence Length Served",
      timesrvd_rel_order < sentlgth_order  ~ "Less than Sentence Length Served"),
    time_served = relyr - admityr) %>%

  # https://www.icpsr.umich.edu/web/NACJD/studies/38492/datasets/0003/variables/PARELIG_YEAR?archive=nacjd
  # remove parelig_year/mand_prisrel_year 2100
  mutate(parelig_year_clean =
           ifelse(parelig_year <= 2105, parelig_year, NA),
         mand_prisrel_year_clean =
           ifelse(mand_prisrel_year <= 2105, mand_prisrel_year, NA),

         time_between_release_ped = relyr - parelig_year_clean,
         time_between_ped_admission = parelig_year_clean - admityr,
         time_between_mandatoryrelease_release = mand_prisrel_year_clean - relyr,
         time_between_release_admissions = relyr - admityr) %>%

  mutate(released_at_ped_status = case_when(
    time_between_release_ped < 0 ~ "Released Before Parole Eligibility Year",
    time_between_release_ped == 0 ~ "Released on Parole Eligibility Year",
    time_between_release_ped > 0 ~ "Released After Parole Eligibility Year",
    is.na(time_between_release_ped) ~ NA))

# Subset to 2020 report
ncrp_releases_2020 <- ncrp_sentlgth_timesrvd_rel %>%
  filter(rptyear == 2020)

# How many people are being released at first eligibility?
ncrp_released_at_ped_2020 <- ncrp_releases_2020 %>%
  # remove states with NA's
  filter(!is.na(released_at_ped_status)) %>%
  group_by(state) %>%
  count(released_at_ped_status) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
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
  filter(!is.na(released_at_ped_status) &
           !is.na(admtype)) %>%
  group_by(state, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
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
  group_by(state, offgeneral) %>%
  count(released_at_ped_status) %>%
  mutate(prop = n/sum(n),
         prop_label = paste0(round(prop*100, 0), "%"),
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

# Get list of states
states <- unique(ncrp_released_at_ped_2020$state)

all_pie_released_at_ped_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_2020 %>% filter(state == x)
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_2020 <- setNames(all_pie_released_at_ped_2020, states)

# Get list of states
states <- unique(ncrp_released_at_ped_admtype_2020$state)

all_pie_released_at_ped_parolereturn_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_admtype_2020 %>%
    filter(state == x) %>%
    filter(admtype == "Parole return/revocation")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_parolereturn_2020 <- setNames(all_pie_released_at_ped_parolereturn_2020, states)

all_pie_released_at_ped_newcrime_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_admtype_2020 %>%
    filter(state == x) %>%
    filter(admtype == "New court commitment")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_newcrime_2020 <- setNames(all_pie_released_at_ped_newcrime_2020, states)

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Drugs") %>%
  pull(state) %>%
  unique()

all_pie_released_at_ped_drugs_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Drugs")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_drugs_2020 <- setNames(all_pie_released_at_ped_drugs_2020, states)

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Other/unspecified") %>%
  pull(state) %>%
  unique()

all_pie_released_at_ped_other_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Other/unspecified")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_other_2020 <- setNames(all_pie_released_at_ped_other_2020, states)

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Property") %>%
  pull(state) %>%
  unique()

all_pie_released_at_ped_property_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Property")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_property_2020 <- setNames(all_pie_released_at_ped_property_2020, states)

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Public order") %>%
  pull(state) %>%
  unique()

all_pie_released_at_ped_publicorder_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Public order")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_publicorder_2020 <- setNames(all_pie_released_at_ped_publicorder_2020, states)

# Get list of states
states <- ncrp_released_at_ped_offgeneral_2020 %>%
  filter(offgeneral == "Violent") %>%
  pull(state) %>%
  unique()
all_pie_released_at_ped_violent_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_offgeneral_2020 %>%
    filter(state == x) %>%
    filter(offgeneral == "Violent")
  highcharts <- fnc_pie_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.chart_label}",
                              accessibility_text = "TBD.")
  return(highcharts)
})

all_pie_released_at_ped_violent_2020 <- setNames(all_pie_released_at_ped_violent_2020, states)



##################

# Save data

##################

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_pie_released_at_ped_2020,
       file=file.path(folder, "all_pie_released_at_ped_2020.rds"))
  save(all_pie_released_at_ped_publicorder_2020,
       file=file.path(folder, "all_pie_released_at_ped_publicorder_2020.rds"))
  save(all_pie_released_at_ped_property_2020,
       file=file.path(folder, "all_pie_released_at_ped_property_2020.rds"))
  save(all_pie_released_at_ped_other_2020,
       file=file.path(folder, "all_pie_released_at_ped_other_2020.rds"))
  save(all_pie_released_at_ped_drugs_2020,
       file=file.path(folder, "all_pie_released_at_ped_drugs_2020.rds"))
  save(all_pie_released_at_ped_newcrime_2020,
       file=file.path(folder, "all_pie_released_at_ped_newcrime_2020.rds"))
  save(all_pie_released_at_ped_parolereturn_2020,
       file=file.path(folder, "all_pie_released_at_ped_parolereturn_2020.rds"))

}
