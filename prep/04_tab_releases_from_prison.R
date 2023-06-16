#######################################
# Project: AV Parole
# File: ncrp_releases.R
# Authors: Mari Roberts
# Date last updated: June 16, 2023 (MAR)
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
  group_by(state, offgeneral) %>%
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

# assign x axis order
desired_order <- c("Released Before Parole Eligibility Year",
                   "Released on Parole Eligibility Year",
                   "Released After Parole Eligibility Year")

# Get list of states
states <- unique(ncrp_released_at_ped_2020$state)

all_bar_released_at_ped_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_2020 %>% filter(state == x) %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_2020 <- setNames(all_bar_released_at_ped_2020, states)
all_bar_released_at_ped_2020$California





# Get list of states
states <- unique(ncrp_released_at_ped_admtype_2020$state)

all_bar_released_at_ped_parolereturn_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_admtype_2020 %>%
    filter(state == x) %>%
    filter(admtype == "Parole return/revocation") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_parolereturn_2020 <- setNames(all_bar_released_at_ped_parolereturn_2020, states)
all_bar_released_at_ped_parolereturn_2020$California






all_bar_released_at_ped_newcrime_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_admtype_2020 %>%
    filter(state == x) %>%
    filter(admtype == "New court commitment") %>%
    arrange(match(released_at_ped_status, desired_order))
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_newcrime_2020 <- setNames(all_bar_released_at_ped_newcrime_2020, states)
all_bar_released_at_ped_newcrime_2020$California





# Get list of states
states <- unique(ncrp_released_at_ped_admtype_2020$state)

all_bar_released_at_ped_admtype_2020 <- map(.x = states,  .f = function(x) {
  df1 <- ncrp_released_at_ped_admtype_2020 %>%
    filter(state == x) %>%
    filter(admtype != "Other admission (including unsentenced, transfer, AWOL/escapee return)")
  highcharts <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = c("New court commitment",
                            "Parole return/revocation")) %>%
    hc_yAxis(labels = list(format = "{value}%"), min = 0, max = 100) %>%
    hc_add_series(data = subset(df1, released_at_ped_status == "Released Before Parole Eligibility Year"),
                  name = "Released Before Parole Eligibility Year",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = admtype, y = prop)) %>%
    hc_add_series(data = subset(df1, released_at_ped_status == "Released on Parole Eligibility Year"),
                  name = "Released on Parole Eligibility Year",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = admtype, y = prop)) %>%
    hc_add_series(data = subset(df1, released_at_ped_status == "Released After Parole Eligibility Year"),
                  name = "Released After Parole Eligibility Year",
                  type = "column",
                  dataLabels = list(enabled = TRUE, format = "{point.prop_label}",
                                    style = list(fontWeight = "regular")),
                  hcaes(x = admtype, y = prop)) %>%
    hc_add_theme(hc_theme_jc) %>%
    hc_colors(colors = c(yellow, teal, orange)) %>%
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = "TBD",
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = "TBD"))
    )

  return(highcharts)
})

all_bar_released_at_ped_admtype_2020 <- setNames(all_bar_released_at_ped_admtype_2020, states)
all_bar_released_at_ped_admtype_2020$Georgia

















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
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_drugs_2020 <- setNames(all_bar_released_at_ped_drugs_2020, states)
all_bar_released_at_ped_drugs_2020$California





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
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_other_2020 <- setNames(all_bar_released_at_ped_other_2020, states)
all_bar_released_at_ped_other_2020$California






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
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_property_2020 <- setNames(all_bar_released_at_ped_property_2020, states)
all_bar_released_at_ped_property_2020$California






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
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_publicorder_2020 <- setNames(all_bar_released_at_ped_publicorder_2020, states)
all_bar_released_at_ped_publicorder_2020$California





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
  highcharts <- fnc_percent_bar_chart(df = df1,
                              x_variable = "released_at_ped_status",
                              y_variable = "prop",
                              point_format = "{point.prop_label}",
                              accessibility_text = "TBD.")
  highcharts <- highcharts %>%
    hc_colors(colors = c(yellow, teal, orange))
  return(highcharts)
})

all_bar_released_at_ped_violent_2020 <- setNames(all_bar_released_at_ped_violent_2020, states)
all_bar_released_at_ped_violent_2020$California





















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
  save(all_bar_released_at_ped_newcrime_2020,
       file=file.path(folder, "all_bar_released_at_ped_newcrime_2020.rds"))
  save(all_bar_released_at_ped_parolereturn_2020,
       file=file.path(folder, "all_bar_released_at_ped_parolereturn_2020.rds"))

  save(all_bar_released_at_ped_admtype_2020,
       file=file.path(folder, "all_bar_released_at_ped_admtype_2020.rds"))

}
