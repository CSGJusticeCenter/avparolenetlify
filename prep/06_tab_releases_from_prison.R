#######################################
# Project: AV Parole
# File: tab_releases_from_prison.R
# Authors: Mari Roberts
# Date last updated: August 28, 2023 (MAR)
# Description:
#    Releases from prison tables and graphics for app
#######################################


# subset to 2020 report
ncrp_releases_2020 <- ncrp_releases %>%
  filter(rptyear == 2020)




################################################################################

# Percentage/number of people who maxed out even though they were parole eligible

# Obtained from NCRP releases (ncrp_releases)

################################################################################

ncrp_releases_maxout_2020 <- ncrp_releases_2020 %>%
  filter(!is.na(mand_prisrel_year) &
           !is.na(parelig_year) &
           !is.na(relyr)) %>%
  mutate(maxout = ifelse(mand_prisrel_year == relyr, 1, 0),

         release_timing_type = case_when(
           mand_prisrel_year == relyr ~ "Released on Mandatory Release Year",
           mand_prisrel_year > relyr  ~ "Released Prior to Mandatory Release Year",
           mand_prisrel_year < relyr  ~ "Released After Mandatory Release Year",
           TRUE ~ "Other"),

         maxout_type = case_when(
           mand_prisrel_year == relyr & parelig_year < relyr  ~ "Maxed out and was Parole Eligible Prior to Release Year",
           mand_prisrel_year == relyr & parelig_year > relyr  ~ "Maxed out and was Parole Eligible After Release Year",
           mand_prisrel_year == relyr & parelig_year == relyr ~ "Maxed out and was Parole Eligible During Release Year",
           TRUE ~ "Other")
  ) %>%
  select(mand_prisrel_year, parelig_year, relyr, maxout, release_timing_type, maxout_type, everything())

# get number and proportion of people who maxed out even though they were parole eligible
releases_maxout_2020 <- ncrp_releases_maxout_2020 %>%
  group_by(state) %>%
  count(release_timing_type, maxout_type) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(release_timing_type, " <b>", prop_label, "</b>")) %>%
  mutate(tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "<b>",
                  release_timing_type,
                  "</b><br><br>",
                  "Number of People: <b>",
                  scales::comma(n),
                  "</b><br><br>",
                  "Percentage of People: <b>",
                  prop_label, "</b></b>", sep = ""))

data1 <- data.frame(
  id = c("Root", "A", "B", "C", "A1", "A2", "B1", "B2", "C1"),
  parent = c("", "Root", "Root", "Root", "A", "A", "B", "B", "C"),
  value = c(100, 40, 30, 30, 20, 20, 10, 20, 30)
)

data <- ncrp_releases_maxout_2020 %>%
  ungroup() %>%
  filter(state == "Georgia") %>%
  droplevels() %>%
  rename(parent = release_timing_type)

root <- data %>%
  summarise(value = n()) %>%
  mutate(parent = "",
         id = "Released from Prison")

A <- data %>%
  filter(parent == "Released Prior to Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released Prior to Mandatory Release Year",
         parent = "Released from Prison")

B <- data %>%
  filter(parent == "Released on Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released on Mandatory Release Year",
         parent = "Released from Prison")

C <- data %>%
  filter(parent == "Released After Mandatory Release Year") %>%
  summarise(value = n()) %>%
  mutate(id = "Released After Mandatory Release Year",
         parent = "Released from Prison")

ABC <- rbind(root, A, B, C)
ABC <- ABC %>% select(id, parent, value)

B123 <- data %>%
  filter(parent == "Released on Mandatory Release Year") %>%
  count(maxout_type) %>%
  mutate(id = maxout_type,
         parent = "Released on Mandatory Release Year") %>%
  select(id, parent, value = n)

ABC_all <- rbind(ABC, B123)

hchart(ABC_all, "sunburst", hcaes(id = id, parent = parent, value = value))








maxout_ratio_by_state_2020 <- ncrp_releases_maxout_2020 %>%
  group_by(state) %>%
  summarize(
    total_cases = n(),
    maxout_pe_prior_cases = sum(release_timing_type == "Maxed out and was Parole Eligible Prior to Release Year"),
    ratio = maxout_pe_prior_cases / total_cases,
    representation = 1 / ratio  # Calculating the "1 in X" representation
  )










################################################################################

# Highcharts for:
# Timing of release overall, by adm type, and by offense type
# Create a grouped percent bar chart for each offense type

################################################################################

# number and prop of people by adm type and timing of release
# create tooltip
ncrp_released_at_ped_2020 <- ncrp_releases_2020 %>%
  # remove states with NA's
  filter(!is.na(released_at_ped_status)) %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>"),
         n_label = formattable::comma(n, 0)) %>%
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

# assign x axis order
desired_order <- c("Released Before Parole Eligibility Year",
                   "Released on Parole Eligibility Year",
                   "Released After Parole Eligibility Year")

# get list of states
states <- unique(ncrp_released_at_ped_2020$state)

# number and prop of people by adm type, offense type, and timing of release
ncrp_released_at_ped_offgeneral_2020 <- ncrp_releases_2020 %>%
  filter(!is.na(released_at_ped_status) &
         !is.na(offgeneral)) %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, offgeneral, admtype) %>%
  count(released_at_ped_status) %>%
  mutate(prop = (n/sum(n))*100,
         prop_label = paste0(round(prop, 0), "%"),
         chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>"),
         n_label = formattable::comma(n, 0)) %>%
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


########
# Overall
########

all_bar_released_at_ped_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_released_at_ped_2020 %>% filter(state == "Georgia") %>%
    arrange(match(released_at_ped_status, desired_order))

  # assign color for each race
  df1$color <- case_when(df1$released_at_ped_status == "Released Before Parole Eligibility Year" ~ purple,
                         df1$released_at_ped_status == "Released on Parole Eligibility Year" ~ teal,
                         df1$released_at_ped_status == "Released After Parole Eligibility Year" ~ orange)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <-
    highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(released_at_ped_status), y = n, color = color),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.n_label:,.0f}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = df1$released_at_ped_status) %>%
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

all_bar_released_at_ped_2020 <- setNames(all_bar_released_at_ped_2020, states)


########
# Other/unspecified
########
all_bar_released_at_ped_other_2020 <-
  fnc_create_bar_chart_released_at_ped(selected_offgeneral = "Other/unspecified",
                                       accessibility_text = "TBD")

########
# Property
########
all_bar_released_at_ped_property_2020 <-
  fnc_create_bar_chart_released_at_ped(selected_offgeneral = "Property",
                                       accessibility_text = "TBD")

########
# Violent
########
all_bar_released_at_ped_violent_2020 <-
  fnc_create_bar_chart_released_at_ped(selected_offgeneral = "Violent",
                                       accessibility_text = "TBD")

########
# Public Order
########
all_bar_released_at_ped_publicorder_2020 <-
  fnc_create_bar_chart_released_at_ped(selected_offgeneral = "Public order",
                                       accessibility_text = "TBD")

########
# Drugs
########
all_bar_released_at_ped_drugs_2020 <-
  fnc_create_bar_chart_released_at_ped(selected_offgeneral = "Drugs",
                                       accessibility_text = "TBD")

























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

all_bar_los_overview_2020 <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_proportion_served_2020 %>%
    filter(state == x) %>%
    arrange(match(timesrvd_rel_vs_sentlgth, desired_order))

  # assign color for each race
  df1$color <- case_when(df1$timesrvd_rel_vs_sentlgth == "Less than Sentence Length Served" ~ yellow,
                         df1$timesrvd_rel_vs_sentlgth == "Full Sentence Length Served" ~ purple)
  df1$color <- htmltools::parseCssColors(df1$color)

  highcharts <- highchart() %>%
    hc_add_series(df1, type = "column",
                  hcaes(x = factor(timesrvd_rel_vs_sentlgth), y = n, color = color),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.n_label:,.0f}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) %>%
    hc_xAxis(categories = df1$timesrvd_rel_vs_sentlgth) %>%
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

all_bar_los_overview_2020 <- setNames(all_bar_los_overview_2020, states)

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


########
# Property
########
all_bar_los_property_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Property",
                           accessibility_text = "TBD")


########
# Violent
########
all_bar_los_violent_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Violent",
                           accessibility_text = "TBD")


########
# Public Order
########
all_bar_los_publicorder_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Public order",
                           accessibility_text = "TBD")


########
# Drugs
########
all_bar_los_drugs_2020 <-
  fnc_create_bar_chart_los(selected_offgeneral = "Drugs",
                           accessibility_text = "TBD")



################################################################################

# Save data

################################################################################

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

  save(all_bar_los_overview_2020,
       file=file.path(folder, "all_bar_los_overview_2020.rds"))
  save(all_bar_los_other_2020,
       file=file.path(folder, "all_bar_los_other_2020.rds"))
  save(all_bar_los_property_2020,
       file=file.path(folder, "all_bar_los_property_2020.rds"))
  save(all_bar_los_violent_2020,
       file=file.path(folder, "all_bar_los_violent_2020.rds"))
  save(all_bar_los_publicorder_2020,
       file=file.path(folder, "all_bar_los_publicorder_2020.rds"))
  save(all_bar_los_drugs_2020,
       file=file.path(folder, "all_bar_los_drugs_2020.rds"))

}
