#######################################
# Project: AV Parole
# File: tab_releases_from_prison.R
# Authors: Mari Roberts
# Date last updated: August 15, 2023 (MAR)
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

# assign x axis order
desired_order <- c("Released Before Parole Eligibility Year",
                   "Released on Parole Eligibility Year",
                   "Released After Parole Eligibility Year")

# get list of states
states <- unique(ncrp_released_at_ped_2020$state)

########
# Overall
########
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

########
# Other/unspecified
########
all_bar_released_at_ped_other_2020 <-
  fnc_create_all_percent_bar_chart_pestatus_admtype(selected_offgeneral = "Other/unspecified")

########
# Property
########
all_bar_released_at_ped_property_2020 <-
  fnc_create_all_percent_bar_chart_pestatus_admtype(selected_offgeneral = "Property")

########
# Violent
########
all_bar_released_at_ped_violent_2020 <-
  fnc_create_all_percent_bar_chart_pestatus_admtype(selected_offgeneral = "Violent")

########
# Public Order
########
all_bar_released_at_ped_publicorder_2020 <-
  fnc_create_all_percent_bar_chart_pestatus_admtype(selected_offgeneral = "Public order")

########
# Drugs
########
all_bar_released_at_ped_drugs_2020 <-
  fnc_create_all_percent_bar_chart_pestatus_admtype(selected_offgeneral = "Drugs")
























################################################################################

# Highcharts for:
# Unconditional vs conditional release

################################################################################

# subset to 2020 report
ncrp_release_type <- ncrp_releases %>%
  filter(admtype == "Parole return/revocation" |
           admtype == "New court commitment") %>%
  filter(reltype == "Unconditional release" |
           reltype == "Conditional release") %>%
  filter(!is.na(proportion_served))

df1 <- ncrp_release_type %>%
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




# # number and prop of people by adm type and timing of release
# # create tooltip
# ncrp_released_at_ped_admtype_2020 <- ncrp_releases_2020 %>%
#   # remove states with NA's
#   dplyr::filter(!is.na(released_at_ped_status) &
#            !is.na(admtype)) %>%
#   filter(admtype == "Parole return/revocation" |
#            admtype == "New court commitment") %>%
#   group_by(state, admtype) %>%
#   count(released_at_ped_status) %>%
#   mutate(prop = (n/sum(n))*100,
#          prop_label = paste0(round(prop, 0), "%"),
#          chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Timing of Release: <b>",
#                   released_at_ped_status,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))
#
# # number and prop of people by adm type, offense type, and timing of release
# ncrp_released_at_ped_offgeneral_2020 <- ncrp_releases_2020 %>%
#   filter(!is.na(released_at_ped_status) &
#          !is.na(offgeneral)) %>%
#   filter(admtype == "Parole return/revocation" |
#          admtype == "New court commitment") %>%
#   group_by(state, offgeneral, admtype) %>%
#   count(released_at_ped_status) %>%
#   mutate(prop = (n/sum(n))*100,
#          prop_label = paste0(round(prop, 0), "%"),
#          chart_label = paste0(released_at_ped_status, " <b>", prop_label, "</b>")) %>%
#   mutate(tooltip =
#            paste0("<b>", state, "</b><br><br>",
#                   "Timing of Release: <b>",
#                   released_at_ped_status,
#                   "</b><br><br>",
#                   "Number of People: <b>",
#                   scales::comma(n),
#                   "</b><br><br>",
#                   "Percentage of People: <b>",
#                   prop_label, "</b></b>", sep = ""))




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
}
