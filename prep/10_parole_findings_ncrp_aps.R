#######################################
# Project: AV Parole
# File: parole_findings_ncrp.R
# Authors: Mari Roberts
# Date last updated: May 3, 2023 (MAR)
# Description:
#    Parole findings TBD tables and graphics for shiny app
#######################################



# # clean data
# term_records_clean <- ncrp_term_records %>%
#   mutate(
#     state = str_sub(state, 6, -1))
#
# # filter to people in Georgia
# # terms with an admission date of 1989 or after
# # excluded terms with an admission type other than a court commitment or probation revocation
# # excluded terms missing a PED and 6% of terms where the PED is before the admission date
# ga_term_data <- term_records_clean %>%
#   filter(state == "Georgia") %>%
#   filter(admityr >= 1989) %>%
#   filter(admtype == "New court commitment" | admtype ==  "Parole return/revocation") %>%
#   filter(!is.na(parelig_year) & parelig_year >= admityr)
#
# # create variable to flag people who were eligible for release but not released
# georgia  <- georgia %>%
#   mutate(time_between_ped_release = releaseyr - parelig_year)

########################################

# Line graph data showing the change in prison population
# and change in people released to parole

########################################

# get prison population by report year and state
# merge with APS data for releases to parole/entries to parole from prison
# aps_parole_2000_2018 table created in parole_aps.R
ncrp_aps_pop_released_to_parole_by_year <- ncrp_yearendpop %>%
  filter(rptyear >= 2000) %>%
  group_by(rptyear, state) %>%
  summarise(total_prison_population = n()) %>%
  ungroup() %>%
  left_join(aps_parole_2000_2018,
            by = c("state", "rptyear"))


