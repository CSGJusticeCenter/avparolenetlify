#######################################
# Project: AV Parole
# File: predicted_probabilities.R
# Authors: Mari Roberts
# Date last updated: May 4, 2023 (MAR)
# Description:
#    Calculate predicted probabilities for each state
#    by race, sex, admtype, offgeneral, sentlngth

# Input:
# ncrp_releases_clean - created in releases_ncrp.R

# Output
# all_pp_by_variable - used for app
#######################################


library(dplyr)
library(broom)
library(broom.helpers)

# prepare data for analysis
# ncrp_releases_clean created in releases_ncrp.R
ncrp_pp_model_data <- ncrp_releases_clean %>%
  # filter to 2020 report year
  # remove releases that were classified as "other"
  filter(rptyear == 2020) %>%
  # filter(state == ) %>%
  filter(reltype != "Other release (including death, transfer, AWOL, escape)") %>%
  filter(admtype != "Other admission (including unsentenced, transfer, AWOL/escapee return)") %>%
  filter(race != "Other race(s), non-Hispanic") %>%
  # create binary variable for being released within 1 year of parole eligibility
  mutate(release_within_1yr_ped =
           case_when(time_between_release_ped <= 1 ~ 1,
                     time_between_release_ped > 1  ~ 0)) %>%

  # remove other offenses
  filter(offgeneral != "Other/unspecified") %>%

  # factor variables
  mutate(release_within_1yr_ped = factor(release_within_1yr_ped),
         state      = factor(state),
         sex        = factor(sex),
         race       = factor(race),
         admtype    = factor(admtype),
         offgeneral = factor(offgeneral, ordered = TRUE,
                             levels = c("Other/unspecified",
                                        "Public order",
                                        "Drug",
                                        "Property",
                                        "Violent")),
         sentlgth   = factor(sentlgth, ordered = TRUE,
                             levels = c("< 1 year",
                                        "1-1.9 years",
                                        "2-4.9 years",
                                        "5-9.9 years",
                                        "10-24.9 years",
                                        ">=25 years")))  %>%
  # set reference levels
  mutate(sex     = relevel(sex,     ref = "Male"),
         race    = relevel(race,    ref = "White, non-Hispanic"),
         admtype = relevel(admtype, ref = "New court commitment")) %>%

  # create id
  mutate(release_id = row_number()) %>%

  # remove missing data and remove offenses in the general category.
  filter(!is.na(release_within_1yr_ped) &
           !is.na(sex) &
           !is.na(race) &
           !is.na(admtype) &
           !is.na(offgeneral) &
           !is.na(sentlgth)) %>%

  # select variables
  select(state,
         release_within_1yr_ped,
         sex,
         race,
         admtype,
         offgeneral,
         sentlgth) %>%

  droplevels()

# https://druedin.com/2016/01/16/predicted-probabilities-in-r/
# save the losgitic regression formula
fmla <- release_within_1yr_ped ~ sex + race + admtype + offgeneral + sentlgth

# define the list of states to loop through, excluding multiple states
excluded_states <- c("California",
                     "Florida",
                     "Illinois",
                     "Louisiana" ,
                     "Minnesota",
                     "Missouri",
                     "Montana",
                     "Nebraska",
                     "Nevada",
                     "New York",
                     "New Hampshire",
                     "Wyoming")

states_list <- setdiff(unique(ncrp_pp_model_data$state), excluded_states)

# create an empty list to store the results for each state
state_pp_list <- list()

# loop through each state and calculate the predicted probabilities
for (state in states_list) {
  state_data <- ncrp_pp_model_data[ncrp_pp_model_data$state == state, ]

  # run logistic regression
  glm_model <- glm(fmla,
                   family = "binomial", data = state_data)

  # calculate the predicted probabilities for each combination of predictor variables
  new_data <- expand.grid(sex        = levels(state_data$sex),
                          race       = levels(state_data$race),
                          admtype    = levels(state_data$admtype),
                          offgeneral = levels(state_data$offgeneral),
                          sentlgth   = levels(state_data$sentlgth))
  new_data$release_within_1yr_ped <- predict(glm_model,
                                             newdata = new_data,
                                             type = "response")

  # save the predicted probabilities for each race
  pp_by_race <- aggregate(release_within_1yr_ped ~ race,
                          data = new_data, FUN = mean)

  # save the predicted probabilities for each sex
  pp_by_sex <- aggregate(release_within_1yr_ped ~ sex,
                         data = new_data, FUN = mean)

  # save the predicted probabilities for each admtype
  pp_by_admtype <- aggregate(release_within_1yr_ped ~ admtype,
                             data = new_data, FUN = mean)

  # save the predicted probabilities for each offgeneral
  pp_by_offgeneral <- aggregate(release_within_1yr_ped ~ offgeneral,
                                data = new_data, FUN = mean)

  # save the predicted probabilities for each sentlgth
  pp_by_sentlgth <- aggregate(release_within_1yr_ped ~ sentlgth,
                              data = new_data, FUN = mean)

  # add all results to the list for this state
  state_pp_list[[state]] <- list(pp_by_race,
                                 pp_by_sex,
                                 pp_by_admtype,
                                 pp_by_offgeneral,
                                 pp_by_sentlgth)
}

# create an empty dataframe to store the results for each state
all_pp_by_variable <- data.frame()

# loop through each state and create a row in the dataframe
# This code loops through each state, extracts the results for that state from
# state_pp_list, and adds a row to the all_pp_by_variable dataframe with the results
# for that state. The resulting dataframe has columns for the state, race,
# sex, admtype, offgeneral, sentlgth, and the corresponding predicted probabilities.
for (state in states_list) {
  # extract the results for this state from the list
  pp_by_race <- state_pp_list[[state]][[1]]
  pp_by_sex <- state_pp_list[[state]][[2]]
  pp_by_admtype <- state_pp_list[[state]][[3]]
  pp_by_offgeneral <- state_pp_list[[state]][[4]]
  pp_by_sentlgth <- state_pp_list[[state]][[5]]

  # add a row to the dataframe with the results for this state
  all_pp_by_variable <- rbind(all_pp_by_variable,
                       data.frame(state = state,
                                  race = pp_by_race$race,
                                  pp_by_race = pp_by_race$release_within_1yr_ped,
                                  sex = pp_by_sex$sex,
                                  pp_by_sex = pp_by_sex$release_within_1yr_ped,
                                  admtype = pp_by_admtype$admtype,
                                  pp_by_admtype = pp_by_admtype$release_within_1yr_ped,
                                  offgeneral = pp_by_offgeneral$offgeneral,
                                  pp_by_offgeneral = pp_by_offgeneral$release_within_1yr_ped,
                                  sentlgth = pp_by_sentlgth$sentlgth,
                                  pp_by_sentlgth = pp_by_sentlgth$release_within_1yr_ped))
}




##########
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(all_pp_by_variable, file=file.path(folder, "all_pp_by_variable.rds"))


}
