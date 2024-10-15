#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: October 14, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#    Focusing on RRIs
#######################################


# ---------------------------------------------------------------------------- #
# Time Served - Sentences
# ---------------------------------------------------------------------------- #

# Calculate average time served by race, ethnicity, and state
# Remove states without parole systems and with high missingness
# (states_to_exclude created in prep/import_format.R)
los_race <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  # Only include these racial and ethnic groups
  filter(race %in% c("White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic")) |>
  group_by(state, race) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# SENTENCE: "In 2020, Black people spent an average of 0.7 more years in prison,
#            and Hispanic people spent an average of 1.5 more years in
#            prison compared to White people."
all_sentence_los_race <- fnc_generate_los_disparity_sentences(los_race, "in prison", "race", "average_los")
all_sentence_los_race$Georgia

# Calculate average time served by sex and state
los_sex <- fnc_filter_population(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(sex != "Unknown") |>
  group_by(state, sex) |>
  summarise(average_los = mean(time_between_admisson_release, na.rm = TRUE),
            .groups = "drop")

# SENTENCE: "In 2020, females spent an average of 1 year fewer in prison compared
#            to males in Georgia."
all_sentence_los_sex <- fnc_generate_los_disparity_sentences(los_sex, "in prison", "sex", "average_los")
all_sentence_los_sex$Georgia





# ---------------------------------------------------------------------------- #
# Time Served - Lollipop Charts
# ---------------------------------------------------------------------------- #

# Generate charts by race
states_race <- unique(los_race$state)
all_lollipop_los_race <- map(.x = states_race, .f = function(x) {
  fnc_create_lollipop_chart(
    df = los_race,
    group_var = "race",
    group_labels = c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race"),
    colors = c(color1, color4, color2),
    state_name = x
  )
})
all_lollipop_los_race <- setNames(all_lollipop_los_race, states_race)
all_lollipop_los_race$Georgia

# Generate charts by sex
states_sex <- unique(los_sex$state)
all_lollipop_los_sex <- map(.x = states_sex, .f = function(x) {
  fnc_create_lollipop_chart(
    df = los_sex,
    group_var = "sex",
    group_labels = c("Male", "Female"),
    colors = c(color4, color2),
    state_name = x
  )
})
all_lollipop_los_sex <- setNames(all_lollipop_los_sex, states_sex)
all_lollipop_los_sex$Georgia


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility - Sentences
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Select racial and ethnic groups of interest
ncrp_current_pe <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  filter(rptyear == select_year &
         parelig_status == "Current")

# Get average time between PE and release by state and sex
avg_current_pe_sex <- ncrp_current_pe |>
  filter(!is.na(sex)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, sex) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# SENTENCE: "In 2020, females spent an average of 1.9 less years past parole
#            eligibility compared to males in Georgia."
all_sentence_avg_past_pe_sex <- fnc_generate_los_disparity_sentences(df = avg_current_pe_sex,
                                                                     type = "past parole eligibility",
                                                                     compare_var = "sex",
                                                                     los_col = "avg_years_to_estimated_pey")
all_sentence_avg_past_pe_sex$Georgia


# Get average time between PE and release by state and race
avg_current_pe_race <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race %in% c("White, non-Hispanic",
                     "Hispanic, any race",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "Hispanic, any race",
                                  "White, non-Hispanic")),
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# SENTENCE: "In 2020, Black people spent an average of 0.7 more years past parole
#            eligibility, and Hispanic people spent an average of 0.7 more years
#            past parole eligibility compared to White people."
all_sentence_avg_past_pe_race <- fnc_generate_los_disparity_sentences(df = avg_current_pe_race,
                                                                      type = "past parole eligibility",
                                                                      compare_var = "race",
                                                                      los_col = "avg_years_to_estimated_pey")
all_sentence_avg_past_pe_race$Georgia


# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  avg_current_pe_race           = "avg_current_pe_race.rds",
  avg_current_pe_sex            = "avg_current_pe_sex.rds",

  all_sentence_los_race         = "all_sentence_los_race.rds",
  all_lollipop_los_race         = "all_lollipop_los_race.rds",
  all_sentence_los_sex          = "all_sentence_los_sex.rds",
  all_lollipop_los_sex          = "all_lollipop_los_sex.rds",

  all_sentence_avg_past_pe_race = "all_sentence_avg_past_pe_race.rds",
  all_sentence_avg_past_pe_sex  = "all_sentence_avg_past_pe_sex.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))





# fnc_generate_los_disparity_sentences <- function(df, type, compare_var, los_col, year = select_year) {
#   # Get unique states to iterate over
#   states <- unique(df$state)
#
#   # Generate sentence for each state
#   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#
#     if (compare_var == "race") {
#
#       # Filter and categorize races within the data
#       df1 <- df |>
#         dplyr::ungroup() |>
#         dplyr::mutate(race = dplyr::case_when(
#           race == "White, non-Hispanic" ~ "White",
#           race == "Black, non-Hispanic" ~ "Black",
#           race == "Hispanic, any race" ~ "Hispanic"
#         )) |>
#         dplyr::filter(state == state_var)
#
#       # Handle missing data for the state
#       if (nrow(df1) == 0) {
#         return(paste0("No data available for ", state_var, "."))
#       }
#
#       # Focus on comparisons with White individuals
#       df_white <- df1 |> dplyr::filter(race == "White")
#
#       # Initialize variables to hold sentences for each race comparison
#       black_sentence <- ""
#       hispanic_sentence <- ""
#
#       # Generate sentence for Black vs White comparison
#       df_black <- df1 |> dplyr::filter(race == "Black")
#       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#         los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
#         if (!is.na(los_diff_black)) {
#           abs_los_diff_black <- round(abs(los_diff_black), 1)
#           if (los_diff_black > 0) {
#             black_sentence <- paste0("Black people spent an average of ",
#                                      abs_los_diff_black, " more years ", type)
#           } else if (los_diff_black < 0) {
#             black_sentence <- paste0("Black people spent an average of ",
#                                      abs_los_diff_black,
#                                      if (abs_los_diff_black == 1) " less year" else " less years",
#                                      " ", type)
#           }
#         }
#       }
#
#       # Generate sentence for Hispanic vs White comparison
#       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#         los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
#         if (!is.na(los_diff_hispanic)) {
#           abs_los_diff_hispanic <- round(abs(los_diff_hispanic), 1)
#           if (los_diff_hispanic > 0) {
#             hispanic_sentence <- paste0("Hispanic people spent an average of ",
#                                         abs_los_diff_hispanic, " more years ", type)
#           } else if (los_diff_hispanic < 0) {
#             hispanic_sentence <- paste0("Hispanic people spent an average of ",
#                                         abs_los_diff_hispanic,
#                                         if (abs_los_diff_hispanic == 1) " less year" else " less years",
#                                         " ", type)
#           }
#         }
#       }
#
#       # Combine both sentences, or indicate no significant differences
#       if (black_sentence != "" && hispanic_sentence != "") {
#         if (abs_los_diff_black == abs_los_diff_hispanic) {
#           sentence <- paste0("In ", year, ", Black people and Hispanic people spent an average of ",
#                              abs_los_diff_black, " more years ", type, " compared to White people.")
#         } else {
#           sentence <- paste0("In ", year, ", ", black_sentence, ", and ",
#                              hispanic_sentence, " compared to White people.")
#         }
#       } else if (black_sentence != "") {
#         sentence <- paste0("In ", year, ", ", black_sentence, " compared to White people.")
#       } else if (hispanic_sentence != "") {
#         sentence <- paste0("In ", year, ", ", hispanic_sentence, " compared to White people.")
#       } else {
#         sentence <- "" # No significant differences
#       }
#
#       return(sentence)
#     } else {
#       return("Invalid comparison variable.")
#     }
#   })
#
#   # Assign state names to list
#   all_sentences <- setNames(all_sentences, states)
#
#   return(all_sentences)
# }

