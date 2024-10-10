#######################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    Prison disparities visualizations and findings for disparities tab
#    Focusing on RRIs
#######################################


# ---------------------------------------------------------------------------- #
# RRIs

# The Relative Rate Index (RRI) compares the rate of an event (e.g., incarceration)
# between groups. It is calculated as the ratio of the rate for the target group to
# the rate for the reference group (usually White, non-Hispanic).
# RRI = 1 means equal rates, RRI > 1 means higher rate for the target group (disparity exists),
# and RRI < 1 means a lower rate for the target group.
# ---------------------------------------------------------------------------- #

# These are the IDs of race variables that we want to pull from tidycensus.
# Each ID corresponds to a specific racial/ethnic group from the decennial census data.
# 18+ years - https://api.census.gov/data/2020/dec/pl/variables.html
race_vars <- c(estimate_white              = "P4_005N",
               estimate_black              = "P4_006N",
               estimate_hispanic           = "P4_002N")

# List of state names used to pull census data for each state
states <- state.name
states <- state_notes |>
  filter(abolished_parole == "N", state %in% states) |>
  pull(state)

# Using lapply to apply the function `fnc_get_census_data` for each state in `states`.
# This will return a list where each element contains the processed census data for a state.
census_state_race_population_list <- lapply(states, fnc_get_census_data)

# Convert the list of state-level census data into a single data frame.
# Each state's data is stacked row-wise.
census_state_race_population <- bind_rows(census_state_race_population_list)

# Add a "state" column to the dataframe to identify which state each row belongs to.
# We repeat the state names for each state's corresponding number of rows.
census_state_race_population$state <- rep(states, each = nrow(census_state_race_population) / length(states))

# Grouping the census data by both state and race.
# Summing the population of each racial group across states (na.rm = TRUE ensures missing values are ignored).
census_state_race_population <- census_state_race_population |>
  group_by(state, race) |>
  summarise(state_population = sum(value, na.rm = TRUE), .groups = "drop")

# Merge the census data with prison population data by state and race.
# The prison data (`bjs_prison_pop_by_race_2020`) contains the number of people incarcerated by race in each state.
# After merging, the result will have both population and prison population data.
merged_data <- census_state_race_population %>%
  inner_join(bjs_prison_pop_by_race_2020, by = c("state", "race")) |>
  rename(prison_population = n) |>
  # Calculate the incarceration rate per 100,000 people for each racial group in each state.
  # This helps to compare the incarceration levels while accounting for the population sizes.
  mutate(incarceration_rate = prison_population / state_population * 100000)

# Reference Rate Calculation:
# To compute Relative Rate Index (RRI), we first select the incarceration rate for White, non-Hispanic people
# in each state, which will serve as the baseline or reference rate for other groups.
reference_rate <- merged_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, incarceration_rate) %>%
  rename(reference_rate = incarceration_rate)  # Rename for clarity.

# Calculate RRI (Relative Rate Index) by dividing the incarceration rate of each racial group by the reference rate.
# This gives us a comparison of how much more or less likely other racial groups are incarcerated compared to Whites.
all_rri_data <- merged_data %>%
  inner_join(reference_rate, by = "state") %>%
  mutate(rri = incarceration_rate / reference_rate,
         rri = round(rri, 1)) %>%  # Calculate the RRI.
  select(state, race, rri)








# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# of 1+ years except life
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  # remove Louisiana and Alabama
  filter(state != "Alabama" & state != "Louisiana")

# Get total prison pop by state and rptyear
prison_pop_by_race <- ncrp_yearendpop_filtered |>
  filter(rptyear == 2020 & race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) |>
  group_by(state, race) |>
  summarise(total_prison_pop = n(), .groups = "drop")

# Get current PE pop by state and rptyear
prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_filtered |>
  filter(rptyear == 2020 & race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) |>
  filter(parelig_status == "Current") |>
  group_by(state, race) |>
  summarise(n = n(), .groups = "drop")

# Merge with parole eligibility data
merged_prison_pop_data <- prison_pop_by_race %>%
  inner_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) %>%
  rename(past_pe_population = n) |>
  # Calculate rate of people past parole eligibility per 100,000
  mutate(past_pe_rate = past_pe_population / total_prison_pop #* 100000
         )

# Calculate the reference rate for White, non-Hispanic individuals
reference_past_pe_rate <- merged_prison_pop_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, past_pe_rate) %>%
  rename(reference_past_pe_rate = past_pe_rate)

# Calculate RRI for other racial groups
all_pe_rri_data <- merged_prison_pop_data %>%
  inner_join(reference_past_pe_rate, by = "state") %>%
  mutate(rri = past_pe_rate / reference_past_pe_rate,
         rri = round(rri, 1)) %>%
  select(state, race, rri)



states <- unique(all_pe_rri_data$state)

# RRI for Black people
# SENTENCE
# Generate sentence for each state
all_sentence_pe_rri_black <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_pe_rri_data %>%
    filter(state == x, race == "Black, non-Hispanic")

  # Generate the sentence only if the RRI for Black people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("In 2020, <span style='color:#49a7a1; font-weight:bold;'>Black people</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                             round(df1$rri, 1), " times</span> higher than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_pe_rri_black <- setNames(all_sentence_pe_rri_black, states)
all_sentence_pe_rri_black$Georgia

# SENTENCE
# Generate sentence for each state
states <- unique(all_pe_rri_data$state)
all_sentence_pe_rri_hispanic <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_pe_rri_data %>%
    filter(state == x, race == "Hispanic, any race")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("In 2020, <span style='color:#55b4e5; font-weight:bold;'>Hispanic people</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#55b4e5; font-weight:bold;'>",
                             round(df1$rri, 1), "</span> times higher than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_pe_rri_hispanic <- setNames(all_sentence_pe_rri_hispanic, states)
all_sentence_pe_rri_hispanic$Wyoming


# ---------------------------------------------------------------------------- #
# PEOPLE INFOGRAPHICS FOR RRI's
# ---------------------------------------------------------------------------- #

pe_rri_greater_than_1 <- all_pe_rri_data |>
  filter(race != "White, non-Hispanic" &
           race != "Other race(s), non-Hispanic" &
           rri > 1) |>
  select(state, race, rri)

pe_rri_greater_than_1_black <- pe_rri_greater_than_1 |>
  filter(race == "Black, non-Hispanic")

pe_rri_greater_than_1_hispanic <- pe_rri_greater_than_1 |>
  filter(race == "Hispanic, any race")

# Create infographics and save them as PNGs for each state (Black RRI)
# Takes 5 minutes to run
states <- unique(pe_rri_greater_than_1_black$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_greater_than_1_black |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color4)

  # Save the infographic
  ggsave(file.path(app_folder, paste0("pe_rri_infographic_black_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_folder, paste0("pe_rri_infographic_black_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_folder, paste0("pe_rri_infographic_black_", x, ".png")))
})

# Create infographics and save them as PNGs for each state (Hispanic RRI)
states <- unique(pe_rri_greater_than_1_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_greater_than_1_hispanic |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color2)

  # Save the infographic
  ggsave(file.path(app_folder, paste0("pe_rri_infographic_hispanic_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_folder, paste0("pe_rri_infographic_hispanic_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_folder, paste0("pe_rri_infographic_hispanic_", x, ".png")))
})





# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_rri_black    = "all_sentence_pe_rri_black",
  all_sentence_pe_rri_hispanic = "all_sentence_pe_rri_hispanic",
  all_pe_rri_data              = "all_pe_rri_data"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
