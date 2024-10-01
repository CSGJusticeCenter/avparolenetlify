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
states <- carl_state_notes |>
  filter(abolished_parole_16_total == "N", state %in% states) |>
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
  rename(prison_population = n)

# Calculate the incarceration rate per 100,000 people for each racial group in each state.
# This helps to compare the incarceration levels while accounting for the population sizes.
merged_data <- merged_data %>%
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
  mutate(rri = incarceration_rate / reference_rate) %>%  # Calculate the RRI.
  select(state, race, rri)





# RRI sentence for Black people
states <- unique(all_rri_data$state)
all_sentence_rri_black <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_rri_data %>%
    filter(state == x, race == "Black, non-Hispanic")

  # Generate the sentence only if the RRI for Black people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("In 2020, <span style='color:#49a7a1; font-weight:bold;'>Black people</span> were incarcerated in state prison at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                             round(df1$rri, 1), " times</span> higher than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_rri_black <- setNames(all_sentence_rri_black, states)
all_sentence_rri_black$Georgia

# RRI sentence for Hispanic people
states <- unique(all_rri_data$state)
all_sentence_rri_hispanic <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_rri_data %>%
    filter(state == x, race == "Hispanic, any race")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("In 2020, <span style='color:#55b4e5; font-weight:bold;'>Hispanic people</span> were incarcerated in state prison at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                             round(df1$rri, 1), "</span> times higher than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_rri_hispanic <- setNames(all_sentence_rri_hispanic, states)
all_sentence_rri_hispanic$Georgia



# ---------------------------------------------------------------------------- #
# PEOPLE INFOGRAPHICS FOR RRI's
# ---------------------------------------------------------------------------- #

# Image setup
whichimage <- "person-2745706-bw"
wd <- getwd()

# Make sure you have the correct image path
if (whichimage == "person-2745706-bw"){
  px_h <- 521
  px_w <- 323
  ex_h <- 0.005
  ex_w <- 0.02
  img_ar_hw <- (px_h*(1+ex_h)) / (px_w*(1+ex_w))
  img_ar_wh <- (px_w*(1+ex_w)) / (px_h*(1+ex_h))
  rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0)
}

rri_greater_than_1 <- all_rri_data |>
  filter(race != "White, non-Hispanic" &
           race != "Other race(s), non-Hispanic" &
           rri > 1) |>
  select(state, race, rri)

rri_greater_than_1_black <- rri_greater_than_1 |>
  filter(race == "Black, non-Hispanic")

rri_greater_than_1_hispanic <- rri_greater_than_1 |>
  filter(race == "Hispanic, any race")

# Set up colors
light_color  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 15

# Create infographics and save them as PNGs for each state (Black RRI)
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_black$state)
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_black |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color4)

  # Save the infographic
  ggsave(file.path(app_path, paste0("rri_infographic_black_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_path, paste0("rri_infographic_black_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_path, paste0("rri_infographic_black_", x, ".png")))
})

# Create infographics and save them as PNGs for each state (Hispanic RRI)
# Takes 5 minutes to run
states <- unique(rri_greater_than_1_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- rri_greater_than_1_hispanic |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color1)

  # Save the infographic
  ggsave(file.path(app_path, paste0("rri_infographic_hispanic_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_path, paste0("rri_infographic_hispanic_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_path, paste0("rri_infographic_hispanic_", x, ".png")))
})




# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

save(all_sentence_rri_black,    file = file.path(app_folder, "all_sentence_rri_black.rds"))
save(all_sentence_rri_hispanic, file = file.path(app_folder, "all_sentence_rri_hispanic.rds"))
save(all_rri_data,              file = file.path(app_folder, "all_rri_data.rds"))



