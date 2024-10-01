
# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# between 1-25 years
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop)

# Get current PE pop by state and rptyear
prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear, race) |>
  summarise(n = n(), .groups = "drop") |>
  filter(rptyear == 2020)

# Merge with parole eligibility data
merged_parole_data <- census_state_race_population %>%
  inner_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) %>%
  rename(past_parole_population = n)

# Calculate rate of people past parole eligibility per 100,000
merged_parole_data <- merged_parole_data %>%
  mutate(past_parole_rate = past_parole_population / state_population * 100000)

# Calculate the reference rate for White, non-Hispanic individuals
reference_parole_rate <- merged_parole_data %>%
  filter(race == "White, non-Hispanic") %>%
  select(state, past_parole_rate) %>%
  rename(reference_parole_rate = past_parole_rate)

# Calculate RRI for other racial groups
all_pe_rri_data <- merged_parole_data %>%
  inner_join(reference_parole_rate, by = "state") %>%
  mutate(rri = past_parole_rate / reference_parole_rate) %>%
  select(state, race, rri)


# RRI for Black people
states <- unique(all_pe_rri_data$state)
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

# RRI for Hispanic people
states <- unique(all_pe_rri_data$state)
all_sentence_pe_rri_hispanic <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_pe_rri_data %>%
    filter(state == x, race == "Hispanic, any race")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0 && df1$rri > 1) {
    final_sentence <- paste0("In 2020, <span style='color:#55b4e5; font-weight:bold;'>Hispanic people</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#49a7a1; font-weight:bold;'>",
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
  ggsave(file.path(app_path, paste0("pe_rri_infographic_black_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_path, paste0("pe_rri_infographic_black_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_path, paste0("pe_rri_infographic_black_", x, ".png")))
})

# Create infographics and save them as PNGs for each state (Hispanic RRI)
states <- unique(pe_rri_greater_than_1_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_greater_than_1_hispanic |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color1)

  # Save the infographic
  ggsave(file.path(app_path, paste0("pe_rri_infographic_hispanic_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_path, paste0("pe_rri_infographic_hispanic_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_path, paste0("pe_rri_infographic_hispanic_", x, ".png")))
})





# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

save(all_sentence_pe_rri_black,    file = file.path(app_folder, "all_sentence_pe_rri_black.rds"))
save(all_sentence_pe_rri_hispanic, file = file.path(app_folder, "all_sentence_pe_rri_hispanic.rds"))
save(all_pe_rri_data,              file = file.path(app_folder, "all_pe_rri_data.rds"))
