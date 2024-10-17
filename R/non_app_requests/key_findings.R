

# 10/15/2024
# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility - Sentences
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Select racial and ethnic groups of interest
ncrp_current_pe <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  filter(rptyear == select_year &
           parelig_status == "Current")

# Get average time between PE and release by state and race
avg_current_pe_race <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race %in% c("White, non-Hispanic",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic")),
         # all are negative or zero since they are past parole eligibility
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Get average time between PE and release by state and race
avg_current_pe_race_by_state <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  filter(race %in% c("White, non-Hispanic",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic")),
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(state, race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

temp <- avg_current_pe_race_by_state |>
  select(state, race, avg_years_to_estimated_pey) |>
  spread(race, avg_years_to_estimated_pey) %>%
  mutate(diff_avg_years = `Black, non-Hispanic` - `White, non-Hispanic`)



# Get average time between PE and release by state and race and offense
avg_current_pe_race_offense <- ncrp_current_pe |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ "Other or Unknown")) |>
  filter(race %in% c("White, non-Hispanic",
                     "Black, non-Hispanic")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic")),
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # change negative to positive, negative means past parole eligibility year
  group_by(race, group) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")


temp <- avg_current_pe_race_offense |>
  select(race, group, avg_years_to_estimated_pey) |>
  spread(race, avg_years_to_estimated_pey) %>%
  mutate(diff_avg_years = `Black, non-Hispanic` - `White, non-Hispanic`)

















# ---------------------------------------------------------------------------- #
# Years Spent in Prison After Parole Eligibility by Race and Ethnicity, and Offense
# ---------------------------------------------------------------------------- #

# Filter to states with parole systems
# Remove missing data
ncrp_pe_release <- fnc_filter_pe_population_criteria(ncrp_releases) |>
  filter(rptyear == select_year) |>
  filter(!is.na(time_between_ped_rptyear) &
           !is.na(estimated_pey) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))

# Get total time between PE and release by race and offense nationally
ncrp_total_pe_release_race_offense <- ncrp_pe_release |>
  filter(!is.na(race) & !is.na(fbi_index)) |>
  group_by(race, fbi_index) |>
  summarise(total_years = sum(time_between_ped_release, na.rm = TRUE),
            avg_years = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Total time between PE and release by race nationally
ncrp_total_pe_release_race <- ncrp_pe_release |>
  filter(!is.na(race) & !is.na(fbi_index)) |>
  group_by(race) |>
  summarise(total_years = sum(time_between_ped_release, na.rm = TRUE),
            avg_years = mean(time_between_ped_release, na.rm = TRUE),
            people_released = n(),
            .groups = "drop")

# Function to check for existing versions and generate a new versioned filename
generate_versioned_filename <- function(base_filename, dir = deliverables_folder) {
  # Get list of files in the specific directory
  existing_files <- list.files(path = dir, pattern = paste0(base_filename, "_v\\d+\\.csv"))

  # If no existing files, start with version 1
  if (length(existing_files) == 0) {
    return(file.path(dir, paste0(base_filename, "_v1.csv")))
  } else {
    # Extract the current highest version number
    version_numbers <- str_extract(existing_files, "_v\\d+")
    version_numbers <- as.numeric(str_remove_all(version_numbers, "_v"))

    # Increment to next version
    new_version <- max(version_numbers) + 1
    return(file.path(dir, paste0(base_filename, "_v", new_version, ".csv")))
  }
}

# Create base filename
base_filename <- paste0("ncrp_total_pe_release_race_offense_", select_year)

# Generate versioned filename with a specific folder path
output_filename <- generate_versioned_filename(base_filename, dir = deliverables_folder)

# Save the dataframe to the specific folder
write_csv(ncrp_total_pe_release_race_offense, output_filename)

# Log the file saving
message("Data successfully saved as: ", output_filename)




# ---------------------------------------------------------------------------- #
# Disparities in Average Time Spent Post-Eligibility by Race
# ---------------------------------------------------------------------------- #

data <- ncrp_total_pe_release_race_offense

# Check the unique race values in the data
unique(data$race)

# Filter the data for Black and White individuals
black_data <- data %>% filter(race == "Black, non-Hispanic")
white_data <- data %>% filter(race == "White, non-Hispanic")

# Summarize key statistics for Black and White individuals
black_summary <- black_data %>%
  summarize(
    total_years_past_pe = sum(total_years),
    avg_years_past_pe = mean(avg_years),
    total_people_released = sum(people_released)
  )

white_summary <- white_data %>%
  summarize(
    total_years_past_pe = sum(total_years),
    avg_years_past_pe = mean(avg_years),
    total_people_released = sum(people_released)
  )

# Print the summaries
print("Black individuals summary:")
print(black_summary)

print("White individuals summary:")
print(white_summary)


















data <- ncrp_total_pe_release_race_offense

# Define violent offenses (adjust as needed)
violent_offenses <- c("Murder and Non-negligent Manslaughter",
                      "Negligent Manslaughter",
                      "Rape or Sexual Assault",
                      "Robbery",
                      "Aggravated or Simple Assault",
                      "Other Violent Offenses")

# Define non-violent offenses (adjust as needed)
non_violent_offenses <- c("Property",
                          "Public order",
                          "Drugs",
                          "Other")

# Filter data for Black, White, and Hispanic individuals for violent offenses
black_violent <- data %>% filter(race == "Black, non-Hispanic", fbi_index %in% violent_offenses)
white_violent <- data %>% filter(race == "White, non-Hispanic", fbi_index %in% violent_offenses)
hispanic_violent <- data %>% filter(race == "Hispanic, any race", fbi_index %in% violent_offenses)

# Filter data for Black, White, and Hispanic individuals for non-violent offenses
black_nonviolent <- data %>% filter(race == "Black, non-Hispanic", fbi_index %in% non_violent_offenses)
white_nonviolent <- data %>% filter(race == "White, non-Hispanic", fbi_index %in% non_violent_offenses)
hispanic_nonviolent <- data %>% filter(race == "Hispanic, any race", fbi_index %in% non_violent_offenses)

# Summarize key statistics for violent offenses
black_violent_summary <- black_violent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

white_violent_summary <- white_violent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

hispanic_violent_summary <- hispanic_violent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

# Summarize key statistics for non-violent offenses
black_nonviolent_summary <- black_nonviolent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

white_nonviolent_summary <- white_nonviolent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

hispanic_nonviolent_summary <- hispanic_nonviolent %>%
  summarize(total_years_past_pe = sum(total_years), avg_years_past_pe = mean(avg_years), total_people_released = sum(people_released))

# Calculate percentage differences in average time served (violent and non-violent) compared to White individuals
violent_black_vs_white <- ((black_violent_summary$avg_years_past_pe - white_violent_summary$avg_years_past_pe) / white_violent_summary$avg_years_past_pe) * 100
violent_hispanic_vs_white <- ((hispanic_violent_summary$avg_years_past_pe - white_violent_summary$avg_years_past_pe) / white_violent_summary$avg_years_past_pe) * 100

nonviolent_black_vs_white <- ((black_nonviolent_summary$avg_years_past_pe - white_nonviolent_summary$avg_years_past_pe) / white_nonviolent_summary$avg_years_past_pe) * 100
nonviolent_hispanic_vs_white <- ((hispanic_nonviolent_summary$avg_years_past_pe - white_nonviolent_summary$avg_years_past_pe) / white_nonviolent_summary$avg_years_past_pe) * 100

# Print the summaries for each racial group for both violent and non-violent offenses
print("Black individuals - violent offenses summary:")
print(black_violent_summary)

print("White individuals - violent offenses summary:")
print(white_violent_summary)

print("Hispanic individuals - violent offenses summary:")
print(hispanic_violent_summary)

print("Black individuals - non-violent offenses summary:")
print(black_nonviolent_summary)

print("White individuals - non-violent offenses summary:")
print(white_nonviolent_summary)

print("Hispanic individuals - non-violent offenses summary:")
print(hispanic_nonviolent_summary)

# Print percentage differences for violent offenses
print(paste("Black individuals served", round(violent_black_vs_white, 2), "% more time past parole eligibility for violent offenses compared to White individuals."))
print(paste("Hispanic individuals served", round(violent_hispanic_vs_white, 2), "% more time past parole eligibility for violent offenses compared to White individuals."))

# Print percentage differences for non-violent offenses
print(paste("Black individuals served", round(nonviolent_black_vs_white, 2), "% more time past parole eligibility for non-violent offenses compared to White individuals."))
print(paste("Hispanic individuals served", round(nonviolent_hispanic_vs_white, 2), "% more time past parole eligibility for non-violent offenses compared to White individuals."))



















