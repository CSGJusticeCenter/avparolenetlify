################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: December 5, 2024 (MAR)
# Description:
#    RRI visualizations and findings for Disparities tab
################################################################################

# ---------------------------------------------------------------------------- #
# Data Preparation on RRIs
# ---------------------------------------------------------------------------- #

# Filter the consolidated year-end prison population data for specific criteria
ncrp_yearendpop_filtered <- ncrp_yearendpop_consolidated |>
  filter(!state %in% states_to_exclude$state) |>  # Exclude states with abolished parole or high missingness
  # Only include people in prison for new court commitments and sentence lengths greater than 1 year but not life
  # Also allow Unknowns in this case to mirror Seba Guzman's methodology on RRIs in Stata
  filter(
    !admtype %in% c("Other", "Parole return/revocation") &
    !sentlgth_raw %in% c("< 1 year", "Life, LWOP, Life plus additional years, Death")
  )

# Exclude states with high missingness for race and ethnicity and filter by state-specific conditions
ncrp_yearendpop_race <- ncrp_yearendpop_filtered |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  group_by(state) |>  # Group by state for state-specific filtering
  filter(
    # Filter to White, Hispanic, and Black for all states except states in states_use_other_race_eth
    race %in% ifelse(
      state %in% states_use_other_race_eth$state,  # For states requiring "Other race(s)"
      c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic"),
      c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")  # Default race categories
    )
  )

# Summarize the total prison population by state, year, and race
prison_pop_by_race <- ncrp_yearendpop_race |>
  group_by(state, rptyear, race) |>
  summarise(
    total_prison_pop = n(),  # Count the total population for each group
    .groups = "drop"         # Avoid grouped output in the result
  ) |>
  fnc_filter_by_year(which_overall_year) |>  # Filter for the most relevant year
  select(-c(rptyear, year_to_use))

# Calculate the population past parole eligibility by state, year, and race
prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_race |>
  filter(parelig_status == "Current") |>  # Include only individuals past their parole eligibility year
  group_by(state, rptyear, race) |>
  summarise(
    n = n(),  # Count the number of individuals past eligibility
    .groups = "drop"
  ) |>
  fnc_filter_by_year(which_overall_year) |>
  select(-year_to_use)

# Merge total prison population and past parole eligibility data to calculate rates
merged_prison_pop_data_race <- prison_pop_by_race |>
  left_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) |>  # Join by state and race
  mutate(past_pe_rate = n / total_prison_pop)  # Calculate the rate of individuals past parole eligibility

# ---------------------------------------------------------------------------- #
# Relative Rate Index (RRI) Calculation for Racial Groups
# ---------------------------------------------------------------------------- #

# Calculate RRI using the merged data, comparing each group to White individuals
all_pe_rri_data <- fnc_calculate_rri(
  merged_prison_pop_data_race,
  comparison_group = "White, non-Hispanic",  # Set "White, non-Hispanic" as the reference group
  category = "race") |>
  mutate(
    rri = case_when(
      TRUE ~ rri  # Retain calculated RRI otherwise
    )
  )

# Filter RRI data to include only disparities (RRI > 1 or RRI < 1)
all_pe_rri_data_filtered <- all_pe_rri_data |>
  filter(rri > 1 | rri < 1)  # Exclude groups with no disparity (RRI = 1)

# Generate disparity sentences for specific racial groups
all_sentence_pe_rri_black <- fnc_generate_rri_sentences(
  all_pe_rri_data_filtered, "race", "Black, non-Hispanic", teal
)
all_sentence_pe_rri_hispanic <- fnc_generate_rri_sentences(
  all_pe_rri_data_filtered, "race", "Hispanic, any race", blue
)
all_sentence_pe_rri_other <- fnc_generate_rri_sentences(
  all_pe_rri_data_filtered, "race", "Other race(s), non-Hispanic", purple
)

# Add a disclaimer to Hispanic RRI sentences about data inconsistencies
all_sentence_pe_rri_hispanic <- map(
  all_sentence_pe_rri_hispanic,
  ~ paste0(
    .,
    "<br><span style='color: gray; font-size: 0.8em;'><i>Analysis of disparities for Hispanic people should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity.</i></span>"
  )
)

# Example states:
# all_sentence_pe_rri_hispanic$Georgia
# all_sentence_pe_rri_black$Georgia
# all_sentence_pe_rri_other$Hawaii

# ---------------------------------------------------------------------------- #
# Data Preparation for Sex-Based Disparity Analysis
# ---------------------------------------------------------------------------- #

# Summarize the total prison population by state, year, and sex
prison_pop_by_sex <- ncrp_yearendpop_filtered |>
  group_by(state, sex, rptyear) |>
  summarise(
    total_prison_pop = n(),  # Count the total population for each sex group
    .groups = "drop"
  ) |>
  fnc_filter_by_year(which_overall_year) |>
  select(-c(rptyear, year_to_use))  # Remove unnecessary columns

# Calculate the population past parole eligibility by state, year, and sex
prison_pop_past_parole_elig_by_sex <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>  # Include only individuals past their parole eligibility year
  group_by(state, sex, rptyear) |>
  summarise(
    n = n(),  # Count the number of individuals past eligibility
    .groups = "drop"
  ) |>
  fnc_filter_by_year(which_overall_year) |>
  select(-year_to_use)

# Merge total prison population and past parole eligibility data to calculate rates
merged_prison_pop_data_sex <- prison_pop_by_sex |>
  left_join(prison_pop_past_parole_elig_by_sex, by = c("state", "sex")) |>  # Join by state and sex
  mutate(past_pe_rate = n / total_prison_pop)  # Calculate the rate of individuals past parole eligibility

# ---------------------------------------------------------------------------- #
# Relative Rate Index (RRI) Calculation for Male vs Female
# ---------------------------------------------------------------------------- #

# Calculate RRI for males compared to females
all_pe_rri_data_male <- fnc_calculate_rri(
  merged_prison_pop_data_sex,
  comparison_group = "Female",  # Use females as the reference group
  category = "sex"
)

# Filter RRI data to include only disparities
all_pe_rri_data_male_filtered <- all_pe_rri_data_male |>
  filter(rri > 1 | rri < 1)

# Generate disparity sentences for males
all_sentence_pe_rri_male <- fnc_generate_rri_sentences(
  all_pe_rri_data_male_filtered, "sex", "Male", teal
)

# Example state:
# all_sentence_pe_rri_male$Georgia
# all_sentence_pe_rri_male$Idaho

# ---------------------------------------------------------------------------- #
# Infographics for RRIs
# ---------------------------------------------------------------------------- #

# General setup
wd <- getwd()
whichimage <- "person-2745706-bw"

# Set up colors
light_color  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 15

# Image setup
if (whichimage == "person-2745706-bw") {
  px_h <- 521
  px_w <- 323
  ex_h <- 0.005
  ex_w <- 0.02
  img_ar_hw <- (px_h * (1 + ex_h)) / (px_w * (1 + ex_w))
  img_ar_wh <- (px_w * (1 + ex_w)) / (px_h * (1 + ex_h))
  rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0)
}

# Clear PNG folder
png_folder <- file.path(sp_data_path, "data/analysis/app/pngs")
if (dir.exists(png_folder)) {
  file.remove(list.files(png_folder, full.names = TRUE))
}

# Create infographics for different groups
pe_rri_incarceration <- all_pe_rri_data |>
  filter(race != "White, non-Hispanic") |>
  select(state, race, rri) |>
  filter(rri > 1 | rri < 1)

# Black, non-Hispanic
# Takes about 10 minutes to run
fnc_create_and_save_infographic(
  data = pe_rri_incarceration |> filter(race == "Black, non-Hispanic"),
  color = color4,
  prefix = "pe_rri_infographic_black_"
)

# Hispanic, any race
# Takes about 10 minutes to run
fnc_create_and_save_infographic(
  data = pe_rri_incarceration |> filter(race == "Hispanic, any race"),
  color = color2,
  prefix = "pe_rri_infographic_hispanic_"
)

# Other race(s), non-Hispanic
# Takes about 10 minutes to run
fnc_create_and_save_infographic(
  data = pe_rri_incarceration |> filter(race == "Other race(s), non-Hispanic"),
  color = color5,
  prefix = "pe_rri_infographic_other_"
)

# Create infographics for males
pe_rri_incarceration_male <- all_pe_rri_data_male |>
  filter(sex == "Male") |>
  select(state, sex, rri) |>
  filter(rri > 1 | rri < 1)

# Male
# Takes about 10 minutes to run
fnc_create_and_save_infographic(
  data = pe_rri_incarceration_male,
  color = color4,
  prefix = "pe_rri_infographic_male_"
)

# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_rri_black    = "all_sentence_pe_rri_black.rds",
  all_sentence_pe_rri_hispanic = "all_sentence_pe_rri_hispanic.rds",
  all_sentence_pe_rri_other    = "all_sentence_pe_rri_other.rds",
  all_sentence_pe_rri_male     = "all_sentence_pe_rri_male.rds",
  all_pe_rri_data              = "all_pe_rri_data.rds",
  all_pe_rri_data_male         = "all_pe_rri_data_male.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))


