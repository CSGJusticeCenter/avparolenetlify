# ---------------------------------------------------------------------------- #
# Past Parole Eligibility
# ---------------------------------------------------------------------------- #

# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# of 1+ years except life
ncrp_yearendpop_race <- fnc_filter_pe_population_criteria(ncrp_yearendpop) |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
# remove states with high missingness for race and ethnicity

# Get total prison pop by state and rptyear
prison_pop_by_race <- ncrp_yearendpop_race |>
  filter(rptyear == select_year & race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) |>
  group_by(state, race) |>
  summarise(total_prison_pop = n(), .groups = "drop")

# Get current PE pop by state and rptyear
prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_race |>
  filter(rptyear == select_year & race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")) |>
  filter(parelig_status == "Current") |>
  group_by(state, race) |>
  summarise(n = n(), .groups = "drop")

# Merge with parole eligibility data
merged_prison_pop_data <- prison_pop_by_race |>
  left_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) |>
  rename(past_pe_population = n) |>
  # Calculate rate of people past parole eligibility per 100,000
  mutate(past_pe_rate = past_pe_population / total_prison_pop #* 100000
  )

# Calculate the reference rate for White, non-Hispanic individuals
reference_past_pe_rate <- merged_prison_pop_data |>
  filter(race == "White, non-Hispanic") |>
  select(state, past_pe_rate) |>
  rename(reference_past_pe_rate = past_pe_rate)

# Calculate RRI for other racial groups
all_pe_rri_data <- merged_prison_pop_data |>
  inner_join(reference_past_pe_rate, by = "state") |>
  mutate(rri = past_pe_rate / reference_past_pe_rate,
         rri = round(rri, 1)) |>
  select(state, race, rri) |>
  filter(rri > 1 | rri < 1)

states <- unique(all_pe_rri_data$state)

# RRI for Black people
# SENTENCE
# Generate sentence for each state
all_sentence_pe_rri_black <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_pe_rri_data |>
    filter(state == x, race == "Black, non-Hispanic")

  # Generate the sentence only if the RRI for Black people is greater than 1.
  if (nrow(df1) > 0) {
    if (df1$rri > 1) {
      final_sentence <- paste0("In ", select_year, ", <span style='color:#49a7a1; font-weight:bold;'>Black people</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                               round(df1$rri, 1), " times higher</span> than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for prison population sizes in ", x, ".")
    } else if (df1$rri < 1) {
      # Calculate percentage for less likelihood
      less_likely <- round((1 - df1$rri) * 100, 0)
      final_sentence <- paste0("In ", select_year, ", <span style='color:#49a7a1; font-weight:bold;'>Black people</span> were <span style='color:#49a7a1; font-weight:bold;'>",
                               less_likely, " percent less likely</span> to be incarcerated in state prison past parole eligibility than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for population sizes in ", x, ".")
    } else {
      final_sentence <- paste0("")
    }
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
  df1 <- all_pe_rri_data |>
    filter(state == x, race == "Hispanic, any race")

  # Generate the sentence only if the RRI for Hispanic people is greater than 1.
  if (nrow(df1) > 0) {
    if (df1$rri > 1) {
      final_sentence <- paste0("In ", select_year, ", <span style='color:#55b4e5; font-weight:bold;'>Hispanic people</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#55b4e5; font-weight:bold;'>",
                               round(df1$rri, 1), " times higher</span> than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for prison population sizes in ", x, ".",
                               "<br><span style='color: gray; font-size: 0.8em;'><i>Hispanic RRI should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity.</i></span>")
    } else if (df1$rri < 1) {
      # Calculate percentage for less likelihood
      less_likely <- round((1 - df1$rri) * 100, 0)
      final_sentence <- paste0("In ", select_year, ", <span style='color:#55b4e5; font-weight:bold;'>Hispanic people</span> were <span style='color:#55b4e5; font-weight:bold;'>",
                               less_likely, " percent less likely</span> to be incarcerated in state prison past parole eligibility than <span style='color:#d97d68; font-weight:bold;'>White people</span>, when accounting for prison population sizes in ", x, ".",
                               "<br><span style='color: gray; font-size: 0.8em;'><i>Hispanic RRI should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity.</i></span>")
    } else {
      final_sentence <- paste0("")
    }
  } else {
    final_sentence <- paste0("")
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_pe_rri_hispanic <- setNames(all_sentence_pe_rri_hispanic, states)
all_sentence_pe_rri_hispanic$Wyoming
all_sentence_pe_rri_hispanic$Georgia

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

# Set up colors
light_color  <- darkgray
empty_color   <- "#FFFFFF"
default_ncols <- 15

pe_rri_incarceration <- all_pe_rri_data |>
  filter(race != "White, non-Hispanic" &
           race != "Other race(s), non-Hispanic") |>
  select(state, race, rri) |>
  filter(rri > 1 | rri < 1)

pe_rri_incarceration_black <- pe_rri_incarceration |>
  filter(race == "Black, non-Hispanic")

pe_rri_incarceration_hispanic <- pe_rri_incarceration |>
  filter(race == "Hispanic, any race")

# Create infographics and save them as PNGs for each state (male RRI)
# Takes 10 minutes to run
states <- unique(pe_rri_incarceration_black$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_incarceration_black |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color4)

  # Save the infographic
  ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")))
})

# Create infographics and save them as PNGs for each state (Hispanic RRI)
states <- unique(pe_rri_incarceration_hispanic$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_incarceration_hispanic |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color2)

  # Save the infographic
  ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")))
})













# Filter NCRP year end pop to people in prison for new crimes and with sentence lengths
# of 1+ years except life
ncrp_yearendpop_sex <- fnc_filter_pe_population_criteria(ncrp_yearendpop)

# Get total prison pop by state and rptyear
prison_pop_by_sex <- ncrp_yearendpop_sex |>
  filter(rptyear == select_year) |>
  group_by(state, sex) |>
  summarise(total_prison_pop = n(), .groups = "drop")

# Get current PE pop by state and rptyear
prison_pop_past_parole_elig_by_sex <- ncrp_yearendpop_sex |>
  filter(rptyear == select_year) |>
  filter(parelig_status == "Current") |>
  group_by(state, sex) |>
  summarise(n = n(), .groups = "drop")

# Merge with parole eligibility data
merged_prison_pop_data <- prison_pop_by_sex |>
  left_join(prison_pop_past_parole_elig_by_sex, by = c("state", "sex")) |>
  rename(past_pe_population = n) |>
  # Calculate rate of people past parole eligibility per 100,000
  mutate(past_pe_rate = past_pe_population / total_prison_pop #* 100000
  )

# Calculate the reference rate for females
reference_past_pe_rate <- merged_prison_pop_data |>
  filter(sex == "Female") |>
  select(state, past_pe_rate) |>
  rename(reference_past_pe_rate = past_pe_rate)

# Calculate RRI for other racial groups
all_pe_rri_data_male <- merged_prison_pop_data |>
  inner_join(reference_past_pe_rate, by = "state") |>
  mutate(rri = past_pe_rate / reference_past_pe_rate,
         rri = round(rri, 1)) |>
  select(state, sex, rri)

states <- unique(all_pe_rri_data_male$state)

# RRI for females
# SENTENCE
# Generate sentence for each state
all_sentence_pe_rri_male <- map(.x = states, .f = function(x) {

  # Filter the RRI data for the specific state.
  df1 <- all_pe_rri_data_male |>
    filter(state == x, sex == "Male")

  # Generate the sentence based on RRI value.
  if (nrow(df1) > 0 && !is.na(df1$rri)) { # Check if rri is not NA
    if (df1$rri > 1) {
      final_sentence <- paste0("In ", select_year, ", <span style='color:#49a7a1; font-weight:bold;'>males </span> were incarcerated in state prison past parole eligibility at a rate <span style='color:#49a7a1; font-weight:bold;'>",
                               df1$rri, " times higher</span> than <span style='color:#55b4e5; font-weight:bold;'>females</span>, when accounting for prison population sizes in ", x, ".")
    } else if (df1$rri < 1) {
      percent_less <- (1 - df1$rri) * 100
      final_sentence <- paste0("In ", select_year, ", <span style='color:#49a7a1; font-weight:bold;'>males </span> were <span style='color:#49a7a1; font-weight:bold;'>",
                               percent_less, " percent less likely</span> to be incarcerated in state prison past parole eligibility compared to <span style='color:#55b4e5; font-weight:bold;'>females</span>, when accounting for prison population sizes in ", x, ".")
    } else {
      final_sentence <- ""
    }
  } else {
    final_sentence <- ""
  }

  return(final_sentence)
})

# Assign state names to the generated sentences for each state.
all_sentence_pe_rri_male <- setNames(all_sentence_pe_rri_male, states)
all_sentence_pe_rri_male$Georgia
all_sentence_pe_rri_male$`North Dakota`






pe_rri_incarceration_male <- all_pe_rri_data_male |>
  filter(sex == "Male") |>
  select(state, sex, rri) |>
  filter(rri > 1 | rri < 1)

# Create infographics and save them as PNGs for each state (Hispanic RRI)
states <- unique(pe_rri_incarceration_male$state)
map(.x = states, .f = function(x) {
  df_state <- pe_rri_incarceration_male |>
    filter(state == x)

  fnc_create_infographic(df_state$rri, color4)

  # Save the infographic
  ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")),
         plot = last_plot(), width = 8, height = 6, dpi = 300)

  # Load the saved image
  img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")))

  # Crop the image
  img_cropped <- image_trim(img)

  # Save the cropped image
  image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")))
})

# ---------------------------------------------------------------------------- #
# Save Data
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_rri_black    = "all_sentence_pe_rri_black.rds",
  all_sentence_pe_rri_hispanic = "all_sentence_pe_rri_hispanic.rds",
  all_sentence_pe_rri_male     = "all_sentence_pe_rri_male.rds",
  all_pe_rri_data              = "all_pe_rri_data.rds",
  all_pe_rri_data_male         = "all_pe_rri_data_male.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
