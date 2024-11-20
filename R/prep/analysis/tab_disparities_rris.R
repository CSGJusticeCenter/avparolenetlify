################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: November 15, 2024 (MAR)
# Description:
#    RRI visualizations and findings for disparities tab
################################################################################

# ---------------------------------------------------------------------------- #
# Helper Function for Generating RRI Sentences
# ---------------------------------------------------------------------------- #

fnc_calculate_rri <- function(data, comparison_group, category) {
  # Calculate reference rate for the comparison group
  reference_rate_data <- data |>
    filter(!!sym(category) == comparison_group) |>
    select(state, past_pe_rate, rptyear) |>
    rename(reference_past_pe_rate = past_pe_rate)

  # Calculate RRI for all groups
  rri_data <- data |>
    inner_join(reference_rate_data, by = "state") |>
    mutate(rri = round(past_pe_rate / reference_past_pe_rate, 1)) |>
    select(state, !!sym(category), rri)

  return(rri_data)
}

fnc_generate_rri_sentences <- function(data, category, label, color) {
  comparison_group <- if (category == "race") "White people" else "females"
  comparison_color <- if (category == "race") red else purple

  map(unique(data$state), function(state_name) {
    df1 <- data |> filter(state == state_name, !!sym(category) == label)
    if (nrow(df1) == 0 || is.na(df1$rri)) return("")

    rri <- df1$rri
    if (rri > 1) {
      paste0("In ", rpytyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
             "</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:",
             color, "; font-weight:bold;'>", rri, " times higher</span> than <span style='color:",
             comparison_color, "; font-weight:bold;'>", comparison_group, "</span>, when accounting for prison population sizes in ", state_name, ".")
    } else {
      percent_less <- round((1 - rri) * 100, 0)
      paste0("In ", rpytyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
             "</span> were <span style='color:", color, "; font-weight:bold;'>", percent_less,
             " percent less likely</span> to be incarcerated in state prison past parole eligibility compared to <span style='color:",
             comparison_color, "; font-weight:bold;'>", comparison_group, "</span>, when accounting for population sizes in ", state_name, ".")
    }
  }) |> setNames(unique(data$state))
}

# ---------------------------------------------------------------------------- #
# Data Preparation and Filtering
# ---------------------------------------------------------------------------- #

# Apply filtering criteria and prepare data
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(
  data = ncrp_yearendpop_consolidated, exclude = states_to_exclude, dont_filter = states_nofilter
)

# Handle race/ethnicity filtering
ncrp_yearendpop_race <- ncrp_yearendpop_filtered |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  group_by(state) |>
  filter(
    race %in% ifelse(
      state %in% states_use_other_race_eth$state,
      c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic"),
      c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")
    )
  )

# Calculate prison populations and past parole eligibility by race
prison_pop_by_race <- ncrp_yearendpop_race |>
  group_by(state, rptyear, race) |>
  summarise(total_prison_pop = n(), .groups = "drop") |>
  fnc_filter_by_year(which_overall_year) |>
  select(-c(rptyear, year_to_use))

prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_race |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear, race) |>
  summarise(n = n(), .groups = "drop") |>
  fnc_filter_by_year(which_overall_year) |>
  select(-year_to_use)

# Merge and calculate past parole eligibility rate
merged_prison_pop_data_race <- prison_pop_by_race |>
  left_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) |>
  mutate(past_pe_rate = n / total_prison_pop)

# Calculate RRI by race
all_pe_rri_data <- fnc_calculate_rri(merged_prison_pop_data_race, comparison_group = "White, non-Hispanic", category = "race") |>
  # add_row(state = "Hawaii", race = "Other race(s), non-Hispanic", rri = 1.3)  # Test data##########################################
  mutate(rri = case_when(state == "Hawaii" & race == "Other race(s), non-Hispanic" ~ 1.3, TRUE ~ rri))

all_pe_rri_data_filtered <- all_pe_rri_data |>
  filter(rri > 1 | rri < 1)

# Sentence generation for race
all_sentence_pe_rri_black    <- fnc_generate_rri_sentences(all_pe_rri_data_filtered, "race", "Black, non-Hispanic", teal)
all_sentence_pe_rri_hispanic <- fnc_generate_rri_sentences(all_pe_rri_data_filtered, "race", "Hispanic, any race", blue)
all_sentence_pe_rri_other    <- fnc_generate_rri_sentences(all_pe_rri_data_filtered, "race", "Other race(s), non-Hispanic", purple)

# Add note for Hispanic RRI
all_sentence_pe_rri_hispanic <- map(all_sentence_pe_rri_hispanic, ~ paste0(.,
                                                                           "<br><span style='color: gray; font-size: 0.8em;'><i>Hispanic RRI should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity.</i></span>"
))
all_sentence_pe_rri_hispanic$Georgia
all_sentence_pe_rri_black$Georgia
all_sentence_pe_rri_other$Hawaii

# ---------------------------------------------------------------------------- #
# Data Preparation and Sentence Generation for Sex
# ---------------------------------------------------------------------------- #

# Calculate prison populations and past parole eligibility by sex
prison_pop_by_sex <- ncrp_yearendpop_filtered |>
  group_by(state, sex, rptyear) |>
  summarise(total_prison_pop = n(), .groups = "drop") |>
  fnc_filter_by_year(which_overall_year) |>
  select(-c(rptyear, year_to_use))

prison_pop_past_parole_elig_by_sex <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, sex, rptyear) |>
  summarise(n = n(), .groups = "drop") |>
  fnc_filter_by_year(which_overall_year) |>
  select(-year_to_use)

# Merge and calculate past parole eligibility rate by sex
merged_prison_pop_data_sex <- prison_pop_by_sex |>
  left_join(prison_pop_past_parole_elig_by_sex, by = c("state", "sex")) |>
  mutate(past_pe_rate = n / total_prison_pop)

# Calculate RRI by sex
all_pe_rri_data_male <- fnc_calculate_rri(merged_prison_pop_data_sex, "Female", "sex")

all_pe_rri_data_male_filtered <- all_pe_rri_data_male |>
  filter(rri > 1 | rri < 1)

# Sentence generation for sex
all_sentence_pe_rri_male <- fnc_generate_rri_sentences(all_pe_rri_data_male_filtered, "sex", "Male", teal)
all_sentence_pe_rri_male$Georgia



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

# Function to create and save infographics
fnc_create_and_save_infographic <- function(data, color, prefix) {
  states <- unique(data$state)

  map(.x = states, .f = function(x) {
    df_state <- data |> filter(state == x)

    # Create the infographic
    fnc_create_infographic(df_state$rri, color)

    # Format the state name to lowercase and replace spaces with underscores
    formatted_state <- str_to_lower(str_replace_all(x, " ", "_"))

    # Save the infographic with the formatted state name
    file_path <- file.path(png_folder, paste0(prefix, formatted_state, ".png"))
    ggsave(file_path, plot = last_plot(), width = 8, height = 6, dpi = 300)

    # Load, crop, and save the image
    img <- image_read(file_path)
    img_cropped <- image_trim(img)
    image_write(img_cropped, file_path)
  })
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

# Male
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


# # ---------------------------------------------------------------------------- #
# # Past Parole Eligibility
# # ---------------------------------------------------------------------------- #
#
# fnc_generate_rri_sentences <- function(data, category, label, color) {
#   map(.x = unique(data$state), .f = function(state_name) {
#     df1 <- data |> filter(state == state_name, !!sym(category) == label)
#
#     if (nrow(df1) > 0) {
#       rri <- df1$rri
#       comparison_group <- if (category == "race") "White people" else "females"
#       comparison_color <- if (category == "race") red else purple
#
#       if (!is.na(rri)) { # Check if rri is not NA
#         if (rri > 1) {
#           paste0("In ", select_year, ", <span style='color:", color, "; font-weight:bold;'>", label, "</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:", color, "; font-weight:bold;'>",
#                  rri, " times higher</span> than <span style='color:", comparison_color, "; font-weight:bold;'>", comparison_group, "</span>, when accounting for prison population sizes in ", state_name, ".")
#         } else {
#           percent_less <- round((1 - rri) * 100, 0)
#           paste0("In ", select_year, ", <span style='color:", color, "; font-weight:bold;'>", label, "</span> were <span style='color:", color, "; font-weight:bold;'>",
#                  percent_less, " percent less likely</span> to be incarcerated in state prison past parole eligibility compared to <span style='color:", comparison_color, "; font-weight:bold;'>", comparison_group, "</span>, when accounting for population sizes in ", state_name, ".")
#         }
#       } else {
#         ""
#       }
#     } else {
#       ""
#     }
#   }) |> setNames(unique(data$state))
# }
#
# # Apply initial filtering criteria
# ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(
#   data = ncrp_yearendpop_consolidated,
#   exclude = states_to_exclude,
#   dont_filter = states_nofilter
# )
#
# # Filter for race/ethnicity and handle state-specific race requirements
# ncrp_yearendpop_race <- ncrp_yearendpop_filtered |>
#   fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
#   group_by(state) |>
#   filter(
#     ifelse(
#       state %in% states_use_other_race_eth$state,
#       race %in% c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic"),
#       race %in% c("Black, non-Hispanic", "Hispanic, any race", "White, non-Hispanic")
#     )
#   )
#
# # Calculate prison population by race
# prison_pop_by_race <- ncrp_yearendpop_race |>
#   group_by(state, rptyear, race) |>
#   summarise(total_prison_pop = n(), .groups = "drop") |>
#   fnc_filter_by_year(which_overall_year) |>
#   select(-c(rptyear, year_to_use))
#
# # Filter for current parole eligibility status and calculate population
# prison_pop_past_parole_elig_by_race <- ncrp_yearendpop_race |>
#   filter(parelig_status == "Current") |>
#   group_by(state, rptyear, race) |>
#   summarise(n = n(), .groups = "drop") |>
#   fnc_filter_by_year(which_overall_year) |>
#   select(-year_to_use)
#
# # Merge and calculate rates
# merged_prison_pop_data <- prison_pop_by_race |>
#   left_join(prison_pop_past_parole_elig_by_race, by = c("state", "race")) |>
#   rename(past_pe_population = n) |>
#   mutate(past_pe_rate = past_pe_population / total_prison_pop)
#
# # Reference rate for White, non-Hispanic individuals
# reference_past_pe_rate <- merged_prison_pop_data |>
#   filter(race == "White, non-Hispanic") |>
#   select(state, past_pe_rate) |>
#   rename(reference_past_pe_rate = past_pe_rate)
#
# # Calculate RRI for all racial groups
# all_pe_rri_data <- merged_prison_pop_data |>
#   inner_join(reference_past_pe_rate, by = "state") |>
#   mutate(rri = round(past_pe_rate / reference_past_pe_rate, 1)) |>
#   select(state, race, rri) |>
#   filter(rri != 1) |>
#   add_row(state = "Hawaii", race = "Other race(s), non-Hispanic", rri = 1.3) # Test data
#
# # Generate sentences for each group
# all_sentence_pe_rri_black    <- fnc_generate_rri_sentences(all_pe_rri_data, "race", "Black, non-Hispanic", teal)
# all_sentence_pe_rri_hispanic <- fnc_generate_rri_sentences(all_pe_rri_data, "race", "Hispanic, any race", blue)
# all_sentence_pe_rri_other    <- fnc_generate_rri_sentences(all_pe_rri_data, "race", "Other race(s), non-Hispanic", purple)
#
# # Notes for Hispanic RRI
# all_sentence_pe_rri_hispanic <-
#   map(all_sentence_pe_rri_hispanic, ~ paste0(.,
#   "<br><span style='color: gray; font-size: 0.8em;'><i>Hispanic RRI should be interpreted with caution due to inconsistencies in how each state collects and reports data on ethnicity.</i></span>"
#   ))
#
# # Get total prison pop by state and rptyear
# prison_pop_by_sex <- ncrp_yearendpop_filtered |>
#   group_by(state, sex, rptyear) |>
#   summarise(total_prison_pop = n(), .groups = "drop") |>
#   fnc_filter_by_year(which_overall_year) |>
#   select(-c(rptyear, year_to_use))
#
# # Get current PE pop by state and rptyear
# prison_pop_past_parole_elig_by_sex <- ncrp_yearendpop_filtered |>
#   filter(parelig_status == "Current") |>
#   group_by(state, sex, rptyear) |>
#   summarise(n = n(), .groups = "drop") |>
#   fnc_filter_by_year(which_overall_year) |>
#   select(-c(rptyear, year_to_use))
#
# # Merge with parole eligibility data
# merged_prison_pop_data <- prison_pop_by_sex |>
#   left_join(prison_pop_past_parole_elig_by_sex, by = c("state", "sex")) |>
#   rename(past_pe_population = n) |>
#   # Calculate rate of people past parole eligibility
#   mutate(past_pe_rate = past_pe_population / total_prison_pop
#   )
#
# # Calculate the reference rate for females
# reference_past_pe_rate <- merged_prison_pop_data |>
#   filter(sex == "Female") |>
#   select(state, past_pe_rate) |>
#   rename(reference_past_pe_rate = past_pe_rate)
#
# # Calculate RRI for males
# all_pe_rri_data_male <- merged_prison_pop_data |>
#   inner_join(reference_past_pe_rate, by = "state") |>
#   mutate(rri = past_pe_rate / reference_past_pe_rate,
#          rri = round(rri, 1)) |>
#   select(state, sex, rri)
#
# all_sentence_pe_rri_male <- fnc_generate_rri_sentences(all_pe_rri_data_male, "sex", "Male", teal)

################################################################################
# Project: AV Parole
# File: tab_disparities.R
# Authors: Mari Roberts
# Date last updated: November 15, 2024 (MAR)
# Description:
#    RRI visualizations and findings for disparities tab
################################################################################
# # ---------------------------------------------------------------------------- #
# # PEOPLE INFOGRAPHICS FOR RRI's
# # ---------------------------------------------------------------------------- #
#
# # Image setup
# whichimage <- "person-2745706-bw"
# wd <- getwd()
#
# # Make sure you have the correct image path
# if (whichimage == "person-2745706-bw"){
#   px_h <- 521
#   px_w <- 323
#   ex_h <- 0.005
#   ex_w <- 0.02
#   img_ar_hw <- (px_h*(1+ex_h)) / (px_w*(1+ex_w))
#   img_ar_wh <- (px_w*(1+ex_w)) / (px_h*(1+ex_h))
#   rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
#   img <- ifelse(rawimg == 0, 1, 0)
# }
#
# # Set up colors
# light_color  <- darkgray
# empty_color   <- "#FFFFFF"
# default_ncols <- 15
#
# pe_rri_incarceration <- all_pe_rri_data |>
#   filter(race != "White, non-Hispanic") |>
#   select(state, race, rri) |>
#   filter(rri > 1 | rri < 1)
#
# pe_rri_incarceration_black <- pe_rri_incarceration |>
#   filter(race == "Black, non-Hispanic")
#
# # RRI infographic for Black, non-Hispanic
# states <- unique(pe_rri_incarceration_black$state)
# map(.x = states, .f = function(x) {
#   df_state <- pe_rri_incarceration_black |>
#     filter(state == x)
#
#   fnc_create_infographic(df_state$rri, color4)
#
#   # Save the infographic
#   ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")),
#          plot = last_plot(), width = 8, height = 6, dpi = 300)
#
#   # Load the saved image
#   img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")))
#
#   # Crop the image
#   img_cropped <- image_trim(img)
#
#   # Save the cropped image
#   image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_black_", x, ".png")))
# })
#
# pe_rri_incarceration_hispanic <- pe_rri_incarceration |>
#   filter(race == "Hispanic, any race")
#
# # RRI infographic for Hispanic, any race
# states <- unique(pe_rri_incarceration_hispanic$state)
# map(.x = states, .f = function(x) {
#   df_state <- pe_rri_incarceration_hispanic |>
#     filter(state == x)
#
#   fnc_create_infographic(df_state$rri, color2)
#
#   # Save the infographic
#   ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")),
#          plot = last_plot(), width = 8, height = 6, dpi = 300)
#
#   # Load the saved image
#   img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")))
#
#   # Crop the image
#   img_cropped <- image_trim(img)
#
#   # Save the cropped image
#   image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_hispanic_", x, ".png")))
# })
#
# pe_rri_incarceration_other <- pe_rri_incarceration |>
#   filter(race == "Other race(s), non-Hispanic")
#
# # RRI infographic for Other race(s), non-Hispanic
# states <- unique(pe_rri_incarceration_other$state)
# map(.x = states, .f = function(x) {
#   df_state <- pe_rri_incarceration_other |>
#     filter(state == x)
#
#   fnc_create_infographic(df_state$rri, color4)
#
#   # Save the infographic
#   ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_other_", x, ".png")),
#          plot = last_plot(), width = 8, height = 6, dpi = 300)
#
#   # Load the saved image
#   img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_other_", x, ".png")))
#
#   # Crop the image
#   img_cropped <- image_trim(img)
#
#   # Save the cropped image
#   image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_other_", x, ".png")))
# })
#
# pe_rri_incarceration_male <- all_pe_rri_data_male |>
#   filter(sex == "Male") |>
#   select(state, sex, rri) |>
#   filter(rri > 1 | rri < 1)
#
# # RRI infographic for male
# states <- unique(pe_rri_incarceration_male$state)
# map(.x = states, .f = function(x) {
#   df_state <- pe_rri_incarceration_male |>
#     filter(state == x)
#
#   fnc_create_infographic(df_state$rri, color4)
#
#   # Save the infographic
#   ggsave(file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")),
#          plot = last_plot(), width = 8, height = 6, dpi = 300)
#
#   # Load the saved image
#   img <- image_read(file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")))
#
#   # Crop the image
#   img_cropped <- image_trim(img)
#
#   # Save the cropped image
#   image_write(img_cropped, file.path(app_folder, paste0("pngs/pe_rri_infographic_male_", x, ".png")))
# })
#

