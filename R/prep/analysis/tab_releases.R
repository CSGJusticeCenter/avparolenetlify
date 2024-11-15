################################################################################
# Project: AV Parole
# File: tab_releases.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    Prison releases visualizations and findings for releases tab
#    Uses BJS Prisoners Data
################################################################################





# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

current_releases <- ncrp_releases_not_consolidated |>  # ncrp_releases_consolidated |>#####################################################might need to temp change this until data is ready
  fnc_filter_by_year(which_overall_year)

current_releases_not_consolidated <- ncrp_releases_not_consolidated |>
  fnc_filter_by_year(which_overall_year)

ncrp_releases_race       <- fnc_summarize_data(current_releases, "race") |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
ncrp_releases_sex        <- fnc_summarize_data(current_releases, "sex")
ncrp_releases_agerlse   <- fnc_summarize_data(current_releases, "agerlse") ################# might need to change to agerelease
ncrp_releases_fbi_index  <- fnc_summarize_data(current_releases, "fbi_index") |>
  fnc_group_offense_type()
ncrp_releases_sentlgth   <- fnc_summarize_data(current_releases, "sentlgth")

# List of parameters for each category
categories <- list(
  list(data = ncrp_releases_race, x_var = "race", metric = "Race and Ethnicity"),
  list(data = ncrp_releases_sex, x_var = "sex", metric = "Sex"),
  list(data = ncrp_releases_agerlse, x_var = "agerlse", metric = "Age"),
  list(data = ncrp_releases_sentlgth, x_var = "sentlgth", metric = "Sentence Length"),
  list(data = ncrp_releases_fbi_index, x_var = "fbi_index", metric = "Offense Type")
)

# ---------------------------------------------------------------------------- #
# Generate Sentences and Column Charts (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Initialize empty lists to store bar charts and sentences
all_bar_releases <- list()
all_sentence_releases <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  all_bar_releases[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,
    x_var      = category$x_var,
    metric     = category$metric,
    type_desc  = "released from prison",
    title_type = "People Released from Prison",
    y_var      = "prop"
  )

  all_sentence_releases[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type_desc = "released from prison"
  )
}

# Access specific bar charts and sentences
all_bar_releases_race <- all_bar_releases[["race"]]
all_sentence_releases_race <- all_sentence_releases[["race"]]
all_bar_releases_sex <- all_bar_releases[["sex"]]
all_sentence_releases_sex <- all_sentence_releases[["sex"]]
all_bar_releases_agerlse <- all_bar_releases[["agerlse"]]
all_sentence_releases_agerlse <- all_sentence_releases[["agerlse"]]
all_bar_releases_sentlgth <- all_bar_releases[["sentlgth"]]
all_sentence_releases_sentlgth <- all_sentence_releases[["sentlgth"]]
all_bar_releases_fbi_index <- all_bar_releases[["fbi_index"]]
all_sentence_releases_fbi_index <- all_sentence_releases[["fbi_index"]]
