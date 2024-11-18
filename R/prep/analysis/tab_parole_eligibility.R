################################################################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: November 15, 2024 (MAR)
# Description:
################################################################################

# ---------------------------------------------------------------------------- #
# Pie charts of the prison population by parole eligibility status
# ---------------------------------------------------------------------------- #

# Function that filters the population data to include only people in prison for new crimes
# with sentence lengths 1+ years except life
# Only includes states with parole systems and without high missingness
# Includes states don't need to be filtered by admission type or sentence length
# These states are in states_nofilter
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(data = ncrp_yearendpop_consolidated,
                                                              exclude = states_to_exclude,
                                                              dont_filter = states_nofilter)

# Total prison population by state and year
total_pe_pop_by_rptyear <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop")

# Prison population by parole eligibility status (missing, current, eligible in the future)
pe_status_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(total_pe_pop_by_rptyear, by = c("state", "rptyear")) |>
  mutate(prop = (n / yearendpop)*100) |>
  fnc_create_tooltip(variable_label = "Parole Eligibility Status", variable = parelig_status) |>
  fnc_filter_by_year(which_overall_year)

# Pie chart showing proportion of people currently eligible, eligible in the future
# or missing PE information
# Generate pie charts for each state using the filtered `pe_status_pop`
all_pie_pe_type <- fnc_hc_pie_chart(
  df = pe_status_pop,
  variable = "parelig_status",
  source = ncrp_csg_source
)
all_pie_pe_type$Georgia

# Generate summary sentences for each state
all_sentence_pe_type <- {
  # Get unique states from the `pe_status_pop` data
  states <- unique(pe_status_pop$state)

  # Use `map` to iterate over each state and generate sentences
  map(states, function(state_name) {
    # Filter the data for the current state
    df <- pe_status_pop |>
      filter(state == state_name)

    # Extract `rptyear` for the current state (assuming it's consistent)
    select_year <- unique(df$rptyear)

    # Get the proportions of people currently eligible and those eligible in the future
    current_prop <- df |> filter(parelig_status == "Current") |> pull(prop)
    future_prop <- df |> filter(parelig_status == "Future") |> pull(prop)

    # Generate the summary sentence
    paste0(
      "In ", select_year, ", ",
      round(current_prop, 0),
      " percent of people in prison were currently past their parole eligibility.",
      " Another ", round(future_prop, 0),
      " percent will reach their parole eligibility after ", select_year, "."
    )
  }) |>
  setNames(states)
}
all_sentence_pe_type$Georgia



# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

pe_proj_pop <- ncrp_projections |>
  select(state, year, pcnt_ppey_rules_wf, proj_pcnt_ppey) |>
  mutate(
    # Determine the percentage past parole eligibility and projection
    pct_past_pe = if_else(
      year >= 2010 & year <= 2020 & !is.na(pcnt_ppey_rules_wf),
      pcnt_ppey_rules_wf,
      if_else(year >= 2019 & year <= 2020, NA_real_, NA_real_)
    ),
    proj_pct_past_pe = if_else(
      year > 2020 & year <= 2023 | (year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf)),
      proj_pcnt_ppey,
      NA_real_
    ),
    used_projected_flag = year >= 2019 & year <= 2020 & is.na(pcnt_ppey_rules_wf)
  )

# Generate sentences for all states
states <- unique(pe_proj_pop$state)
all_sentence_pop_pe_by_year <- map(states, ~ fnc_generate_projection_sentence(.x, pe_proj_pop)) |>
  setNames(states)
all_sentence_pop_pe_by_year$Georgia

# Generate chart for each state
all_line_pop_pe_by_year <- map(states, function(x) {
  df1 <- pe_proj_pop |>
    filter(state == x) |>
    mutate(
      last_value_past_pe = last(na.omit(pct_past_pe)),

      # Only calculate `year_to_fill` if there are non-NA projected values
      year_to_fill = if (any(!is.na(proj_pct_past_pe))) {
        min(year[!is.na(proj_pct_past_pe)], na.rm = TRUE) - 1
      } else {
        NA_real_
      },

      # Fill the NA in proj_pct_past_pe for the identified year
      proj_pct_past_pe = if_else(
        is.na(proj_pct_past_pe) & year == year_to_fill,
        last_value_past_pe,
        proj_pct_past_pe
      )
    ) |>
    select(-last_value_past_pe, -year_to_fill)

  title <- "People in Prison Past Parole Eligibility by Year"
  hc_accessibility_text <- "This chart shows the percentage of people in prison who are past their parole eligibility year, with projections highlighted in red."

  # Create the Highcharts object
  highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = title) |>
    hc_xAxis(categories = df1$year, lineWidth = 1) |>
    hc_yAxis(title = list(text = "Percent Past Parole Eligibility"),
             min = 0, max = 100, labels = list(format = "{value}%")) |>
    hc_add_series(name = "Past Parole Eligibility", data = round(df1$pct_past_pe, 1),
                  color = teal, marker = list(enabled = TRUE), connectNulls = TRUE,
                  tooltip = list(valueSuffix = "%")) |>
    hc_add_series(name = "Projected Past Parole Eligibility",
                  data = round(df1$proj_pct_past_pe, 1), color = red,
                  marker = list(enabled = TRUE), connectNulls = TRUE,
                  tooltip = list(valueSuffix = "%")) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE, filename = paste0(gsub(" ", "_", tolower(title)), "_")) |>
    hc_caption(text = ncrp_csg_source) |>
    fnc_add_hc_accessibility(hc_accessibility_text)
})
all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)
all_line_pop_pe_by_year$Georgia



# ---------------------------------------------------------------------------- #
# Prepare Column Charts Data (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Prepare data for people in prison past their parole eligibility date by race, ethnicity and sex
# Current = currently eligible for parole release (includes people past parole eligibility)
current_pe <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  fnc_filter_by_year(which_overall_year)

# Use unconsolidated file for age - ageyrend not in consolidated file
current_pe_unconsolidated <-
  fnc_filter_pe_population_criteria(ncrp_yearendpop_not_consolidated,
                                    exclude = states_to_exclude,
                                    dont_filter = states_nofilter) |>
  filter(parelig_status == "Current") |>
  fnc_filter_by_year(which_overall_year)

## Summarize number of people in prison by race, sex, ageyrend, offense, and sentence length
current_pe_race     <- fnc_summarize_data(current_pe, "race") |>
  # Exclude states with high missingness for race and ethnicity
  # Prints off which states are missing data
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race)
current_pe_sex      <- fnc_summarize_data(current_pe, "sex")
current_pe_ageyrend <- fnc_summarize_data(current_pe, "ageyrend")
current_pe_sentlgth <- fnc_summarize_data(current_pe, "sentlgth")
current_pe_fbi_index <- fnc_summarize_data(current_pe, "fbi_index") |>
  fnc_group_offense_type()


# List of parameters for each category
categories <- list(
  list(data = current_pe_race, x_var = "race", metric = "Race and Ethnicity"),
  list(data = current_pe_sex, x_var = "sex", metric = "Sex"),
  list(data = current_pe_ageyrend, x_var = "ageyrend", metric = "Age"),
  list(data = current_pe_sentlgth, x_var = "sentlgth", metric = "Sentence Length"),
  list(data = current_pe_fbi_index, x_var = "fbi_index", metric = "Offense Type")
)



# ---------------------------------------------------------------------------- #
# Generate Sentences and Column Charts (Demographics, Offense Type, Sentence Length)
# ---------------------------------------------------------------------------- #

# Initialize empty lists to store bar charts and sentences
all_bar_pe <- list()
all_sentence_pe <- list()

# Loop through each category to generate bar charts and sentences
for (category in categories) {
  all_bar_pe[[category$x_var]] <- fnc_generate_bar_charts(
    data       = category$data,
    x_var      = category$x_var,
    metric     = category$metric,
    type_desc  = "the prison population past parole eligibility",
    title_type = "People in Prison Past Parole Eligibility",
    y_var      = "prop"
  )

  all_sentence_pe[[category$x_var]] <- fnc_generate_sentences(
    data      = category$data,
    x_var     = category$x_var,
    type_desc = "in prison past parole eligibility"
  )
}

# Access specific bar charts and sentences
all_bar_pe_race <- all_bar_pe[["race"]]
all_sentence_pe_race <- all_sentence_pe[["race"]]
all_bar_pe_sex <- all_bar_pe[["sex"]]
all_sentence_pe_sex <- all_sentence_pe[["sex"]]
all_bar_pe_ageyrend <- all_bar_pe[["ageyrend"]]
all_sentence_pe_ageyrend <- all_sentence_pe[["ageyrend"]]
all_bar_pe_sentlgth <- all_bar_pe[["sentlgth"]]
all_sentence_pe_sentlgth <- all_sentence_pe[["sentlgth"]]
all_bar_pe_fbi_index <- all_bar_pe[["fbi_index"]]
all_sentence_pe_fbi_index <- all_sentence_pe[["fbi_index"]]

# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

# Define the data objects and their corresponding file names
data_files <- list(
  all_sentence_pe_type        = "all_sentence_pe_type.rds",
  all_pie_pe_type             = "all_pie_pe_type.rds",
  all_sentence_pop_pe_by_year = "all_sentence_pop_pe_by_year.rds",
  all_line_pop_pe_by_year     = "all_line_pop_pe_by_year.rds",
  all_sentence_pe_race        = "all_sentence_pe_race.rds",
  all_bar_pe_race             = "all_bar_pe_race.rds",
  all_sentence_pe_sex         = "all_sentence_pe_sex.rds",
  all_bar_pe_sex              = "all_bar_pe_sex.rds",
  all_sentence_pe_ageyrend    = "all_sentence_pe_ageyrend.rds",
  all_bar_pe_ageyrend         = "all_bar_pe_ageyrend.rds",
  all_sentence_pe_fbi_index   = "all_sentence_pe_fbi_index.rds",
  all_bar_pe_fbi_index        = "all_bar_pe_fbi_index.rds",
  all_sentence_pe_sentlgth    = "all_sentence_pe_sentlgth.rds",
  all_bar_pe_sentlgth         = "all_bar_pe_sentlgth.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))
