







# Consolidate common functionality into a reusable function
generate_charts_and_sentences <- function(df, x_var, metric_label, chart_title_suffix) {
  states <- unique(df$state)

  # VISUALIZATION: Generate chart for each state
  all_bar_charts <- map(.x = states, .f = function(x) {
    fnc_hc_columnchart(
      state_var  = x,
      df         = df,
      x_var      = x_var,
      y_var      = "prop",
      metric     = metric_label,
      type       = "released from prison",
      title_type = chart_title_suffix
    )
  })

  # Assign state names to the list
  all_bar_charts <- setNames(all_bar_charts, states)

  # SENTENCE: Generate sentence for each state
  all_sentences <- map(.x = states, .f = function(x) {
    fnc_generate_columnchart_sentence(
      state_var  = x,
      df         = df,
      x_var      = x_var,
      type       = "released from prison"
    )
  })

  # Assign state names to the sentence list
  all_sentences <- setNames(all_sentences, states)

  # Return both visualizations and sentences as a list
  return(list(charts = all_bar_charts, sentences = all_sentences))
}

# Summarize data and generate visualizations and sentences for different categories
prison_releases_race      <- fnc_summarize_data(ncrp_releases_filtered, "race")
prison_releases_sex       <- fnc_summarize_data(ncrp_releases_filtered, "sex")
prison_releases_agerlse   <- fnc_summarize_data(ncrp_releases_filtered, "agerlse")
prison_releases_fbi_index <- fnc_summarize_data(ncrp_releases_filtered, "fbi_index")

# Generate charts and sentences for each category
results_race      <- generate_charts_and_sentences(prison_releases_race,      "race",      "Race and Ethnicity", "People Released from Prison")
results_sex       <- generate_charts_and_sentences(prison_releases_sex,       "sex",       "Sex",                "People Released from Prison")
results_age       <- generate_charts_and_sentences(prison_releases_agerlse,   "agerlse",   "Age",                "People Released from Prison")
results_fbi_index <- generate_charts_and_sentences(prison_releases_fbi_index, "fbi_index", "Offense Type",        "People Released from Prison")

# Access specific state data if needed, e.g., for Georgia
results_race$charts$Georgia
results_race$sentences$Georgia

results_sex$charts$Georgia
results_sex$sentences$Georgia

results_age$charts$Georgia
results_age$sentences$Georgia

results_fbi_index$charts$Georgia
results_fbi_index$sentences$Georgia






























fnc_generate_visualization <- function(state_var, df, x_var, y_var, metric, title_type, type, source) {
  highcharts <- fnc_hc_columnchart(
    state_var  = state_var,
    df         = df,
    x_var      = x_var,
    y_var      = y_var,
    metric     = metric,
    type       = type,
    title_type = title_type,
    source     = source
  )
  return(highcharts)
}

fnc_generate_visualizations_by_state <- function(states, df, x_var, y_var, metric, title_type, type, source) {
  visualizations <- map(.x = states,  .f = function(x) {
    fnc_generate_visualization(x, df, x_var, y_var, metric, title_type, type, source)
  })
  setNames(visualizations, states)
}

fnc_generate_sentence <- function(state_var, df, x_var, type) {
  sentences <- fnc_generate_columnchart_sentence(
    state_var  = state_var,
    df         = df,
    x_var      = x_var,
    type       = type
  )
  return(sentences)
}

fnc_generate_sentences_by_state <- function(states, df, x_var, type) {
  sentences <- map(.x = states,  .f = function(x) {
    fnc_generate_sentence(x, df, x_var, type)
  })
  setNames(sentences, states)
}

# Generalized process for handling each category
process_population_data <- function(df, x_var, metric, type, title_type, source) {
  states <- unique(df$state)

  # Generate visualizations
  all_bar_population <- fnc_generate_visualizations_by_state(
    states = states,
    df     = df,
    x_var  = x_var,
    y_var  = "prop",
    metric = metric,
    title_type = title_type,
    type   = type,
    source = source
  )

  # Generate sentences
  all_sentence_population <- fnc_generate_sentences_by_state(
    states = states,
    df     = df,
    x_var  = x_var,
    type   = type
  )

  return(list(visualizations = all_bar_population, sentences = all_sentence_population))
}

# Process Race Data
race_results <- process_population_data(
  df         = bjs_prison_pop_by_race_2020,
  x_var      = "race",
  metric     = "Race and Ethnicity",
  type       = "the prison population",
  title_type = "People in Prison",
  source     = bjs_source
)
all_bar_population_race <- race_results$visualizations
all_sentence_population_race <- race_results$sentences

# Process Sex Data
sex_results <- process_population_data(
  df         = bjs_prison_pop_by_sex_2022,
  x_var      = "sex",
  metric     = "Sex",
  type       = "the prison population",
  title_type = "People in Prison",
  source     = bjs_source
)
all_bar_population_sex <- sex_results$visualizations
all_sentence_population_sex <- sex_results$sentences

# Process Age Data
age_results <- process_population_data(
  df         = ncrp_population_ageyrend,
  x_var      = "ageyrend",
  metric     = "Age",
  type       = "the prison population",
  title_type = "People in Prison",
  source     = ncrp_source
)
all_bar_population_ageyrend <- age_results$visualizations
all_sentence_population_ageyrend <- age_results$sentences
