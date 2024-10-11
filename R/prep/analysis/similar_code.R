generate_visualization <- function(state_var, df, x_var, y_var, metric, title_type, type, source) {
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

generate_visualizations_by_state <- function(states, df, x_var, y_var, metric, title_type, type, source) {
  visualizations <- map(.x = states,  .f = function(x) {
    generate_visualization(x, df, x_var, y_var, metric, title_type, type, source)
  })
  setNames(visualizations, states)
}

generate_sentence <- function(state_var, df, x_var, type) {
  sentences <- fnc_generate_columnchart_sentence(
    state_var  = state_var,
    df         = df,
    x_var      = x_var,
    type       = type
  )
  return(sentences)
}

generate_sentences_by_state <- function(states, df, x_var, type) {
  sentences <- map(.x = states,  .f = function(x) {
    generate_sentence(x, df, x_var, type)
  })
  setNames(sentences, states)
}

# Generalized process for handling each category
process_population_data <- function(df, x_var, metric, type, title_type, source) {
  states <- unique(df$state)

  # Generate visualizations
  all_bar_population <- generate_visualizations_by_state(
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
  all_sentence_population <- generate_sentences_by_state(
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
