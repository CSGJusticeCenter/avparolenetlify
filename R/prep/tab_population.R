#######################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison population visualizations and findings for population tab
#    Uses BJS Prisoners Data
#######################################

#------ Prison Population by Year ------#

states <- unique(bjs_prison_pop_by_rptyear$state)

# Generate sentence for each state
all_sentence_population <- map(.x = states, .f = function(x) {
  # Filter data for the specific state
  df1 <- bjs_prison_pop_by_rptyear %>% filter(state == x)

  # Find the earliest year and handle missing data
  earliest_year <- min(df1$rptyear)
  earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]

  if(is.na(earliest_year_population) | length(earliest_year_population) == 0) {
    # If earliest year data is missing, find the next available year
    earliest_year <- min(df1$rptyear[!is.na(df1$bjs_prison_population)])
    earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]
  }

  # Find the latest year and handle missing data
  latest_year <- max(df1$rptyear)
  latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]

  if(is.na(latest_year_population) | length(latest_year_population) == 0) {
    # If latest year data is missing, find the most recent available year
    latest_year <- max(df1$rptyear[!is.na(df1$bjs_prison_population) & df1$rptyear < latest_year])
    latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]
  }

  # Calculate the percent change
  percent_change <- (latest_year_population - earliest_year_population) / earliest_year_population * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population <b>",
                      change_type, " ", percent_change_abs, "%</b>, changing from ",
                      format(earliest_year_population, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_population, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Set names for the list elements
all_sentence_population <- setNames(all_sentence_population, states)

# Check the sentence for Georgia
all_sentence_population$Georgia


# Highchart by state since 2010
states <- unique(bjs_prison_pop_by_rptyear$state)
all_line_population_by_year <- map(.x = states,  .f = function(x) {
  df1 <- bjs_prison_pop_by_rptyear |>
    ungroup() |>
    filter(state == x) |>
    distinct()

  # Determine the maximum value for the y-axis in the visualization
  # Adds a small margin space at the top
  max_value <- max(df1$bjs_prison_population)*1.1
  min_value <- min(df1$bjs_prison_population)/1.5

  hc_accessibility_text <- paste0("TBD")

  highcharts <- # Create the line chart
    hc <- highchart() |>
    hc_chart(type = "line") |>
    hc_title(text = "Prison Population by Year") |>
    hc_yAxis(title = list(text = ""),
             min = min_value,
             max = max_value) |>
    hc_xAxis(categories = df1$rptyear,
             lineWidth = 1) |>
    hc_series(
      list(
        name = "population",
        data = df1$bjs_prison_population,
        tooltip = list(
          pointFormat = "Year: {point.category}<br>Prison Population: {point.y}"
        )
      )
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color2))

  return(highcharts)
})
all_line_population_by_year <- setNames(all_line_population_by_year, states)
all_line_population_by_year$Georgia











#------ NCRP Population by Race, Ethnicity, Age, and Gender ------#

# Prepare the data for race
current_population_race <- ncrp_yearendpop |>
  group_by(state) |>
  count(race) |>
  mutate(
    prop = n/sum(n),
    yearendpop_population = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          race, "</b><br>",
                          prop_label, "<br>"))

# Colors for race
colors_race <- c(color1, color2, color3, color4, darkgray)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_population_race <- fnc_hc_waffle(current_population_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)

# Prepare the data for sex
current_population_sex <- fnc_prepare_population_data(ncrp_yearendpop, sex)

# Colors for sex
colors_sex <- c(color1, color3)

# Accessibility text for sex
accessibility_text_sex <- "TBD"

# Create the charts for sex
all_waffle_population_sex <- fnc_hc_waffle(current_population_sex, "sex", colors_sex, "Gender", accessibility_text_sex)

# Prepare the data for age
current_population_ageyrend <- fnc_prepare_population_data(ncrp_yearendpop, ageyrend) |>
  arrange(state, desc(ageyrend))
current_population_ageyrend$ageyrend <- factor(current_population_ageyrend$ageyrend,
                                        levels = c("18-24 years",
                                                   "25-34 years",
                                                   "35-44 years",
                                                   "45-54 years",
                                                   "55+ years"))

# Colors for age
colors_age <- c(color1, color2, color3, color5, color4)

# Accessibility text for age
accessibility_text_age <- "TBD"

# Create the charts for age
all_waffle_population_ageyrend <- fnc_hc_waffle(current_population_ageyrend, "ageyrend", colors_age, "Current Age", accessibility_text_age)

# Display the chart for Georgia as an example
all_waffle_population_race$Georgia
all_waffle_population_sex$Georgia
all_waffle_population_ageyrend$Georgia



# Currently parole eligible population but still in prison by fbi_index in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_population_fbi_index <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  count(fbi_index) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          fbi_index, "</b><br>",
                          prop_label, "<br>")) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder and Non-negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drugs", "Public order", "Property") ~ "Non-Violent",
    TRUE ~ "Other or Unknown"
  ),
  color = case_when(
    group == "Violent" ~ color3,
    group == "Non-Violent" ~ color2,
    group == "Other or Unknown" ~ darkgray
  ))


# Create highcharts showing breakdown of prison population by fbi_index
states <- unique(current_population_fbi_index$state)
all_bar_population_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_population_fbi_index |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Offense:</b> ", fbi_index, "<br>",
                            "<b>Count:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Proportion:</b> ", round(prop, 1), "%"))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  by their most serious sentenced offense in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_barchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_yAxis(max = max(df1$prop)*1.5,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Offense Types for People in Prison") |>
    hc_tooltip(pointFormat = "{point.tooltip}") %>%
    hc_plotOptions(series = list(
      colorByPoint = TRUE
    )) %>%
    hc_colors(df1$color)
  return(highcharts)
})
all_bar_population_fbi_index <- setNames(all_bar_population_fbi_index, states)
all_bar_population_fbi_index$Georgia



current_population_sentlgth <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  group_by(state) |>
  count(sentlgth) |>
  mutate(
    prop = n/sum(n),
    yearendpop_population = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          sentlgth, "</b><br>",
                          prop_label, "<br>"))


# Create highcharts showing breakdown of prison population by sentlgth
states <- unique(current_population_sentlgth$state)
all_bar_population_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_population_sentlgth |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Sentence Length:</b> ", sentlgth, "<br>",
                            "<b>Count:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Proportion:</b> ", round(prop, 1), "%"))
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  by their original sentence length in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sentlgth", "prop", hc_accessibility_text) |>
    hc_yAxis(max = max(df1$prop)*1.5,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Sentence Lengths for People in Prison") |>
    hc_tooltip(pointFormat = "{point.tooltip}")
  return(highcharts)
})
all_bar_population_sentlgth <- setNames(all_bar_population_sentlgth, states)
all_bar_population_sentlgth$`New Hampshire`















#------ BJS Data - Prison Population by Race, Ethnicity, Age, and Gender ------#

# Prepare the data for race
current_pop_race <- bjs_prison_pop_by_race_2022

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_population_race <- fnc_hc_waffle(current_pop_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)






#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_line_population_by_year,    file = file.path(folder, "all_line_population_by_year.rds"))
  save(all_sentence_population,        file = file.path(folder, "all_sentence_population.rds"))
  save(all_waffle_population_race,     file = file.path(folder, "all_waffle_population_race.rds"))
  save(all_waffle_population_sex,      file = file.path(folder, "all_waffle_population_sex.rds"))
  save(all_waffle_population_ageyrend, file = file.path(folder, "all_waffle_population_ageyrend.rds"))
  save(all_bar_population_sentlgth,    file = file.path(folder, "all_bar_population_sentlgth.rds"))
  save(all_bar_population_fbi_index,   file = file.path(folder, "all_bar_population_fbi_index.rds"))


}













