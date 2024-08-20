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

# Get states with data
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

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population ",
                      change_type, " ", percent_change_abs, "%, changing from ",
                      format(earliest_year_population, big.mark = ","), " in ",
                      earliest_year, " to ", format(latest_year_population, big.mark = ","), " in ", latest_year, ".")
  return(sentences)
})

# Set names for the list elements
all_sentence_population <- setNames(all_sentence_population, states)

# Check the sentence for Georgia
all_sentence_population$Georgia


# Generate graph for each state
states <- unique(bjs_prison_pop_by_rptyear$state)
all_line_population_by_year <- map(.x = states,  .f = function(x) {
   df1 <- bjs_prison_pop_by_rptyear |>
    ungroup() |>
    filter(state == x) |>
    distinct() |>
    mutate(tooltip =
             paste0(
               "Year: ", rptyear, "<br>",
               "Year-End Population: ", bjs_prison_population))

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
          # pointFormat = "Year: {point.category}<br>Prison Population: {point.y}"
          pointFormat = "<b>Prison Population:</b> {point.y}"
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
  filter(race != "Unknown") |>
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

current_population_race <- current_population_race |> arrange(desc(n))

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_population_race <- fnc_hc_waffle(current_population_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)
all_waffle_population_race$Georgia


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
    group == "Violent" ~ color2,
    group == "Non-Violent" ~ color2,
    group == "Other or Unknown" ~ darkgray
  ))


# Create highcharts showing breakdown of prison population by fbi_index
states <- unique(current_population_fbi_index$state)
all_bar_population_fbi_index <- map(.x = states,  .f = function(x) {
  df1 <- current_population_fbi_index |>
    mutate(fbi_index = case_when(fbi_index == "Murder and Non-negligent Manslaughter" ~
                                   "Murder and Non-negligent<br>Manslaughter",
                                 TRUE ~ fbi_index)) |>
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
    hc_tooltip(pointFormat = "{point.tooltip}") |>
    hc_colors(c(color5))
  return(highcharts)
})
all_bar_population_sentlgth <- setNames(all_bar_population_sentlgth, states)
all_bar_population_sentlgth$`New Hampshire`















#------ BJS Data - Prison Population by Race, Ethnicity, Age, and Gender ------#

# Prepare the data for race
current_population_race <- bjs_prison_pop_by_race_2022
current_population_race <- current_population_race |> arrange(desc(n))

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "TBD"

# Create the charts for race
all_waffle_population_race <- fnc_hc_waffle(current_population_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)
all_waffle_population_race$Georgia






# Generate graph for each state
states <- unique(current_population_race$state)
all_sentence_population_demographics <- map(.x = states,  .f = function(x) {

  # Race demographics
  df_race <- current_population_race  |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check for missing race data
  if (nrow(df_race) < 2 || any(is.na(df_race$prop[1:2]))) {
    race_sentence <- "Data on race and ethnicity is incomplete or missing."
  } else {
    # race_sentence <- paste0("notable proportions among ",
    #                         df_race$race[1], " (", round(df_race$prop[1] * 100, 0), "%) and ",
    #                         tolower(df_race$race[2]), " (", round(df_race$prop[2] * 100, 0), "%) people.")
    race_sentence <- paste0("notable proportions among ",
                            df_race$race[1], " and ",
                            df_race$race[2], " people.")
  }

  # Gender distribution
  df_sex <- current_population_sex  |>
    filter(state == x)

  # Check for missing sex data
  if (nrow(df_sex) < 2 || any(is.na(df_sex$prop))) {
    sex_sentence <- "Gender distribution data is incomplete or missing."
  } else {
    if (df_sex$prop[df_sex$sex == "Male"] > df_sex$prop[df_sex$sex == "Female"]) {
      sex_sentence <- "By gender, there were more males than females."
    } else if (df_sex$prop[df_sex$sex == "Female"] > df_sex$prop[df_sex$sex == "Male"]) {
      sex_sentence <- "By gender, there were more females than males."
    } else {
      sex_sentence <- "By gender, there were equal proportions of males and females."
    }
  }

  # Age distribution
  df_ageyrend <- current_population_ageyrend  |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check for missing ageyrend data
  if (nrow(df_ageyrend) < 2 || any(is.na(df_ageyrend$prop[1:2]))) {
    age_sentence <- "Age distribution data is incomplete or missing."
  } else {
    # age_sentence <- paste0("Age-wise, most people were ",
    #                        df_ageyrend$ageyrend[1], " (", round(df_ageyrend$prop[1] * 100, 0), "%) and ",
    #                        df_ageyrend$ageyrend[2], " (", round(df_ageyrend$prop[2] * 100, 0), "%) old.")
    age_sentence <- paste0("Age-wise, most people were ",
                           df_ageyrend$ageyrend[1], " and ",
                           df_ageyrend$ageyrend[2], " old.")
  }

  # Combine the sentences
  sentences <- paste0("The demographics of people in prison reveal ",
                      race_sentence, " ", sex_sentence, " ", age_sentence)

  return(sentences)
})

all_sentence_population_demographics <- setNames(all_sentence_population_demographics, states)
all_sentence_population_demographics$Georgia



# Get proportion of offenses that were violent and non-violent
current_population_offense_group <- ncrp_yearendpop |>
  filter(rptyear == select_year) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder and Non-negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drugs", "Public order", "Property") ~ "Non-Violent",
    TRUE ~ "Other or Unknown"
  )) |>
  group_by(state) |>
  count(group) |>
  mutate(
    prop = n/sum(n),
    yearendpop_ped = sum(n),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(n, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          group, "</b><br>",
                          "Number of People: ", n_label, "<br>",
                          "Percentage of People: ", prop_label, "<br>"),
         color = case_when(
           group == "Violent" ~ color3,
           group == "Non-Violent" ~ color2,
           group == "Other or Unknown" ~ darkgray
         )) |>
  mutate(group = ifelse(group == "Other or Unknown", "Other<br>or Unknown", group))


# Generate sentence for each state
states <- unique(current_population_fbi_index$state)
all_sentence_population_fbi_index <- map(.x = states,  .f = function(x) {

  # Get the top group
  df1 <- current_population_offense_group  |>
    filter(state == x) |>
    arrange(-prop)

  # Check if there's missing data in df1
  if (nrow(df1) < 2 || any(is.na(df1$prop[1:2]))) {
    return(paste0("Data for ", x, " is incomplete or missing for the top offense groups."))
  }

  # Check if the top two groups have equal proportions
  if (length(unique(df1$prop[1:2])) == 1) {
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison were incarcerated for ",
                             tolower(df1$group[1]), " offenses and ",
                             round(df1$prop[2] * 100, 0), "% for ",
                             tolower(df1$group[2]), " offenses.")
  } else {
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison were incarcerated for ",
                             tolower(df1$group[1]), " offenses.")
  }

  # Get the top two FBI index categories
  df2 <- current_population_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check if there's missing data in df2
  if (nrow(df2) < 2 || any(is.na(df2$prop[1:2]))) {
    return(paste0("Data for ", x, " is incomplete or missing."))
  }

  # Construct the sentence for the FBI index breakdown
  fbi_sentence <- paste0("The breakdown of criminal offenses reveals a more varied landscape, with most people incarcerated for ",
                         tolower(df2$fbi_index[1]), " (", round(df2$prop[1] * 100, 0), "%) and ",
                         tolower(df2$fbi_index[2]), " (", round(df2$prop[2] * 100, 0), "%) offenses.")

  # Combine the sentences
  sentences <- paste0("In ", select_year, ", ", group_sentence, " ", fbi_sentence)

  return(sentences)
})

all_sentence_population_fbi_index <- setNames(all_sentence_population_fbi_index, states)
all_sentence_population_fbi_index$Georgia



# Generate sentence for each state
states <- unique(current_population_sentlgth$state)
all_sentence_population_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_population_sentlgth |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$sentlgth <- gsub("-", " to ", df1$sentlgth)
  sentences <- paste0("In ", select_year, ", most people in prison had original sentence lengths between ",
                      df1$sentlgth, " representing ", round(df1$prop*100, 0), "%.")
  return(sentences)
})

all_sentence_population_sentlgth <- setNames(all_sentence_population_sentlgth, states)
all_sentence_population_sentlgth$Georgia




#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_line_population_by_year,          file = file.path(folder, "all_line_population_by_year.rds"))
  save(all_sentence_population,              file = file.path(folder, "all_sentence_population.rds"))

  save(all_sentence_population_demographics, file = file.path(folder, "all_sentence_population_demographics.rds"))
  save(all_waffle_population_race,           file = file.path(folder, "all_waffle_population_race.rds"))
  save(all_waffle_population_sex,            file = file.path(folder, "all_waffle_population_sex.rds"))
  save(all_waffle_population_ageyrend,       file = file.path(folder, "all_waffle_population_ageyrend.rds"))

  save(all_sentence_population_sentlgth,     file = file.path(folder, "all_sentence_population_sentlgth.rds"))
  save(all_bar_population_sentlgth,          file = file.path(folder, "all_bar_population_sentlgth.rds"))

  save(all_sentence_population_fbi_index,    file = file.path(folder, "all_sentence_population_fbi_index.rds"))
  save(all_bar_population_fbi_index,         file = file.path(folder, "all_bar_population_fbi_index.rds"))
}









