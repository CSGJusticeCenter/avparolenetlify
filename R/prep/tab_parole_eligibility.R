#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
# Description:
#    Parole eligibility visualizations for tab on state reports
#######################################

# pes = parole eligibility status
# pop = population
# ncrp = NCRP data


#------ Prison Population by PE Status ------#

# Total prison population by state and year
ncrp_pop <- ncrp_yearendpop |>
  filter(admtype == "New court commitment") |>
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n())

# Total prison population for new crimes/sentence lengths between 1-25 years by state and year
ncrp_pes_subset <- ncrp_yearendpop|>
  filter(admtype == "New court commitment") |>
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(ncrp_pop,
            by = c("state", "rptyear")) |>
  mutate(prop = n / yearendpop,
         tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "<b>", parelig_status, "</b><br><br>",
                  "Percentage of the Prison Population: <br><b>",
                  paste0(round(prop*100, 1), "%</b></b>", sep = ""), "<br>"),
         prop_label = paste0(round(prop*100, 0), "%"))

# horizontal stacked bar chart showing prison population by parole eligibility status
states <- unique(ncrp_pes_subset$state)
all_stackedbar_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pes_subset |>
    filter(state == x) |>
    filter(rptyear == select_year)

  hc_accessibility_text <-
    paste0("This graph shows the proportion of the prison population by parole eligibility status in ",
           select_year, " in the state of ", x, ". Parole eligibility statuses include the new court commitment popultion currently eligible,
      new court commitment population eligible in 1 to 5 years, new court commitment population eligible in 6 or more years, other population currently or
      eligible in the future, and population with missing parole eligibility data.")

  highcharts <- highchart() |>
    hc_chart(type = "bar", marginLeft = 10) |>
    hc_title(text = "Pct. of Prison Population by Parole Eligibility Status",
             align = "left") |>
    hc_xAxis(title = list(text = NULL),
             lineWidth = 0,
             minorGridLineWidth = 0,
             lineColor = 'transparent',
             labels = list(enabled = FALSE)) |>
    hc_yAxis(title = list(text = ""),
             gridLineWidth = 0,
             minorGridLineWidth = 0,
             labels = list(enabled = FALSE)) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 pointWidth = 60)) |>
    hc_tooltip(formatter = JS("function () {
    return this.point.tooltip;
  }")) |>
    hc_add_series(name = "Missing or Not Parole-Eligible",
                  data = list(list(y = df1$prop[4], tooltip = df1$tooltip[4], label = df1$prop_label[4])),
                  stack = "a",
                  color = darkgray,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  //
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 6+ Years",
                  data = list(list(y = df1$prop[3], tooltip = df1$tooltip[3], label = df1$prop_label[3])),
                  stack = "a",
                  color = color3,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  //
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 1-5 Years",
                  data = list(list(y = df1$prop[2], tooltip = df1$tooltip[2], label = df1$prop_label[2])),
                  stack = "a",
                  color = color2,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  //
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Current",
                  data = list(list(y = df1$prop[1], tooltip = df1$tooltip[1], label = df1$prop_label[1])),
                  stack = "a",
                  color = color1,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  //
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_legend(align = "left",
              verticalAlign = "top",
              layout = "horizontal",
              reversed = TRUE,
              x = -10,
              title = list(style = list(fontWeight = "regular", fontSize = "12px"))) |>
    hc_add_theme(base_hc_theme) |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Georgia


# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison for
#           new crimes and sentence lengths between 1-25 years.

# get list of states
states <- unique(ncrp_pes_subset$state)

all_sentence_parole_eligibility_population <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pes_subset |>
    filter(state == x &
             parelig_status == "Current"&
             rptyear == select_year)

  sentences <- paste0("In ", select_year, ", there were <b>", formattable::comma(df1$n, digits = 0),
                      "</b> people in prison past their parole eligibility date. This group made up <b>",
                      df1$prop_label, "</b> of people in prison for new crimes and with sentence lengths between 1 to 25 years.")
  return(sentences)
})

all_sentence_parole_eligibility_population <- setNames(all_sentence_parole_eligibility_population, states)
all_sentence_parole_eligibility_population$Georgia





#------ PE Prison Population by Demographics ------#


# Prepare the data for race
current_ped_race <- fnc_prepare_pe_data(ncrp_yearendpop, race)

# Colors for race
colors_race <- c(color1, color2, color3, color4)

# Accessibility text for race
accessibility_text_race <- "This graph shows the proportion of the prison population who are currently eligible for parole but not yet released by %s in %s in the state of %s."

# Create the charts for race
all_waffle_parole_eligibility_race <- fnc_hc_waffle(current_ped_race, "race", colors_race, "Race and Ethnicity", accessibility_text_race)

# Prepare the data for sex
current_ped_sex <- fnc_prepare_pe_data(ncrp_yearendpop, sex)

# Colors for sex
colors_sex <- c(color1, color3)

# Accessibility text for sex
accessibility_text_sex <- "This graph shows the proportion of the prison population who are currently eligible for parole but not yet released by %s in %s in the state of %s."

# Create the charts for sex
all_waffle_parole_eligibility_sex <- fnc_hc_waffle(current_ped_sex, "sex", colors_sex, "Gender", accessibility_text_sex)

# Prepare the data for age
current_ped_ageyrend <- fnc_prepare_pe_data(ncrp_yearendpop, ageyrend) |>
  arrange(state, desc(ageyrend))
current_ped_ageyrend$ageyrend <- factor(current_ped_ageyrend$ageyrend,
                                        levels = c("18-24 years",
                                                   "25-34 years",
                                                   "35-44 years",
                                                   "45-54 years",
                                                   "55+ years"))

# Colors for age
colors_age <- c(color1, color2, color3, color5, color4)

# Accessibility text for age
accessibility_text_age <- "This graph shows the proportion of the prison population who are currently eligible for parole but not yet released by %s in %s in the state of %s."

# Create the charts for age
all_waffle_parole_eligibility_ageyrend <- fnc_hc_waffle(current_ped_ageyrend, "ageyrend", colors_age, "Current Age", accessibility_text_age)

# Display the chart for Georgia as an example
all_waffle_parole_eligibility_race$Georgia
all_waffle_parole_eligibility_sex$Georgia
all_waffle_parole_eligibility_ageyrend$Georgia







#------ PE Prison Population by Offense Type ------#

current_ped_fbi_index <- fnc_prepare_pe_data(ncrp_yearendpop, fbi_index)
current_ped_fbi_index <- current_ped_fbi_index |>
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

# Generate the highcharts for each state
states <- unique(current_ped_fbi_index$state)
all_bar_ped_fbi_index <- map(.x = states, .f = function(x) {
  df1 <- current_ped_fbi_index %>%
    filter(state == x) %>%
    mutate(prop = prop * 100,
           tooltip = paste0("<b>Offense:</b> ", fbi_index, "<br>",
                            "<b>Count:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Proportion:</b> ", round(prop, 1), "%"))

  hc_accessibility_text <- paste0("TBD")

  highcharts <- fnc_hc_barchart(df1, "fbi_index", "prop", hc_accessibility_text) %>%
    hc_yAxis(max = max(df1$prop) * 1.5,
             labels = list(
               formatter = JS("function() {
                  return this.value + '%';
                }")
             )) %>%
    hc_title(text = "Offense Types for People in Prison Past Their Parole Eligibility Year") %>%
    hc_tooltip(pointFormat = "{point.tooltip}") %>%
    hc_plotOptions(series = list(
      colorByPoint = TRUE
    )) %>%
    hc_colors(df1$color)

  return(highcharts)
})

all_bar_ped_fbi_index <- setNames(all_bar_ped_fbi_index, states)
all_bar_ped_fbi_index$Georgia









####################
#
# TITLE: Sentence Lengths
#
####################

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data(ncrp_yearendpop, sentlgth) |>
  mutate(prop_label = paste0(
    "<b>", prop_label, "</b> (", n_label, ")")
  )

# Create highcharts showing breakdown of parole-eligible prison population by sentlgth
states <- unique(current_ped_sentlgth$state)
all_bar_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    mutate(prop = prop*100)
  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their original sentence length in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sentlgth", "prop", hc_accessibility_text) |>
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Sentence Lengths for People in Prison Past Their Parole Eligibility Date")
  return(highcharts)
})
all_bar_parole_eligibility_sentlgth <- setNames(all_bar_parole_eligibility_sentlgth, states)
all_bar_parole_eligibility_sentlgth$Georgia



# Create sentences describing breakdown of parole-eligible prison population by sentlgth
states <- unique(current_ped_sentlgth$state)
all_sentence_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$sentlgth <- gsub("-", " to ", df1$sentlgth)
  sentences <- paste0("In ", select_year, ", among the prison population eligible for parole but not yet released, people with sentences between <b>",
                      df1$sentlgth, "</b> constituted the majority, representing <b>", round(df1$prop*100, 0), "%</b>.")
  return(sentences)
})

all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia

















#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_stackedbar_pe_type,                     file = file.path(folder, "all_stackedbar_pe_type.rds"))
  save(all_sentence_parole_eligibility_population, file = file.path(folder, "all_sentence_parole_eligibility_population.rds"))
  save(all_waffle_parole_eligibility_race,         file = file.path(folder, "all_waffle_parole_eligibility_race.rds"))
  save(all_waffle_parole_eligibility_sex,          file = file.path(folder, "all_waffle_parole_eligibility_sex.rds"))
  save(all_waffle_parole_eligibility_ageyrend,     file = file.path(folder, "all_waffle_parole_eligibility_ageyrend.rds"))
  save(all_bubble_ped_fbi_index,                   file = file.path(folder, "all_bubble_ped_fbi_index.rds"))
  save(all_bar_ped_fbi_index,                      file = file.path(folder, "all_bar_ped_fbi_index.rds"))
  save(all_bar_parole_eligibility_sentlgth,        file = file.path(folder, "all_bar_parole_eligibility_sentlgth.rds"))
  save(all_sentence_parole_eligibility_sentlgth,   file = file.path(folder, "all_sentence_parole_eligibility_sentlgth.rds"))
}

