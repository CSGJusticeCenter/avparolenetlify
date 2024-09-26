#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    This script generates parole eligibility visualizations and related summaries
#    for the "Parole Eligibility" tab in state reports.
#
#    Key Components:
#    - **Prison Population by Parole Eligibility Status**: Filters the NCRP prison population data by specific criteria,
#      including new court commitments and sentence lengths of 1-25 years, to analyze people in prison past their parole eligibility date.
#      It then visualizes the proportion of individuals in different parole eligibility statuses (current, future, missing).
#
#    - **Demographic Breakdown**: Analyzes and visualizes demographics such as race, sex, and age for
#      people in prison past parole eligibility.
#
#    - **Offense Type Analysis**: Breaks down the parole eligibility population by offense types (e.g., violent, non-violent) to see what
#      percentage of people are in prison past their eligibility date based on the crimes committed.
#
#    - **Sentence Length Distribution**:  Breaks down the parole eligibility population by sentence length for individuals in prison past their parole eligibility year,
#      with a focus on people sentenced to 1-24.9 years.
#
#    For each of these components, the script generates both **visualizations** (e.g., stacked bar charts, column charts) and **descriptive sentences**
#    to summarize the findings for each state.
#
#    Finally, the output data and visualizations are saved as `.rds` files for later use in the interactive tool.
#######################################

# ---------------------------------------------------------------------------- #
# Prison Population by PE Status
# ---------------------------------------------------------------------------- #

# Filter the population data to include only people in prison for new court commitments
# with sentence lengths between 1-25 years, based on our criteria
# Only includes states with parole systems
ncrp_yearendpop_filtered <- fnc_filter_pe_population_criteria(ncrp_yearendpop)

# Total prison population by state and year
# Only people in prison for new court commitments
# with sentence lengths between 1-25 years, based on our criteria
# Only includes states with parole systems
total_pe_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n(), .groups = "drop")

# Prison population by parole eligibility status (missing, current, eligible in the future)
# Only people in prison for new court commitments
# with sentence lengths between 1-25 years, based on our criteria
# Only includes states with parole systems
pe_status_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(total_pe_pop, by = c("state", "rptyear")) |>
  mutate(prop = n / yearendpop,
         tooltip = paste0("<b>Parole Eligibility Status:</b> ", parelig_status, "<br>",
                          "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                          "<b>Percentage of People:</b> ", round(prop*100, 0), "%"),
         parelig_status_1 =
           case_when(parelig_status == "Missing or Not Parole-Eligible" ~ "Missing or Not<br>Parole-Eligible",
                     TRUE ~ parelig_status),
         prop_label = paste0(
           "<div style='text-align: center;'><b>", parelig_status_1, "</b><br>",  # Center the label
           round(prop * 100, 1), "%</div>"
         ))

# VISUALIZATION: Prison Population by Parole Eligibility Status
# Horizontal stacked bar chart showing prison population by parole eligibility status
states <- unique(pe_status_pop$state)
all_stackedbar_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- pe_status_pop |>
    filter(state == x) |>
    filter(rptyear == select_year)

  hc_accessibility_text <-
    paste0("This graph shows the proportion of the prison population by parole eligibility status in ",
           select_year, " in the state of ", x,
           ". Parole eligibility statuses include the new court commitment population currently eligible,
      new court commitment population eligible in the future, and population with missing parole eligibility data.")

  highcharts <- highchart() |>
    hc_chart(type = "bar",
             marginTop = -20) |>
    hc_title(text = paste0("Prison Population by Parole Eligibility Status, ", select_year)) |>
    hc_add_theme(base_hc_theme) |>
    hc_xAxis(title = list(text = NULL),
             lineWidth = 0,
             minorGridLineWidth = 0,
             tickColor = "transparent",
             lineColor = 'transparent',
             labels = list(enabled = FALSE)) |>
    hc_yAxis(title = list(text = ""),
             max = 1,
             gridLineWidth = 0,
             tickColor = "transparent",
             minorGridLineWidth = 0,
             labels = list(enabled = FALSE)) |>
    hc_plotOptions(series = list(stacking = "normal",
                                 pointWidth = 40,
                                 borderWidth = 3,  # Adjust this to increase outline size
                                 borderColor = "#FFFFFF",
                                 minPointLength = 5)) |>
    hc_tooltip(formatter = JS("function () {return this.point.tooltip;}")) |>
    hc_add_series(name = "Missing or Not Parole-Eligible",
                  data = list(list(y = df1$prop[3], tooltip = df1$tooltip[3], label = df1$prop_label[3])),
                  stack = "a",
                  color = darkgray,
                  dataLabels = list(
                    enabled = TRUE,
                    align = "center",
                    formatter = JS("function() {if (this.y > 0.00) {return this.point.label;}return null;}"),
                    useHTML = TRUE,
                    x = 0,
                    y = 50,
                    style = list(fontSize = "12px", fontWeight = "normal",
                                 color = "#000000", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future",
                  data = list(list(y = df1$prop[2], tooltip = df1$tooltip[2], label = df1$prop_label[2])),
                  stack = "a",
                  color = color2,
                  dataLabels = list(
                    enabled = TRUE,
                    align = "center",
                    formatter = JS("function() {if (this.y > 0.00) {return this.point.label;}return null;}"),
                    useHTML = TRUE,
                    x = 0,
                    y = 50,
                    style = list(fontSize = "12px", fontWeight = "normal",
                                 color = "#000000", textOutline = "none")
                  )) |>
    hc_add_series(name = "Current",
                  data = list(list(y = df1$prop[1], tooltip = df1$tooltip[1], label = df1$prop_label[1])),
                  color = color4,
                  stack = "a",
                  dataLabels = list(
                    enabled = TRUE,
                    align = "center",
                    formatter = JS("function() {if (this.y > 0.00) {return this.point.label;}return null;}"),
                    useHTML = TRUE,
                    x = 0,
                    y = 50,
                    style = list(fontSize = "12px", fontWeight = "regular",
                                 color = "#000000", textOutline = "none")
                  )) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Alabama
all_stackedbar_pe_type$Georgia



# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison.
states <- unique(pe_status_pop$state)
all_sentence_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- pe_status_pop |>
    filter(state == x &
             parelig_status == "Current"&
             rptyear == select_year)

  sentences <- paste0("In ", select_year, ", there were ", formattable::comma(df1$n, digits = 0),
                      " people* in prison past their parole eligibility. This group made up ",
                      round(df1$prop*100, 0), " percent of people* in prison.")
  return(sentences)
})

all_sentence_pe_type <- setNames(all_sentence_pe_type, states)
all_sentence_pe_type$Georgia


# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

# Get the number of people currently eligible for parole who are still incarcerated
# Group by state and report year, and create a summary count for each group
current_pe_pop <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(type = "Current")

# Get the total prison population by state and year
# Group by state and report year, and summarize the total population
total_pe_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(total_n = n(), .groups = "drop") |>
  mutate(type = "Total Population")

# Merge the PCE population with the total population data by state and year
# Calculate the proportion of people past parole eligibility out of the total prison population
pe_pop_prop <- current_pe_pop |>
  left_join(total_pe_pop, by = c("state", "rptyear")) |>
  mutate(proportion = n / total_n)

# Generate sentences summarizing the proportion of PCE population changes over time for each state
# Loop through each unique state
states <- unique(pe_pop_prop$state)
all_sentence_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter data for the current state
  df <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010)

  # Get the earliest and latest years
  earliest_year <- min(df$rptyear)
  latest_year <- max(df$rptyear)

  # Get the proportion of people past parole eligibility for the earliest and latest years
  proportion_earliest <- df |>
    filter(rptyear == earliest_year) |>
    pull(proportion) * 100

  proportion_latest <- df |>
    filter(rptyear == latest_year) |>
    pull(proportion) * 100

  # Calculate the change in proportion
  change <- proportion_latest - proportion_earliest

  # Determine if it increased, decreased, or stayed the same
  if (change > 0) {
    direction <- paste0("increased by ", abs(round(change, 0)), " percent")
  } else if (change < 0) {
    direction <- paste0("decreased by ", abs(round(change, 0)), " percent")
  } else {
    direction <- "stayed the same"
  }

  # Generate the sentence
  sentence <- paste0(
    "From ", earliest_year, " to ", latest_year,
    ", the percentage of people* in prison past parole eligibility ", direction, "."
  )

  return(sentence)
})

# Assign state names as labels to the generated sentences for each state
all_sentence_pop_pe_by_year <- setNames(all_sentence_pop_pe_by_year, states)
all_sentence_pop_pe_by_year$Georgia
all_sentence_pop_pe_by_year$Hawaii


# VISUALIZATION: Create a stacked bar chart showing the percentage of people past parole eligibility (PCE)
# and the remaining total prison population for each state over time

# Loop through each unique state to generate the visualizations
states <- unique(pe_pop_prop$state)
all_stackedbar_pop_pe_by_year <- map(.x = states, .f = function(x) {

  # Filter the data for the current state and limit the analysis to years from 2010 onward
  df_state <- pe_pop_prop |>
    filter(state == x) |>
    filter(rptyear >= 2010) |>
    mutate(rptyear_fac = factor(rptyear))  # Convert years to a factor for the x-axis

  # Create the highchart visualization
  highcharts <- highchart() |>
    hc_title(text = paste0("Pct. of Prison Population Incarcerated Past Parole Eligibility, ",
                           min(df_state$rptyear), "-", max(df_state$rptyear))) |>
    hc_xAxis(categories = df_state$rptyear_fac) |>
    hc_yAxis(
      title = list(text = ""),
      max = 100,  # Set the y-axis maximum to 100% for percentage representation
      labels = list(format = "{value}%")  # Format y-axis labels as percentages
    ) |>

    # Add the first series: Remaining population (Total Population - Incarcerated Past Parole Eligibility)
    hc_add_series(
      name = "Total Population (Remaining)",
      data = (1 - df_state$proportion) * 100,  # Remaining proportion as percentage
      type = "column",
      stacking = "percent"  # Stack as percentage
    ) |>

    # Add the second series: Proportion of people past parole eligibility
    hc_add_series(
      name = "In Prison Past Parole Eligibility",
      data = df_state$proportion * 100,  # Convert proportion to percentage
      type = "column",
      stacking = "percent"  # Stack as percentage
    ) |>

    hc_plotOptions(series = list(stacking = "normal",
                                 pointWidth = 40,
                                 borderWidth = 3,  # Adjust this to increase outline size
                                 borderColor = "#FFFFFF",
                                 minPointLength = 5)) |>

    hc_tooltip(pointFormat = '{series.name}: <b>{point.y:.0f}%</b>') |>
    hc_add_theme(hc_theme_with_line) |>
    hc_exporting(enabled = TRUE) |>
    hc_colors(c(color3, color4)) |>
    hc_legend(reversed = TRUE)  # Reverse the order of the legend for better clarity

  return(highcharts)  # Return the highchart object for the current state
})

# Assign state names to the generated visualizations for each state
all_stackedbar_pop_pe_by_year <- setNames(all_stackedbar_pop_pe_by_year, states)
all_stackedbar_pop_pe_by_year$Georgia
all_stackedbar_pop_pe_by_year$Hawaii





# ---------------------------------------------------------------------------- #
# DEMOGRAPHICS
# ---------------------------------------------------------------------------- #

# Get number and proportion of people in prison past their parole eligibility year
# by offense
current_ped_race     <- fnc_prepare_pe_data(ncrp_yearendpop, race)
current_ped_sex      <- fnc_prepare_pe_data(ncrp_yearendpop, sex)
current_ped_ageyrend <- fnc_prepare_pe_data(ncrp_yearendpop, ageyrend)

# Generate graph for each state
states <- unique(current_ped_race$state)
all_bar_parole_eligibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_race |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Race and Ethnicity:</b> ", race, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their race and ethnicity in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "race", "prop", hc_accessibility_text) |>
    hc_title(text = "Race and Ethnicity") |>
    hc_subtitle(text = paste0("People in Prison Past Their Parole Eligibility, ", select_year)) |>
    hc_colors(c(color4))
  return(highcharts)
})
all_bar_parole_eligibility_race <- setNames(all_bar_parole_eligibility_race, states)
all_bar_parole_eligibility_race$Georgia


# Generate sentence for each state
states <- unique(current_ped_race$state)
all_sentence_parole_eligibility_race <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_race |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", most people* in prison past their parole eligibility were ",
                      df1$race, " people, representing ", round(df1$prop*100, 0),
                      " percent of people* in prison past parole eligibility.")
  return(sentences)
})

all_sentence_parole_eligibility_race <- setNames(all_sentence_parole_eligibility_race, states)
all_sentence_parole_eligibility_race$Georgia

# Generate graph for each state
states <- unique(current_ped_sex$state)
all_bar_parole_eligibility_sex <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sex |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Sex:</b> ", sex, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(prop))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their sex in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sex", "prop", hc_accessibility_text) |>
    hc_title(text = "Sex") |>
    hc_subtitle(text = paste0("People in Prison Past Their Parole Eligibility, ", select_year)) |>
    hc_colors(c(color4))
  return(highcharts)
})
all_bar_parole_eligibility_sex <- setNames(all_bar_parole_eligibility_sex, states)
all_bar_parole_eligibility_sex$Georgia


# Generate sentence for each state
states <- unique(current_ped_sex$state)
all_sentence_parole_eligibility_sex <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sex |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  sentences <- paste0("In ", select_year, ", most people* in prison past their parole eligibility were ",
                      tolower(df1$sex), "s, representing ", round(df1$prop*100, 0), " percent of people* in prison past parole eligibility.")
  return(sentences)
})

all_sentence_parole_eligibility_sex <- setNames(all_sentence_parole_eligibility_sex, states)
all_sentence_parole_eligibility_sex$Georgia

# Generate graph for each state
states <- unique(current_ped_ageyrend$state)
all_bar_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_ageyrend |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Age:</b> ", ageyrend, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%")) |>
    arrange(desc(ageyrend))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their age in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "ageyrend", "prop", hc_accessibility_text) |>
    hc_title(text = "Age") |>
    hc_subtitle(text = paste0("People in Prison Past Their Parole Eligibility, ", select_year)) |>
    hc_colors(c(color4))
  return(highcharts)
})
all_bar_parole_eligibility_ageyrend <- setNames(all_bar_parole_eligibility_ageyrend, states)
all_bar_parole_eligibility_ageyrend$Georgia


# Generate sentence for each state
states <- unique(current_ped_ageyrend$state)
all_sentence_parole_eligibility_ageyrend <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_ageyrend |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$ageyrend <- gsub("-", " to ", df1$ageyrend)
  sentences <- paste0("In ", select_year, ", most people* in prison past their parole eligibility were between the ages of ",
                      df1$ageyrend, " old, representing ", round(df1$prop*100, 0), " percent of people* in prison past parole eligibility.")
  return(sentences)
})

all_sentence_parole_eligibility_ageyrend <- setNames(all_sentence_parole_eligibility_ageyrend, states)
all_sentence_parole_eligibility_ageyrend$Georgia




# ---------------------------------------------------------------------------- #
# OFFENSE TYPE
# ---------------------------------------------------------------------------- #

# Get number and proportion of people in prison past their parole eligibility year
# by offense
current_ped_fbi_index <- fnc_prepare_pe_data(ncrp_yearendpop, fbi_index)
current_ped_fbi_index <- current_ped_fbi_index |>
  mutate(group = case_when(
    fbi_index %in% c("Murder and Non-negligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drugs", "Public order", "Property") ~ "Non-Violent",
    TRUE ~ "Other or Unknown"
  ),
  color = case_when(
    group == "Violent" ~ color2, # color3
    group == "Non-Violent" ~ color2,
    group == "Other or Unknown" ~ darkgray
  ))

# Generate graph for each state
states <- unique(current_ped_fbi_index$state)
all_bar_ped_fbi_index <- map(.x = states, .f = function(x) {
  df1 <- current_ped_fbi_index |>
    filter(state == x) |>
    mutate(prop = prop * 100,
           tooltip = paste0("<b>Offense:</b> ", fbi_index, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  hc_accessibility_text <- paste0("TBD")

  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_title(text = "Offense Type") |>
    hc_subtitle(text = paste0("People in Prison Past Their Parole Eligibility, ", select_year)) |>
    hc_colors(c(color4))

  return(highcharts)
})

all_bar_ped_fbi_index <- setNames(all_bar_ped_fbi_index, states)
all_bar_ped_fbi_index$Georgia




# Get proportion of offenses that were violent and non-violent
current_ped_offense_group <- ncrp_yearendpop |>
  filter(rptyear == select_year &
           parelig_status == "Current") |>
  filter(admtype == "New court commitment") |>
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") |>
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
states <- unique(current_ped_fbi_index$state)
all_sentence_parole_eligibility_fbi_index <- map(.x = states,  .f = function(x) {

  # Get the top group
  df1 <- current_ped_offense_group  |>
    filter(state == x) |>
    arrange(-prop)

  # Check if there's missing data in df1
  if (nrow(df1) < 2 || any(is.na(df1$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing for the top offense groups."))
  }

  # Violent vs Non-Violent breakdown sentence
  violent_prop <- df1 |> filter(group == "Violent") |> pull(prop) * 100
  nonviolent_prop <- df1 |> filter(group == "Non-Violent") |> pull(prop) * 100

  group_sentence <- paste0("In ", select_year, ", ", round(violent_prop, 0),
                           " percent of people* in prison past their parole eligibility were in prison for violent offenses and ",
                           round(nonviolent_prop, 0), " percent for non-violent offenses.")

  # Get the top two FBI index categories
  df2 <- current_ped_fbi_index |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1:2)

  # Check if there's missing data in df2
  if (nrow(df2) < 2 || any(is.na(df2$prop[1:2]))) {
    return(paste0("Data for ", x, " is missing."))
  }

  # Construct the sentence for the FBI index breakdown
  fbi_sentence <- paste0("Most people were incarcerated past parole eligibility were serving time for ",
                         tolower(df2$fbi_index[1]), " (", round(df2$prop[1] * 100, 0), "%) and ",
                         tolower(df2$fbi_index[2]), " (", round(df2$prop[2] * 100, 0), "%) offenses.")

  # Combine the sentences
  sentences <- paste0(group_sentence, " ", fbi_sentence)

  return(sentences)
})

all_sentence_parole_eligibility_fbi_index <- setNames(all_sentence_parole_eligibility_fbi_index, states)
all_sentence_parole_eligibility_fbi_index$Georgia








# ---------------------------------------------------------------------------- #
# SENTENCE LENGTH
# ---------------------------------------------------------------------------- #

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data(ncrp_yearendpop, sentlgth)

# Generate graph for each state
states <- unique(current_ped_sentlgth$state)
all_bar_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    mutate(prop = prop*100,
           tooltip = paste0("<b>Sentence Length:</b> ", sentlgth, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  hc_accessibility_text <- paste0("This graph shows the proportion of the prison population
                                  who are currently eligible for parole but not yet released by
                                  their original sentence length in ",
                                  select_year, " in the state of ", x, ".")
  highcharts <- fnc_hc_columnchart(df1, "sentlgth", "prop", hc_accessibility_text) |>
    hc_title(text = "Sentence Length") |>
    hc_subtitle(text = paste0("People in Prison Past Their Parole Eligibility, ", select_year)) |>
    hc_colors(c(color4))
  return(highcharts)
})
all_bar_parole_eligibility_sentlgth <- setNames(all_bar_parole_eligibility_sentlgth, states)
all_bar_parole_eligibility_sentlgth$Georgia


# Generate sentence for each state
states <- unique(current_ped_sentlgth$state)
all_sentence_parole_eligibility_sentlgth <- map(.x = states,  .f = function(x) {
  df1 <- current_ped_sentlgth |>
    filter(state == x) |>
    arrange(-prop) |>
    slice(1)
  df1$sentlgth <- gsub("-", " to ", df1$sentlgth)
  sentences <- paste0("In ", select_year, ", most people* in prison past their parole eligibility had original sentence lengths between ",
                      df1$sentlgth, ", representing ", round(df1$prop*100, 0), " percent of in prison past parole eligibility.")
  return(sentences)
})

all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia












# ---------------------------------------------------------------------------- #
# STATE NOTES - WAITING ON SEBA'S WORK AS OF 9/24/2024
# ---------------------------------------------------------------------------- #





# ---------------------------------------------------------------------------- #
# SAVE DATA
# ---------------------------------------------------------------------------- #

save(all_sentence_pe_type,                         file = file.path(app_folder, "all_sentence_pe_type.rds"))
save(all_stackedbar_pe_type,                       file = file.path(app_folder, "all_stackedbar_pe_type.rds"))

save(all_sentence_pop_pe_by_year,                  file = file.path(app_folder, "all_sentence_pop_pe_by_year.rds"))
save(all_stackedbar_pop_pe_by_year,                file = file.path(app_folder, "all_stackedbar_pop_pe_by_year.rds"))

save(all_sentence_parole_eligibility_race,         file = file.path(app_folder, "all_sentence_parole_eligibility_race.rds"))
save(all_bar_parole_eligibility_race,              file = file.path(app_folder, "all_bar_parole_eligibility_race.rds"))

save(all_sentence_parole_eligibility_sex,          file = file.path(app_folder, "all_sentence_parole_eligibility_sex.rds"))
save(all_bar_parole_eligibility_sex,               file = file.path(app_folder, "all_bar_parole_eligibility_sex.rds"))

save(all_sentence_parole_eligibility_ageyrend,     file = file.path(app_folder, "all_sentence_parole_eligibility_ageyrend.rds"))
save(all_bar_parole_eligibility_ageyrend,          file = file.path(app_folder, "all_bar_parole_eligibility_ageyrend.rds"))

save(all_sentence_parole_eligibility_fbi_index,    file = file.path(app_folder, "all_sentence_parole_eligibility_fbi_index.rds"))
save(all_bar_ped_fbi_index,                        file = file.path(app_folder, "all_bar_ped_fbi_index.rds"))

save(all_sentence_parole_eligibility_sentlgth,     file = file.path(app_folder, "all_sentence_parole_eligibility_sentlgth.rds"))
save(all_bar_parole_eligibility_sentlgth,          file = file.path(app_folder, "all_bar_parole_eligibility_sentlgth.rds"))


