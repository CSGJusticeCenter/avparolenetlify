#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: September 12, 2024 (MAR)
# Description:
#    This script generates parole eligibility visualizations and related summaries
#    for the "Parole Eligibility" tab in state reports.
#
#    Key Components:
#    - **Prison Population by Parole Eligibility Status**: Filters the NCRP prison population data by specific criteria,
#      including new court commitments and sentence lengths of 1-25 years, to analyze people in prison past their parole eligibility date.
#      It then visualizes the proportion of individuals in different parole eligibility statuses.
#
#    - **Demographic Breakdown**: Analyzes and visualizes parole eligibility status by demographic factors such as race, sex, and age for
#      people in prison with new court commitments and sentence lengths between 1 and 25 years.
#
#    - **Offense Type Analysis**: Breaks down the parole eligibility population by offense types (e.g., violent, non-violent) to see what
#      percentage of people are in prison past their eligibility date based on the crimes committed.
#
#    - **Sentence Length Distribution**: Examines parole eligibility status by sentence length for individuals in prison past their parole eligibility year,
#      with a focus on people sentenced to 1-24.9 years.
#
#    For each of these components, the script generates both **visualizations** (e.g., stacked bar charts, column charts) and **descriptive sentences**
#    to summarize the findings for each state.
#
#    Finally, the output data and visualizations are saved as `.rds` files for later use in the interactive tool.
#######################################

# pes = parole eligibility status
# pop = population
# ncrp = NCRP data

# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

# Create a dataframe with our filtered criteria
# Only interested in people in prison for new court commitments and
# with sentence lengths between 1-25 years
ncrp_yearendpop_filtered <- filter_population_criteria(ncrp_yearendpop)

# Get number of people currently eligible (PCE) for parole and incarcerated
# By state and year
# Filtered to people in prison for new offenses and sentence lengths between 1-25 years
current_pe_pop <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(n = n()) |>
  mutate(type = "Current")

# Get total population by state and year
# Filtered to people in prison for new offenses and sentence lengths between 1-25 years
ncrp_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(n = n()) |>
  mutate(type = "Total Population")

ncrp_current_pe_pop <- rbind(current_pe_pop, ncrp_pop)

# VISUALIZATION: Line graph showing change in PCE population and total prison population
# Generate graph for each state
states <- unique(ncrp_current_pe_pop$state)
all_line_pop_pe_by_year <- map(.x = states, .f = function(x) {

  df_state_current <- ncrp_current_pe_pop |>
    filter(state == x, type == "Current") |>
    filter(rptyear >= 2010)
  df_state_total <- ncrp_current_pe_pop |>
    filter(state == x, type == "Total Population") |>
    filter(rptyear >= 2010)

  highcharts <- highchart() |>
    hc_title(text = "Population Trends") |>
    hc_xAxis(categories = df_state_current$rptyear) |>
    hc_yAxis(title = list(text = "")) |>

    # Add series for Current Population
    hc_add_series(name = "In Prison Past Parole Eligiblity", data = df_state_current$n, type = "line") |>

    # Add series for Total Population
    hc_add_series(name = "Total Population", data = df_state_total$n, type = "line") |>

    hc_tooltip(pointFormat = '{series.name}: <b>{point.y}</b>') |>

    hc_add_theme(hc_theme_with_line)

  return(highcharts)
})

all_line_pop_pe_by_year <- setNames(all_line_pop_pe_by_year, states)
all_line_pop_pe_by_year$Georgia


# ---------------------------------------------------------------------------- #
# Prison Population by PE Status
# ---------------------------------------------------------------------------- #

# Total prison population by state and year
# Only interested in people in prison for new court commitments and
# with sentence lengths between 1-25 years
ncrp_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(yearendpop = n())

# Prison population by parole eligibility status (missing, current, eligible in the future)
# Total prison population for new crimes/sentence lengths between 1-25 years by state and year
# In essence, who is in prison past their parole eligibility year?
ncrp_pes_subset <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  count(parelig_status) |>
  left_join(ncrp_pop, by = c("state", "rptyear")) |>
  mutate(prop = n / yearendpop,
         tooltip = paste0("<b>Parole Eligibility Status:</b> ", parelig_status, "<br>",
                          "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                          "<b>Percentage of People:</b> ", round(prop*100, 0), "%"),
         parelig_status_1 = case_when(parelig_status == "Missing or Not Parole-Eligible" ~ "Missing or Not<br>Parole-Eligible",
                                      TRUE ~ parelig_status),
         prop_label = paste0(
           "<div style='text-align: center;'><b>", parelig_status_1, "</b><br>",  # Center the label
           round(prop * 100, 0), "%</div>"  # Keep the number normal
         ))

# VISUALIZATION: Pct. of Prison Population by Parole Eligibility Status
# Horizontal stacked bar chart showing prison population by parole eligibility status
states <- unique(ncrp_pes_subset$state)
all_stackedbar_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pes_subset |>
    filter(state == x) |>
    filter(rptyear == select_year)

  hc_accessibility_text <-
    paste0("This graph shows the proportion of the prison population by parole eligibility status in ",
           select_year, " in the state of ", x, ". Parole eligibility statuses include the new court commitment population currently eligible,
      new court commitment population eligible in the future, and population with missing parole eligibility data.")

  highcharts <- highchart() |>
    hc_chart(type = "bar",
             marginTop = -20) |>
    hc_title(text = "Pct. of Prison Population by Parole Eligibility Status") |>
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
                                 borderColor = "#FFFFFF")) |>
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
                    style = list(fontSize = "12px", fontWeight = "normal", color = "#000000", textOutline = "none")
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
                    style = list(fontSize = "12px", fontWeight = "normal", color = "#000000", textOutline = "none")
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
                    style = list(fontSize = "12px", fontWeight = "regular", color = "#000000", textOutline = "none")
                  )) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Georgia

# SENTENCE: In X year, there were X people who were in prison past their parole
#           eligibility date. This group made up X% of the people in prison.
states <- unique(ncrp_pes_subset$state)
all_sentence_pe_type <- map(.x = states,  .f = function(x) {

  df1 <- ncrp_pes_subset |>
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





fnc_prepare_pe_data2 <- function(df, count_column){
  df1 <- df |>
    filter(rptyear == select_year &
             parelig_status == "Current") |>
    filter(admtype == "New court commitment") |>
    filter(sentlgth == "1-1.9 years" |
             sentlgth == "2-4.9 years" |
             sentlgth == "5-9.9 years" |
             sentlgth == "10-24.9 years") |>
    group_by(state) |>
    filter(!is.na({{ count_column }})) |>
    count({{ count_column }}) |>
    mutate(
      prop = n/sum(n),
      yearendpop_ped = sum(n),
      prop_label = paste0(round(prop*100, 0), "%"),
      n_label = formattable::comma(n, 0)
    ) |>
    ungroup()
  return(df1)
}

fnc_hc_columnchart <- function(df, x_var, y_var, accessibility_text) {

  xaxis_order <- df[[x_var]]

  highcharts <- highchart() |>
    hc_add_series(df,
                  type = "column",
                  hcaes(x = !!sym(x_var),
                        y = !!sym(y_var)),
                  dataLabels = list(enabled = TRUE,
                                    format = "{point.prop_label}",
                                    style = list(fontWeight = "regular",
                                                 fontSize = "1em",
                                                 fontFamily = "Graphik",
                                                 textOutline = 0))) |>
    hc_xAxis(categories = xaxis_order) |>
    hc_yAxis(labels = list(enabled = TRUE),
             title = list(text = "")
    ) |>
    hc_add_theme(hc_theme_with_line) |>
    hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |>
    hc_legend(enabled = FALSE) |>
    hc_exporting(enabled = TRUE) |>
    hc_plotOptions(series = list(animation = FALSE,
                                 cursor = "pointer",
                                 borderWidth = 3,
                                 minPointLength = 4),
                   accessibility = list(enabled = TRUE,
                                        keyboardNavigation = list(enabled = TRUE),
                                        linkedDescription = accessibility_text,
                                        landmarkVerbosity = "one"),
                   area = list(accessibility = list(description = accessibility_text)))

  return(highcharts)
}


# Get number and proportion of people in prison past their parole eligibility year
# by offense
current_ped_fbi_index <- fnc_prepare_pe_data2(ncrp_yearendpop, fbi_index)
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
    mutate(fbi_index = case_when(fbi_index == "Murder and Non-negligent Manslaughter" ~
                                   "Murder and Non-negligent<br>Manslaughter",
                                 fbi_index == "Aggravated or Simple Assault" ~ "Aggravated or<br>Simple Assault",
                                 TRUE ~ fbi_index)) |>
    filter(state == x) |>
    mutate(prop = prop * 100,
           tooltip = paste0("<b>Offense:</b> ", fbi_index, "<br>",
                            "<b>People:</b> ", formattable::comma(n, 0), "<br>",
                            "<b>Percentage of People:</b> ", round(prop, 0), "%"))

  hc_accessibility_text <- paste0("TBD")

  highcharts <- fnc_hc_columnchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
    hc_yAxis(max = max(df1$prop) * 1.5,
             labels = list(
               formatter = JS("function() {
                  return this.value + '%';
                }")
             )) |>
    hc_title(text = "Offense Types for People in Prison Past Their Parole Eligibility") |>
    hc_tooltip(pointFormat = "{point.tooltip}") |>
    hc_plotOptions(series = list(
      colorByPoint = TRUE
    )) |>
    hc_colors(df1$color)

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

# # Get unique states
# states <- unique(current_ped_offense_group$state)
#
# # Create Highcharts visualizations for each state
# all_bubble_ped_offense_group <- map(.x = states, .f = function(x) {
#
#   # Sample data for three circles
#   df1 <- current_ped_offense_group |>
#     filter(state == x) |>
#     rename(name = group,
#            value = prop)
#
#   highcharts <- highchart() |>
#     hc_chart(
#       type = "packedbubble",
#       height = 200, # Adjust height
#       width = 200,  # Adjust width
#       margin = c(0, 0, 0, 0),
#
#       spacingBottom = 0,
#       spacingTop = 0,
#       spacingLeft = 0,
#       spacingRight = 0
#     ) |>
#     hc_add_series(
#       data = list_parse(df1),
#       type = "packedbubble",
#       dataLabels = list(
#         enabled = TRUE,
#         useHTML = TRUE,
#         style = list(
#           color = "black",
#           textOutline = "none",
#           fontWeight = "normal", # Normal weight for proportions
#           fontSize = "14px" # Adjust the font size
#         ),
#         align = 'center', # Center text horizontally
#         verticalAlign = 'middle', # Center text vertically
#         allowOverlap = TRUE,
#         inside = TRUE,
#         formatter = JS("function() {
#           if (this.point.value < .01) {
#             return null;
#           }
#           return '<div style=\"text-align: center;\">' + this.point.name + '<br>' + this.point.prop_label + '</div>';
#         }")
#       ),
#       maxSize = "100%",
#       layoutAlgorithm = list(
#         gravitationalConstant = 0.05,
#         splitSeries = FALSE,
#         seriesInteraction = TRUE,
#         dragBetweenSeries = TRUE,
#         parentNodeLimit = TRUE
#       )
#     ) |>
#     # hc_tooltip(pointFormat = "<b>{point.name} Offenses:</b><br><br>Number of People: {point.n_label}<br>Proportion: {point.prop_label}"
#     # ) |>
#     hc_tooltip(
#       pointFormat = "<b>{point.name} Offenses:</b><br><br>Number of People: {point.n_label}<br>Proportion: {point.prop_label}",
#       borderWidth = 1,
#       borderRadius = 0,
#       backgroundColor = '#FFFFFF', # Fully opaque white background
#       outside = TRUE, # Ensure tooltip is rendered outside
#       useHTML = TRUE,
#       formatter = JS("function() {
#           return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
#           '<div style=\"text-align:left;\">' +
#           '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.tooltip + '</span>' +
#           '</div></div>';
#     }")
#     ) |>
#     hc_add_theme(base_hc_theme) |>
#     hc_legend(enabled = FALSE) |>
#     hc_colors(c(df1$color)) |>
#     hc_exporting(enabled = FALSE)
#
#   return(highcharts)
# })
#
# # Name the list of charts by state
# all_bubble_ped_offense_group <- setNames(all_bubble_ped_offense_group, states)
#
# # Display the chart for Georgia as an example
# all_bubble_ped_offense_group$Georgia

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

  # Check if the top two groups have equal proportions
  if (length(unique(df1$prop[1:2])) == 1) {
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison past their parole eligibility were in prison for ",
                             tolower(df1$group[1]), " offenses and ",
                             round(df1$prop[2] * 100, 0), "% for ",
                             tolower(df1$group[2]), " offenses.")
  } else {
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison past their parole eligibility were in prison for ",
                             tolower(df1$group[1]), " offenses.")
  }

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
  fbi_sentence <- paste0("The breakdown of criminal offenses reveals a more varied landscape, with most people incarcerated for ",
                         tolower(df2$fbi_index[1]), " (", round(df2$prop[1] * 100, 0), "%) and ",
                         tolower(df2$fbi_index[2]), " (", round(df2$prop[2] * 100, 0), "%) offenses.")

  # Combine the sentences
  sentences <- paste0("In ", select_year, ", ", group_sentence, " ", fbi_sentence)

  return(sentences)
})

all_sentence_parole_eligibility_fbi_index <- setNames(all_sentence_parole_eligibility_fbi_index, states)
all_sentence_parole_eligibility_fbi_index$Georgia








# ------------------------ Sentence Length ------------------------ #


# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data2(ncrp_yearendpop, sentlgth)

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
    hc_yAxis(max = 100,
             labels = list(
               formatter = JS("function() {
        return this.value + '%';
      }")
             )) |>
    hc_title(text = "Sentence Lengths for People in Prison Past Their Parole Eligibility") |>
    hc_exporting(enabled = TRUE)
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
  sentences <- paste0("In ", select_year, ", most people in prison past their parole eligibility had original sentence lengths between ",
                      df1$sentlgth, ", representing ", round(df1$prop*100, 0), "% of in prison past parole eligibility.")
  return(sentences)
})

all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia

















#------ Save Data ------#

save(all_sentence_pe_type,                         file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_pe_type.rds"))
save(all_stackedbar_pe_type,                       file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stackedbar_pe_type.rds"))

# SENTENCE NEEDED ###############################################################
save(all_line_pop_pe_by_year,                      file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_line_pop_pe_by_year.rds"))

save(all_sentence_parole_eligibility_fbi_index,    file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_parole_eligibility_fbi_index.rds"))
save(all_bar_ped_fbi_index,                        file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_ped_fbi_index.rds"))

save(all_sentence_parole_eligibility_sentlgth,     file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_parole_eligibility_sentlgth.rds"))
save(all_bar_parole_eligibility_sentlgth,          file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_bar_parole_eligibility_sentlgth.rds"))


