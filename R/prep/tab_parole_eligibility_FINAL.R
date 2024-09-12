#######################################
# Project: AV Parole
# File: tab_parole_eligibility.R
# Authors: Mari Roberts
# Date last updated: July 15, 2024 (MAR)
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
    hc_title(text = "Offense Types for People in Prison Past Their Parole Eligibility Year") |>
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
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison past their parole eligibility year were in prison for ",
                             tolower(df1$group[1]), " offenses and ",
                             round(df1$prop[2] * 100, 0), "% for ",
                             tolower(df1$group[2]), " offenses.")
  } else {
    group_sentence <- paste0(round(df1$prop[1] * 100, 0), "% of people in prison past their parole eligibility year were in prison for ",
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
    hc_title(text = "Sentence Lengths for People in Prison Past Their Parole Eligibility Year") |>
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
  sentences <- paste0("In ", select_year, ", most people in prison past their parole eligibility year had original sentence lengths between ",
                      df1$sentlgth, " representing ", round(df1$prop*100, 0), "% of those eligible for parole.")
  return(sentences)
})

all_sentence_parole_eligibility_sentlgth <- setNames(all_sentence_parole_eligibility_sentlgth, states)
all_sentence_parole_eligibility_sentlgth$Georgia

















#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_sentence_parole_eligibility_fbi_index,    file = file.path(folder, "all_sentence_parole_eligibility_fbi_index.rds"))
  save(all_bar_ped_fbi_index,                        file = file.path(folder, "all_bar_ped_fbi_index.rds"))
  save(all_bar_parole_eligibility_sentlgth,          file = file.path(folder, "all_bar_parole_eligibility_sentlgth.rds"))
  save(all_sentence_parole_eligibility_sentlgth,     file = file.path(folder, "all_sentence_parole_eligibility_sentlgth.rds"))
}


