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
    hc_chart(type = "bar"
             # marginLeft = 10,
             # marginBottom = -30,
             # marginTop = 10
             ) |>
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
                                 pointWidth = 40)) |>
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
                  if (this.y > 0.00) {
                    return this.point.label;
                  }
                  return null;
                }"),
                    x = 0,
                    y = 60,
                    style = list(fontSize = "12px", fontWeight = "normal", color = "#000000", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 6+ Years",
                  data = list(list(y = df1$prop[3], tooltip = df1$tooltip[3], label = df1$prop_label[3])),
                  stack = "a",
                  color = color3,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                  if (this.y > 0.00) {
                    return this.point.label;
                  }
                  return null;
                }"),
                    x = 0,
                    y = 60,
                    style = list(fontSize = "12px", fontWeight = "normal", color = "#000000", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 1-5 Years",
                  data = list(list(y = df1$prop[2], tooltip = df1$tooltip[2], label = df1$prop_label[2])),
                  stack = "a",
                  color = color2,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                  if (this.y > 0.00) {
                    return this.point.label;
                  }
                  return null;
                }"),
                    x = 0,
                    y = 60,
                    style = list(fontSize = "12px", fontWeight = "normal", color = "#000000", textOutline = "none")
                  )) |>
    hc_add_series(name = "Current",
                  data = list(list(y = df1$prop[1], tooltip = df1$tooltip[1], label = df1$prop_label[1])),
                  color = color1,
                  stack = "a",
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                  if (this.y > 0.00) {
                    return this.point.label;
                  }
                  return null;
                }"),
                    reversed = TRUE,
                    x = 0,
                    y = 60,
                    style = list(fontSize = "12px", fontWeight = "regular", color = "#000000", textOutline = "none")
                  )) |>
    hc_legend(align = "left",
              verticalAlign = "top",
              layout = "horizontal",
              reversed = TRUE,
              title = list(style = list(fontWeight = "regular", fontSize = "12px"))) |>
    hc_exporting(enabled = TRUE)

  return(highcharts)
})

all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Georgia |>
  hc_size(height = 150)|>
  hc_chart( backgroundColor = "darkgray")
save(all_stackedbar_pe_type, file = file.path(folder, "all_stackedbar_pe_type.rds"))



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

  sentences <- paste0("In ", select_year, ", there were ", formattable::comma(df1$n, digits = 0),
                      " people in prison past their parole eligibility date. This group made up ",
                      df1$prop_label, " of people in prison for new crimes and with sentence lengths between 1 to 25 years.")
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
                          "Proportion: ", prop_label, "<br>"),
         color = case_when(
           group == "Violent" ~ color3,
           group == "Non-Violent" ~ color2,
           group == "Other or Unknown" ~ darkgray
         )) |>
  mutate(group = ifelse(group == "Other or Unknown", "Other<br>or Unknown", group))

# Get unique states
states <- unique(current_ped_offense_group$state)

# Create Highcharts visualizations for each state
all_bubble_ped_offense_group <- map(.x = states, .f = function(x) {

  # Sample data for three circles
  df1 <- current_ped_offense_group |>
    filter(state == x) |>
    rename(name = group,
           value = prop)

  highcharts <- highchart() %>%
    hc_chart(
      type = "packedbubble",
      height = 200, # Adjust height
      width = 200,  # Adjust width
      margin = c(0, 0, 0, 0),

      spacingBottom = 0,
      spacingTop = 0,
      spacingLeft = 0,
      spacingRight = 0
    ) %>%
    hc_add_series(
      data = list_parse(df1),
      type = "packedbubble",
      dataLabels = list(
        enabled = TRUE,
        useHTML = TRUE,
        style = list(
          color = "black",
          textOutline = "none",
          fontWeight = "normal", # Normal weight for proportions
          fontSize = "14px" # Adjust the font size
        ),
        align = 'center', # Center text horizontally
        verticalAlign = 'middle', # Center text vertically
        allowOverlap = TRUE,
        inside = TRUE,
        formatter = JS("function() {
          if (this.point.value < .01) {
            return null;
          }
          return '<div style=\"text-align: center;\">' + this.point.name + '<br>' + this.point.prop_label + '</div>';
        }")
      ),
      maxSize = "100%",
      layoutAlgorithm = list(
        gravitationalConstant = 0.05,
        splitSeries = FALSE,
        seriesInteraction = TRUE,
        dragBetweenSeries = TRUE,
        parentNodeLimit = TRUE
      )
    ) %>%
    # hc_tooltip(pointFormat = "<b>{point.name} Offenses:</b><br><br>Number of People: {point.n_label}<br>Proportion: {point.prop_label}"
    # ) |>
    hc_tooltip(
      pointFormat = "<b>{point.name} Offenses:</b><br><br>Number of People: {point.n_label}<br>Proportion: {point.prop_label}",
      borderWidth = 1,
      borderRadius = 0,
      backgroundColor = '#FFFFFF', # Fully opaque white background
      outside = TRUE, # Ensure tooltip is rendered outside
      useHTML = TRUE,
      formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 15px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 14px;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
    ) |>
    hc_add_theme(base_hc_theme) |>
    hc_legend(enabled = FALSE) |>
    hc_colors(c(df1$color)) |>
    hc_exporting(enabled = FALSE)

  return(highcharts)
})

# Name the list of charts by state
all_bubble_ped_offense_group <- setNames(all_bubble_ped_offense_group, states)

# Display the chart for Georgia as an example
all_bubble_ped_offense_group$Georgia









####################
#
# TITLE: Sentence Lengths
#
####################

# Currently parole eligible population but still in prison by sentlgth in select year
# Only for people in prison most recently for a new court commitment, sentence lengths (1 to 24.99 years)
current_ped_sentlgth <- fnc_prepare_pe_data(ncrp_yearendpop, sentlgth)
  # mutate(prop_label = paste0(
  #   "<b>", prop_label, "</b> (", n_label, ")")
  # )

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
    hc_title(text = "Sentence Lengths for People in Prison Past Their Parole Eligibility Date") |>
    hc_exporting(enabled = TRUE)
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
  sentences <- paste0("In ", select_year, ", among the prison population eligible for parole but not yet released, people with sentences between ",
                      df1$sentlgth, " constituted the majority, representing ", round(df1$prop*100, 0), "%.")
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
  # save(all_bubble_ped_fbi_index,                   file = file.path(folder, "all_bubble_ped_fbi_index.rds"))
  save(all_bubble_ped_offense_group,               file = file.path(folder, "all_bubble_ped_offense_group.rds"))
  save(all_bar_ped_fbi_index,                      file = file.path(folder, "all_bar_ped_fbi_index.rds"))
  save(all_bar_parole_eligibility_sentlgth,        file = file.path(folder, "all_bar_parole_eligibility_sentlgth.rds"))
  save(all_sentence_parole_eligibility_sentlgth,   file = file.path(folder, "all_sentence_parole_eligibility_sentlgth.rds"))
}

# current_ped_fbi_index <- fnc_prepare_pe_data(ncrp_yearendpop, fbi_index) |>
#   mutate(group = case_when(
#     fbi_index %in% c("Murder and Non-negligent Manslaughter",
#                      "Rape or Sexual Assault",
#                      "Robbery",
#                      "Aggravated or Simple Assault",
#                      "Other Violent Offenses") ~ "Violent",
#     fbi_index %in% c("Drugs", "Public order", "Property") ~ "Non-Violent",
#     TRUE ~ "Other or Unknown"
#   ),
#   color = case_when(
#     group == "Violent" ~ color3,
#     group == "Non-Violent" ~ color2,
#     group == "Other or Unknown" ~ darkgray
#   ))
#
# # Get unique states
# states <- unique(current_ped_fbi_index$state)
#
# # Create Highcharts visualizations for each state
# all_bubble_ped_fbi_index <- map(.x = states, .f = function(x) {
#
#   df1 <- current_ped_fbi_index |>
#     filter(state == x) |>
#     select(fbi_index, n, group)
#
#   # Manual adjustments to group names
#   df1$group <- as.character(df1$group)
#   df1$group[df1$group == "Murder and Non-negligent Manslaughter"] <- "Murder and Non-negligent<br>Manslaughter"
#   df1$group[df1$group == "Rape or Sexual Assault"] <- "Rape or<br>Sexual Assault"
#   df1$group[df1$group == "Aggravated or Simple Assault"] <- "Aggravated or<br>Simple Assault"
#   df1$group[df1$group == "Other Violent Offenses"] <- "Other Violent"
#   df1$group[df1$group == "Other or Unknown"] <- "Other or<br>Unknown"
#
#   # Unique groups
#   groups <- unique(df1$group)
#
#   # Create the nested list structure
#   data <- lapply(groups, function(g) {
#     items <- df1[df1$group == g, ]
#     items_list <- lapply(1:nrow(items), function(i) {
#       list(name = as.character(items$fbi_index[i]),
#            value = items$n[i],
#            color = case_when(g == "Violent" ~ color4,
#                              g == "Non-Violent" ~ color1,
#                              TRUE ~ darkgray)) # Assuming color assignment based on group
#     })
#     list(name = g, data = items_list)
#   })
#
#   # Create the plot
#   highcharts <- highchart() |>
#     hc_chart(type = "packedbubble"
#              # marginTop = 50, marginBottom = 50,
#              # marginLeft = 50, marginRight = 50
#     ) |>
#     hc_add_series_list(data) |>
#     hc_plotOptions(
#       packedbubble = list(
#         minSize = "20%",
#         maxSize = "80%",
#         layoutAlgorithm = list(
#           splitSeries = TRUE,
#           gravitationalConstant = 0.02,
#           seriesInteraction = FALSE,
#           parentNodeLimit = TRUE
#         ),
#         dataLabels = list(
#           enabled = TRUE,
#           useHTML = TRUE, # Use HTML to support line breaks
#           format = '{point.name}',
#           style = list(
#             color = "black",
#             textOutline = "none",
#             fontWeight = "normal",
#             fontSize = "10px", # Adjust the font size
#             textAlign = "center" # Center text horizontally
#           ),
#           allowOverlap = TRUE
#         )
#       )
#     ) |>
#     hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.value}") |>
#     hc_colors(c(color4, color1, darkgray)) |>
#     hc_title(text = "Offense Breakdown for People in Prison Past Their Parole Eligibility Date") |>
#     hc_exporting(enabled = TRUE) |>
#     hc_add_theme(base_hc_theme)
#
#   return(highcharts)
# })
#
# # Name the list of charts by state
# all_bubble_ped_fbi_index <- setNames(all_bubble_ped_fbi_index, states)
#
# # Display the chart for Georgia as an example
# all_bubble_ped_fbi_index$Georgia
#
#
#
# # Get unique states
# states <- unique(current_ped_fbi_index$state)
#
# # Create Highcharts visualizations for each state
# all_bubble_ped_fbi_index <- map(.x = states, .f = function(x) {
#
#   # # Manual adjustments to group names
#   # df1$group <- as.character(df1$group)
#   # df1$group[df1$group == "Murder and Non-negligent Manslaughter"] <- "Murder and Non-negligent<br>Manslaughter"
#   # df1$group[df1$group == "Rape or Sexual Assault"] <- "Rape or<br>Sexual Assault"
#   # df1$group[df1$group == "Aggravated or Simple Assault"] <- "Aggravated or<br>Simple Assault"
#   # df1$group[df1$group == "Other Violent Offenses"] <- "Other Violent"
#   # df1$group[df1$group == "Other or Unknown"] <- "Other or<br>Unknown"
#
#   df1 <- current_ped_fbi_index |>
#     filter(state == x) |>
#     select(fbi_index, n, prop, group) |>
#     mutate(y = 1,
#            prop = round(prop*100,0),
#            prop_label = paste0(prop, "%"),
#            color = case_when(
#              group == "Violent" ~ color3,
#              group == "Non-Violent" ~ color2,
#              group == "Other or Unknown" ~ darkgray
#            ))
#
#   # Create bubble chart
#   highcharts <- highchart() |>
#     hc_chart(type = "bubble", marginTop = 0) |>
#     hc_add_series(
#       data = df1,
#       type = "bubble",
#       hcaes(x = fbi_index, y = y, size = n, color = color),
#       name = "Proportion"
#     ) |>
#     hc_add_series(
#       data = df1,
#       type = "scatter",
#       hcaes(x = fbi_index, y = y),
#       name = "Labels",
#       marker = list(enabled = FALSE),
#       dataLabels = list(
#         enabled = TRUE,
#         useHTML = TRUE,
#         format = '{point.prop_label}',
#         style = list(
#           color = "black",
#           textOutline = "none",
#           fontWeight = "bold",
#           fontSize = "1em",
#           textAlign = "center"
#         ),
#         align = "center",
#         verticalAlign = "bottom",
#         allowOverlap = TRUE,
#         y = 50 # Adjust y position to place label above the x-axis
#       )
#     ) |>
#     hc_title(text = "Bubble Chart Example") |>
#     hc_add_theme(base_hc_theme) |>
#     hc_yAxis(title = list(text = ""), visible = FALSE) |>
#     hc_xAxis(
#       # categories = df1$fbi_index,
#       categories = c(
#         "Murder and<br>Non-negligent<br>Manslaughter",
#         "Rape or<br>Sexual<br>Assault",
#         "Robbery",
#         "Aggravated<br>or Simple<br>Assault",
#         "Other<br>Violent<br>Offenses",
#         "Property",
#         "Public<br>Order",
#         "Drugs",
#         "Other or<br>Unknown"
#       ),
#       title = list(text = ""),
#       labels = list(
#         enabled = TRUE,
#         rotation = 0,
#         overflow = "allow",
#         allowOverlap = TRUE
#       ),
#       majorGridLineColor = "transparent",
#       gridLineColor = "transparent",
#       lineColor = "black",
#       majorGridLineColor = "transparent",
#       minorGridLineColor = "transparent",
#       tickColor = "black"
#     ) |>
#     hc_tooltip(pointFormat = '<b>{point.fbi_index}</b><br>Count: {point.n:,.0f}<br>Proportion: {point.prop}%') |>
#     hc_title(text = "Offense Breakdown for People in Prison Past Their Parole Eligibility Date") |>
#     hc_plotOptions(bubble = list(
#       minSize = 10,
#       maxSize = 50,
#       sizeBy = "area"
#     )) |>
#     hc_legend(enabled = FALSE) |>
#     hc_exporting(enabled = TRUE)
#
#   return(highcharts)
# })
#
# # Name the list of charts by state
# all_bubble_ped_fbi_index <- setNames(all_bubble_ped_fbi_index, states)
#
# # Display the chart for Georgia as an example
# all_bubble_ped_fbi_index$Georgia




# # Get unique states
# states <- unique(current_ped_fbi_index$state)
#
# # Create Highcharts visualizations for each state
# all_bar_ped_fbi_index <- map(.x = states, .f = function(x) {
#
#   df1 <- current_ped_fbi_index |>
#     filter(state == x) |>
#     mutate(prop = prop*100,
#            tooltip = paste0("<b>Offense:</b> ", fbi_index, "<br>",
#                             "<b>Count:</b> ", formattable::comma(n, 0), "<br>",
#                             "<b>Proportion:</b> ", round(prop, 1), "%"))
#
#   hc_accessibility_text <- paste0("TBD")
#
#   highcharts <- fnc_hc_barchart(df1, "fbi_index", "prop", hc_accessibility_text) |>
#     hc_yAxis(max = max(df1$prop)*1.5,
#              labels = list(
#                formatter = JS("function() {
#         return this.value + '%';
#       }")
#              )) |>
#     hc_title(text = "Offense Types for People in Prison Past Their Parole Eligibility Year") |>
#     hc_tooltip(pointFormat = "{point.tooltip}") |>
#     hc_colors(c(color3))
#
#   return(highcharts)
# })
#
# # Name the list of charts by state
# all_bar_ped_fbi_index <- setNames(all_bar_ped_fbi_index, states)
#
# # Display the chart for Georgia as an example
# all_bar_ped_fbi_index$Georgia
