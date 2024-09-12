#######################################
# Project: AV Parole
# File: tab_population.R
# Authors: Mari Roberts
# Date last updated: August 5, 2024 (MAR)
# Description:
#    Prison population visualizations and findings for population tab
#    Uses BJS Prisoners Data
#######################################

# # Create a dataframe with our filtered criteria
# # Only interested in people in prison for new court commitments and
# # with sentence lengths between 1-25 years
# ncrp_yearendpop_filtered <- filter_population_criteria(ncrp_yearendpop)
#
# # Generate graph for each state
# states <- unique(bjs_prison_pop_by_rptyear$state)
# all_line_population_by_year <- map(.x = states,  .f = function(x) {
#
#   df1 <- bjs_prison_pop_by_rptyear |>
#     ungroup() |>
#     filter(state == x) |>
#     distinct() |>
#     mutate(tooltip =
#              paste0(
#                "Year: ", rptyear, "<br>",
#                "Year-End Population: ", bjs_prison_population))
#
#   df2 <- ncrp_yearendpop_filtered |>
#     filter(parelig_status == "Current") |>
#     group_by(rptyear, state) |>
#     summarise(n = n())
#
#   # Determine the maximum value for the y-axis in the visualization
#   # Adds a small margin space at the top
#   max_value <- max(df1$bjs_prison_population)*1.1
#   min_value <- min(df1$bjs_prison_population)/1.5
#
#   hc_accessibility_text <- paste0("TBD")
#
#   highcharts <- # Create the line chart
#     hc <- highchart() |>
#     hc_chart(type = "line") |>
#     hc_title(text = "Prison Population by Year") |>
#     hc_yAxis(title = list(text = ""),
#              min = min_value,
#              max = max_value) |>
#     hc_xAxis(categories = df1$rptyear,
#              lineWidth = 1) |>
#     hc_series(
#       list(
#         name = "population",
#         data = df1$bjs_prison_population,
#         tooltip = list(
#           # pointFormat = "Year: {point.category}<br>Prison Population: {point.y}"
#           pointFormat = "<b>Prison Population:</b> {point.y}"
#         )
#       )
#     ) |>
#     hc_add_theme(hc_theme_with_line) |>
#     hc_legend(enabled = FALSE) |>
#     hc_exporting(enabled = TRUE) |>
#     hc_colors(c(color2))
#
#   return(highcharts)
# })
# all_line_population_by_year <- setNames(all_line_population_by_year, states)
# all_line_population_by_year$Georgia

# ---------------------------------------------------------------------------- #
# PE Prison Population Trends
# ---------------------------------------------------------------------------- #

# Summarize current and total population
current_pe_pop <- ncrp_yearendpop_filtered |>
  filter(parelig_status == "Current") |>
  group_by(state, rptyear) |>
  summarise(n = n()) |>
  mutate(type = "Current")

ncrp_pop <- ncrp_yearendpop_filtered |>
  group_by(state, rptyear) |>
  summarise(n = n()) |>
  mutate(type = "Total Population")

ncrp_current_pe_pop <- rbind(current_pe_pop, ncrp_pop)

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

# Create a dataframe with our filtered criteria
# Only interested in people in prison for new court commitments and
# with sentence lengths between 1-25 years
ncrp_yearendpop_filtered <- filter_population_criteria(ncrp_yearendpop)

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













# ---------------------------------------------------------------------------- #
# PE Prison Population by Demographics, Offense Type, Sentence Length
# ---------------------------------------------------------------------------- #

state_pe_race      <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, race)
state_pe_sex       <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, sex)
state_pe_ageyrend  <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, ageyrend)
state_pe_sentlgth  <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, sentlgth)
state_pe_fbi_index <- fnc_prepare_pe_data(df = ncrp_yearendpop_filtered, fbi_index)

# Example of calling the function for race
all_stacked_bar_pe_race <- map(.x = states, .f = function(x) {
  state_data <- state_pe_race |> filter(state == x)
  fnc_hc_stackedbar_pe_population(
    df = state_data,
    count_column = race,
    title = "Race and Ethnicity",
    subtitle = "Prison Population by Parole Eligibility Status",
    categories_col = "race",
    colors = c(darkgray, color2, color4))
})
all_stacked_bar_pe_race <- setNames(all_stacked_bar_pe_race, states)
all_stacked_bar_pe_race$Georgia

# Example of calling the function for sex
all_stacked_bar_pe_sex <- map(.x = states, .f = function(x) {
  state_data <- state_pe_sex |> filter(state == x)
  fnc_hc_stackedbar_pe_population(df = state_data,
                                  count_column = sex,
                                  title = "Sex",
                                  subtitle = "Prison Population by Parole Eligibility Status",
                                  categories_col = "sex",
                                  colors = c(darkgray, color2, color4))
})
all_stacked_bar_pe_sex <- setNames(all_stacked_bar_pe_sex, states)
all_stacked_bar_pe_sex$Georgia


# Example of calling the function for age
all_stacked_bar_pe_ageyrend <- map(.x = states, .f = function(x) {
  state_data <- state_pe_ageyrend |> filter(state == x)
  fnc_hc_stackedbar_pe_population(df = state_data,
                                  count_column = ageyrend,
                                  title = "Age",
                                  subtitle = "Prison Population by Parole Eligibility Status",
                                  categories_col = "ageyrend",
                                  colors = c(darkgray, color2, color4))
})
all_stacked_bar_pe_ageyrend <- setNames(all_stacked_bar_pe_ageyrend, states)
all_stacked_bar_pe_ageyrend$Georgia

# Example of calling the function for sentence length
all_stacked_bar_pe_sentlgth <- map(.x = states, .f = function(x) {
  state_data <- state_pe_sentlgth |> filter(state == x)
  fnc_hc_stackedbar_pe_population(df = state_data,
                                  count_column = sentlgth,
                                  title = "Sentence Length",
                                  subtitle = "Prison Population by Parole Eligibility Status",
                                  categories_col = "sentlgth",
                                  colors = c(darkgray, color2, color4))
})
all_stacked_bar_pe_sentlgth <- setNames(all_stacked_bar_pe_sentlgth, states)
all_stacked_bar_pe_sentlgth$Georgia

# Example of calling the function for offense type
all_stacked_bar_pe_fbi_index <- map(.x = states, .f = function(x) {
  state_data <- state_pe_fbi_index |> filter(state == x)
  state_data$fbi_index <- factor(state_data$fbi_index, levels = rev(levels(state_data$fbi_index)))
  fnc_hc_stackedbar_pe_population(
    df = state_data,
    count_column = fbi_index,
    title = "Offense Type",
    subtitle = "Prison Population by Parole Eligibility Status",
    categories_col = "fbi_index",
    colors = c(darkgray, color2, color4))
})
all_stacked_bar_pe_fbi_index <- setNames(all_stacked_bar_pe_fbi_index, states)
all_stacked_bar_pe_fbi_index$Georgia




#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

save(all_stackedbar_pe_type, file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stackedbar_pe_type.rds"))
save(all_sentence_pe_type,   file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_sentence_pe_type.rds"))

save(all_stacked_bar_pe_race,      file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stacked_bar_pe_race.rds"))
save(all_stacked_bar_pe_sex,       file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stacked_bar_pe_sex.rds"))
save(all_stacked_bar_pe_ageyrend,  file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stacked_bar_pe_ageyrend.rds"))
save(all_stacked_bar_pe_fbi_index, file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stacked_bar_pe_fbi_index.rds"))
save(all_stacked_bar_pe_sentlgth,  file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_stacked_bar_pe_sentlgth.rds"))



#------------------------------------------------------------------------------#
# SAVE DATA
#------------------------------------------------------------------------------#

save(all_line_pop_pe_by_year, file = file.path(paste0(config$sp_data_path, "/data/analysis/app"), "all_line_pop_pe_by_year.rds"))


