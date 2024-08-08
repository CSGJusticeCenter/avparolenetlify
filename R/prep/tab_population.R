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

  # Find the earliest and latest year prison populations
  earliest_year <- min(df1$rptyear)
  latest_year <- max(df1$rptyear)
  earliest_year_population <- df1$bjs_prison_population[df1$rptyear == earliest_year]
  latest_year_population <- df1$bjs_prison_population[df1$rptyear == latest_year]

  # Calculate the percent change
  percent_change <- (latest_year_population - earliest_year_population) / earliest_year_population * 100
  change_type <- ifelse(percent_change < 0, "decreased", "increased")
  percent_change_abs <- abs(round(percent_change, 0))

  sentences <- paste0("From ", earliest_year, " to ", latest_year, ", the prison population <b>",
                      change_type, " ", percent_change_abs, "%</b>, dropping from ",
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













#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_line_population_by_year, file = file.path(folder, "all_line_population_by_year.rds"))
  save(all_sentence_population,     file = file.path(folder, "all_sentence_population.rds"))
}














