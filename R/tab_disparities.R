# Filter and prepare the data
parole_release_disparities <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr) &
           !is.na(race) &
           time_between_ped_release >= 0 &
           reltype == "Conditional release") |>
  filter(admtype == "New court commitment") |>
  filter(sentlgth == "1-1.9 years" |
           sentlgth == "2-4.9 years" |
           sentlgth == "5-9.9 years" |
           sentlgth == "10-24.9 years") |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Other race(s), non-Hispanic")))






# Create Highcharts visualizations for each state
all_scatter_race_ped_release <- map(.x = states, .f = function(x) {

  df1 <- parole_release_disparities |>
    filter(state == x) |>
    select(time_between_ped_release, race)

  hc_accessibility_text <- paste0("Text TBD")

  highcharts <- hchart(df1, "scatter",
                       hcaes(x = race, y = time_between_ped_release, group = race)) |>
      hc_chart(type = "scatter") |>
      hc_colors(colors) |>
      hc_title(text = "Years Spent in Prison After Parole Eligibility by Race and Ethnicity") |>
      hc_xAxis(title = list(text = "")) |>
      hc_yAxis(title = list(text = ""), tickInterval = 1) |>
      hc_plotOptions(
        scatter = list(
          jitter = list(
            x = .25,
            y = .25
          ),
          marker = list(
            radius = 2,
            symbol = 'circle'
          ),
          tooltip = list(
            pointFormat = 'Time Between Parole Eligibility Year<br>and Release Year: {point.y:.0f}'
          ),
          showInLegend = FALSE
        )
      ) |>
      hc_colors(c(green2, yellow, purple, red)) |>
      hc_add_theme(hc_theme_with_line) |>
    hc_exporting(enabled = TRUE)
    return(highcharts)
})

# Name the list of charts by state
all_scatter_race_ped_release <- setNames(all_scatter_race_ped_release, states)

# Display the chart for Georgia as an example
all_scatter_race_ped_release$Georgia



#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_scatter_race_ped_release, file = file.path(folder, "all_scatter_race_ped_release.rds"))
}

