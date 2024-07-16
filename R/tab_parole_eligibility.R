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

  df1 <- ncrp_pes_subset %>%
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
                  color = colors$darkgray,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  // Adjust this threshold as needed
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 6+ Years",
                  data = list(list(y = df1$prop[3], tooltip = df1$tooltip[3], label = df1$prop_label[3])),
                  stack = "a",
                  color = colors$green3,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  // Adjust this threshold as needed
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Future 1-5 Years",
                  data = list(list(y = df1$prop[2], tooltip = df1$tooltip[2], label = df1$prop_label[2])),
                  stack = "a",
                  color = colors$green2,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  // Adjust this threshold as needed
                      return this.point.label;
                    }
                    return null;
                  }"),
                    style = list(fontSize = "12px", color = "#ffffff", textOutline = "none")
                  )) |>
    hc_add_series(name = "Current",
                  data = list(list(y = df1$prop[1], tooltip = df1$tooltip[1], label = df1$prop_label[1])),
                  stack = "a",
                  color = colors$red,
                  dataLabels = list(
                    enabled = TRUE,
                    formatter = JS("function() {
                    if (this.y > 0.05) {  // Adjust this threshold as needed
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
    hc_add_theme(base_hc_theme)

  return(highcharts)
})

all_stackedbar_pe_type <- setNames(all_stackedbar_pe_type, states)
all_stackedbar_pe_type$Georgia



#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){
  save(all_stackedbar_pe_type, file = file.path(folder, "all_stackedbar_pe_type.rds"))
}
