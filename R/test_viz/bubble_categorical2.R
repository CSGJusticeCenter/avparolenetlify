library(highcharter)

# Expand the data to include all combinations of categories, race, and year
races <- c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race")
levels <- c("Murder and Non-negligent Manslaughter", "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
            "Other Violent Offenses", "Property", "Public order", "Drugs", "Other or Unknown")
years <- c("First Year", "Second Year", "Third Year or More")

df1 <- ncrp_time_between_ped_release |>
  filter(race != "Unknown" & race != "Other race(s), non-Hispanic") |>
  filter(state == "Georgia") |>
  group_by(state, fbi_index, race) |>
  count(time_between_ped_release_category2) |>
  mutate(prop = (n / sum(n)),
         prop = round(prop*100, 0)) |>
  arrange(desc(race)) |>
  ungroup() |>
  select(-state) |>
  rename(Level = fbi_index,
         Race = race,
         Year = time_between_ped_release_category2,
         Value = prop) |>
  droplevels()

# Create a complete grid of all possible combinations of Level, Race, and Year
complete_grid <- expand.grid(
  Level = levels(df1$Level),
  Race = levels(df1$Race),
  Year = levels(df1$Year)
)

# Convert to a tibble
complete_grid <- as_tibble(complete_grid)

# Merge the complete grid with the original data
df_complete <- complete_grid %>%
  left_join(df1, by = c("Level", "Race", "Year"))

# Replace NA values in the Value column with zero
df_complete <- df_complete %>%
  mutate(Value = replace_na(Value, 0))

# Create a mapping of categories to numerical values for y-axis
y_levels <- levels #rev(levels) # Reverse the levels order
y_mapping <- setNames(seq_along(y_levels) - 1, y_levels)
df_complete$y_value <- y_mapping[df_complete$Level]

# Create a mapping of years to numerical values for x-axis
year_mapping <- setNames(seq_along(years) - 1, years)
df_complete$x_value <- year_mapping[df_complete$Year] * length(races) + as.numeric(factor(df_complete$Race, levels = races)) - 1

# Create plot bands for years
plot_bands <- list(
  list(from = -0.5, to = 2.5, color = "lightgray", label = list(text = "First Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
  list(from = 2.5, to = 5.5, color = "white", label = list(text = "Second Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
  list(from = 5.5, to = 8.5, color = "lightgray", label = list(text = "Third Year or More", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold")))
)

# Define colors for races
color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color2, "Hispanic, any race" = color3)
df_complete$color <- color_mapping[df_complete$Race]

# Create the chart
highchart() |>
  hc_chart(type = "bubble", marginTop = 70) |>
  hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(style = list(fontWeight = "bold"))) |>
  hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
  hc_add_series(name = "", data = list_parse(data.frame(x = df_complete$x_value, y = df_complete$y_value, z = df_complete$Value, n = df_complete$n, color = df_complete$color, Race = df_complete$Race, Level = df_complete$Level, Year = df_complete$Year))) |>
  hc_title(text = "How Quickly People Are Released After Parole Eligibility Year by Race and Ethnicity") |>
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = '<b>Race:</b> {point.Race}<br><b>Level:</b> {point.Level}<br><b>Year:</b> {point.Year}<br><b>%:</b> {point.z}%<br><b>Count:</b> {point.n}<br>') |>
  hc_plotOptions(bubble = list(
    maxSize = 50,
    sizeBy = "area",
    dataLabels = list(
      enabled = TRUE,
      format = '{point.z}%',
      style = list(
        color = "black",
        textOutline = "none",
        fontWeight = "bold"
      )
    )
  )) |>
  hc_legend(enabled = FALSE) |>
  hc_exporting(enabled = FALSE) |>
  hc_add_theme(base_hc_theme)

