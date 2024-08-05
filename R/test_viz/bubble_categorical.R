
library(highcharter)
library(dplyr)
library(tidyr)

# Determine the timing of release categories: 1st year, 2nd year, etc
# Remove people with missing parole eligibility data and those released
# before their parole eligibility year
ncrp_time_between_ped_release <- ncrp_releases |>
  filter(rptyear == select_year) |>
  filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
           time_between_ped_release_category != "Released before Parole Eligibility Year" &
           !is.na(parelig_year) &
           !is.na(relyr)) |>
  mutate(time_between_ped_release_category2 = case_when(
    time_between_ped_release == 0 ~ "First Year",
    time_between_ped_release == 1 ~ "Second Year",
    time_between_ped_release >= 2 ~ "Third Year or More"
  )) |>
  mutate(time_between_ped_release_category2 =
           factor(time_between_ped_release_category2,
                  levels = c("Third Year or More",
                             "Second Year",
                             "First Year")))


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
  droplevels() |>
  mutate(Year = factor(Year,
                       levels = c(
                         "First Year",
                         "Second Year",
                         "Third Year or More"
                       )))

# Reverse the order of levels (crimes)
y_levels <- rev(levels)

# Create a new mapping of categories to numerical values for y-axis
y_mapping <- setNames(seq_along(y_levels) - 1, y_levels)

# Create a complete grid of all possible combinations of Level, Race, and Year
complete_grid <- expand.grid(
  Level = y_levels,
  Race = races,
  Year = years
)

# Convert to a tibble
complete_grid <- as_tibble(complete_grid)

# Merge the complete grid with the original data
df_complete <- complete_grid %>%
  left_join(df1, by = c("Level", "Race", "Year"))

# Replace NA values in the Value column with zero
df_complete <- df_complete %>%
  mutate(Value = replace_na(Value, 0))

# Update y_value after merging
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
color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color4, "Hispanic, any race" = color3)
df_complete$color <- color_mapping[df_complete$Race]

# Create the chart
highchart() |>
  hc_chart(type = "bubble", marginTop = 70) |>
  hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(style = list(fontWeight = "bold"))) |>
  hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
  hc_add_series(name = "", data = list_parse(data.frame(x = df_complete$x_value, y = df_complete$y_value, z = df_complete$Value, n = df_complete$n, color = df_complete$color, Race = df_complete$Race, Level = df_complete$Level, Year = df_complete$Year))) |>
  hc_title(text = "How Quickly People Are Released After Parole Eligibility Year by Race and Ethnicity") |>
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = '<b>Race:</b> {point.Race}<br><b>Level:</b> {point.Level}<br><b>Year:</b> {point.Year}<br><b>Percentage:</b> {point.z}%<br><b>Count:</b> {point.n}<br>') |>
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


# library(highcharter)
#
# # Determine the timing of release categories: 1st year, 2nd year, etc
# # Remove people with missing parole eligibility data and those released
# # before their parole eligibility year
# ncrp_time_between_ped_release <- ncrp_releases |>
#   filter(rptyear == select_year) |>
#   filter(time_between_ped_release_category != "Missing Parole Eligibility Year" &
#            time_between_ped_release_category != "Released before Parole Eligibility Year" &
#            !is.na(parelig_year) &
#            !is.na(relyr)) |>
#   mutate(time_between_ped_release_category2 = case_when(
#     time_between_ped_release == 0 ~ "First Year",
#     time_between_ped_release == 1 ~ "Second Year",
#     time_between_ped_release >= 2 ~ "Third Year or More"
#   )) |>
#   mutate(time_between_ped_release_category2 =
#            factor(time_between_ped_release_category2,
#                   levels = c("Third Year or More",
#                              "Second Year",
#                              "First Year")))
#
#
# # Expand the data to include all combinations of categories, race, and year
# races <- c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race")
# levels <- c("Murder and Non-negligent Manslaughter", "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
#             "Other Violent Offenses", "Property", "Public order", "Drugs", "Other or Unknown")
# years <- c("First Year", "Second Year", "Third Year or More")
#
# df1 <- ncrp_time_between_ped_release |>
#   filter(race != "Unknown" & race != "Other race(s), non-Hispanic") |>
#   filter(state == "Georgia") |>
#   group_by(state, fbi_index, race) |>
#   count(time_between_ped_release_category2) |>
#   mutate(prop = (n / sum(n)),
#          prop = round(prop*100, 0)) |>
#   arrange(desc(race)) |>
#   ungroup() |>
#   select(-state) |>
#   rename(Level = fbi_index,
#          Race = race,
#          Year = time_between_ped_release_category2,
#          Value = prop) |>
#   droplevels() |>
#   mutate(Year = factor(Year,
#                        levels = c(
#                          "First Year",
#                          "Second Year",
#                          "Third Year or More"
#                        )))
#
# # Reverse the order of levels (crimes)
# y_levels <- rev(levels)
#
# # Create a new mapping of categories to numerical values for y-axis
# y_mapping <- setNames(seq_along(y_levels) - 1, y_levels)
# df_complete$y_value <- y_mapping[df_complete$Level]
#
# # Create a complete grid of all possible combinations of Level, Race, and Year
# complete_grid <- expand.grid(
#   Level = y_levels,
#   Race = races,
#   Year = years
# )
#
# # Convert to a tibble
# complete_grid <- as_tibble(complete_grid)
#
# # Merge the complete grid with the original data
# df_complete <- complete_grid %>%
#   left_join(df1, by = c("Level", "Race", "Year"))
#
# # Replace NA values in the Value column with zero
# df_complete <- df_complete %>%
#   mutate(Value = replace_na(Value, 0))

# # Update y_value after merging
# df_complete$y_value <- y_mapping[df_complete$Level]
#
# # Create a mapping of years to numerical values for x-axis
# year_mapping <- setNames(seq_along(years) - 1, years)
# df_complete$x_value <- year_mapping[df_complete$Year] * length(races) + as.numeric(factor(df_complete$Race, levels = races)) - 1
#
# # Create plot bands for years
# plot_bands <- list(
#   list(from = -0.5, to = 2.5, color = "lightgray", label = list(text = "First Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
#   list(from = 2.5, to = 5.5, color = "white", label = list(text = "Second Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold"))),
#   list(from = 5.5, to = 8.5, color = "lightgray", label = list(text = "Third Year or More", align = "center", verticalAlign = "top", y = -10, style = list(color = "black", fontWeight = "bold")))
# )
#
# # Define colors for races
# color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color2, "Hispanic, any race" = color3)
# df_complete$color <- color_mapping[df_complete$Race]
#
# # Create the chart
# highchart() |>
#   hc_chart(type = "bubble", marginTop = 70) |>
#   hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(style = list(fontWeight = "bold"))) |>
#   hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
#   hc_add_series(name = "", data = list_parse(data.frame(x = df_complete$x_value, y = df_complete$y_value, z = df_complete$Value, n = df_complete$n, color = df_complete$color, Race = df_complete$Race, Level = df_complete$Level, Year = df_complete$Year))) |>
#   hc_title(text = "How Quickly People Are Released After Parole Eligibility Year by Race and Ethnicity") |>
#   hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = '<b>Race and Ethnicity:</b> {point.Race}<br><b>Offense:</b> {point.Level}<br><b>Year Released:</b> {point.Year} of Parole Eligibility<br><b>Percentage of Parole-Eligible People Released:</b> {point.z}%<br><b>People:</b> {point.n}<br>') |>
#   hc_plotOptions(bubble = list(
#     maxSize = 50,
#     sizeBy = "area",
#     dataLabels = list(
#       enabled = TRUE,
#       format = '{point.z}%',
#       style = list(
#         color = "black",
#         textOutline = "none",
#         fontWeight = "bold"
#       )
#     )
#   )) |>
#   hc_legend(enabled = FALSE) |>
#   hc_exporting(enabled = FALSE) |>
#   hc_add_theme(base_hc_theme)


# library(highcharter)
#
#
# df1 <- ncrp_time_between_ped_release |>
#   filter(race != "Unknown") |>
#   filter(state == "Georgia") |>
#   group_by(state, fbi_index, race) |>
#   count(time_between_ped_release_category2) |>
#   mutate(prop = (n / sum(n))) |>
#   arrange(desc(race)) |>
#   ungroup() |>
#   select(-state) |>
#   rename(Level = fbi_index,
#          Race = race,
#          Year = time_between_ped_release_category2,
#          Value = n) |>
#   select(-prop)
#
#
#
#
# # Define the data
# categories_x <- c("Other, non-Hispanic", "Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race", "Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race")
# categories_y <- c("Drugs", "Drugs", "Property", "Assault", "Property", "Assault", "Drugs")
# values <- c(90.4, 106.4, 71.5, 129.2, 144.0, 176.0, 40)
#
# # Expand the data to include all combinations of categories, race, and year
# races <- c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race", "Other, non-Hispanic")
# levels <- c("Murder", "Sexual Assault", "Robbery", "Other Violent", "Property", "Public Order", "Drugs", "Other or Unknown")
# years <- c("First Year", "Second Year", "Third Year")
#
# # Create a complete data frame
# complete_data <- expand.grid(Race = races, Level = levels, Year = years)
# complete_data$Value <- runif(nrow(complete_data), min = 20, max = 200) # Assign random values for demonstration
#
# # Create a mapping of categories to numerical values for y-axis
# y_levels <- rev(levels) # Reverse the levels order
# y_mapping <- setNames(seq_along(y_levels) - 1, y_levels)
# complete_data$y_value <- y_mapping[complete_data$Level]
#
# # Create a mapping of years to numerical values for x-axis
# year_mapping <- setNames(seq_along(years) - 1, years)
# complete_data$x_value <- year_mapping[complete_data$Year] * length(races) + as.numeric(factor(complete_data$Race, levels = races)) - 1
#
# # Create plot bands for years
# plot_bands <- list(
#   list(from = -0.5, to = 3.5, color = "lightgray", label = list(text = "First Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "Black, non-Hispanic", fontWeight = "bold"))),
#   list(from = 3.5, to = 7.5, color = "white", label = list(text = "Second Year", align = "center", verticalAlign = "top", y = -10, style = list(color = "Black, non-Hispanic", fontWeight = "bold"))),
#   list(from = 7.5, to = 11.5, color = "lightgray", label = list(text = "Third Year or More", align = "center", verticalAlign = "top", y = -10, style = list(color = "Black, non-Hispanic", fontWeight = "bold")))
# )
#
# # Define colors for races
# color_mapping <- c("Black, non-Hispanic" = color1, "White, non-Hispanic" = color2, "Hispanic, any race" = color4, "Other, non-Hispanic" = color3)
# complete_data$color <- color_mapping[complete_data$Race]
#
# # Create the chart
# highchart() |>
#   hc_chart(type = "bubble", marginTop = 70) |>
#   hc_xAxis(categories = rep(races, times = length(years)), plotBands = plot_bands, labels = list(style = list(fontWeight = "bold"))) |>
#   hc_yAxis(categories = y_levels, title = list(text = ""), type = "category", min = 0, max = length(y_levels) - 1) |>
#   hc_add_series(name = "Series 1", data = list_parse(data.frame(x = complete_data$x_value, y = complete_data$y_value, z = complete_data$Value, color = complete_data$color))) |>
#   hc_title(text = "How Quickly People Are Released After Parole Eligibility Year by Race and Ethnicity") |>
#   hc_tooltip(pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.z}</b><br/>') |>
#   hc_plotOptions(bubble = list(
#     #minSize = 10,
#     maxSize = 50,
#     sizeBy = "area"
#   )) |>
#   hc_legend(enabled = FALSE) |>
#   hc_exporting(enabled = FALSE) |>
#   hc_add_theme(base_hc_theme)

