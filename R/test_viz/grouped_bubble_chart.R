# Load required libraries
library(highcharter)

df1 <- current_ped_fbi_index |>
  filter(state == "Georgia") |>
  select(fbi_index, n) |>
  mutate(group = case_when(
    fbi_index %in% c("Murder and Non-negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drugs", "Public order", "Property") ~ "Non-Violent",
    TRUE ~ fbi_index
  ))

# Manual adjustments to group names
df1$group <- as.character(df1$group)
df1$group[df1$group == "Murder and Non-negligent Manslaughter"] <- "Murder and Non-negligent<br>Manslaughter"
df1$group[df1$group == "Rape or Sexual Assault"] <- "Rape or<br>Sexual Assault"
df1$group[df1$group == "Aggravated or Simple Assault"] <- "Aggravated or<br>Simple Assault"
df1$group[df1$group == "Other Violent Offenses"] <- "Other Violent"
df1$group[df1$group == "Other or Unknown"] <- "Other or<br>Unknown"

# Unique groups
groups <- unique(df1$group)

# Create the nested list structure
data <- lapply(groups, function(g) {
  items <- df1[df1$group == g, ]
  items_list <- lapply(1:nrow(items), function(i) {
    list(name = as.character(items$fbi_index[i]),
         value = items$n[i],
         color = case_when(g == "Violent" ~ red,
                           g == "Non-Violent" ~ green3,
                           TRUE ~ darkgray)) # Assuming color assignment based on group
  })
  list(name = g, data = items_list)
})

# Create the plot
hc <- highchart() |>
  hc_chart(type = "packedbubble",
           marginTop = 50, marginBottom = 50,
           marginLeft = 50, marginRight = 50) |>
  hc_add_series_list(data) |>
  hc_plotOptions(
    packedbubble = list(
      minSize = "20%",
      maxSize = "80%",
      layoutAlgorithm = list(
        splitSeries = TRUE,
        gravitationalConstant = 0.02,
        seriesInteraction = FALSE,
        parentNodeLimit = TRUE
      ),
      dataLabels = list(
        enabled = TRUE,
        useHTML = TRUE, # Use HTML to support line breaks
        format = '{point.name}',
        style = list(
          color = "black",
          textOutline = "none",
          fontWeight = "normal",
          fontSize = "10px", # Adjust the font size
          textAlign = "center" # Center text horizontally
        ),
        allowOverlap = TRUE
      )
    )
  ) |>
  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.value}") |>
  hc_colors(c(red, green3, darkgray)) |>
  hc_title(text = "Offense Breakdown for People in Prison Past Their Parole Eligibility Date") |>
  hc_add_theme(base_hc_theme)

# Display the plot
hc
