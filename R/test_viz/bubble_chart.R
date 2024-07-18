data <- structure(list(category = structure(1:9, levels = c("Murder and Non-negligent Manslaughter",
                                                            "Rape or Sexual Assault", "Robbery", "Aggravated or Simple Assault",
                                                            "Other Violent Offenses", "Property", "Public order", "Drugs",
                                                            "Other or Unknown"), class = "factor"), value = c(571L, 1802L,
                                                                                                              900L, 3068L, 860L, 2216L, 1392L, 987L, 31L)), row.names = c(NA,
                                                                                                                                                                          -9L), class = c("tbl_df", "tbl", "data.frame"))

# Manual adjustments to category names
data$category <- as.character(data$category)
data$category[data$category == "Murder and Non-negligent Manslaughter"] <- "Murder and Non-negligent<br>Manslaughter"
data$category[data$category == "Rape or Sexual Assault"] <- "Rape or<br>Sexual Assault"
data$category[data$category == "Aggravated or Simple Assault"] <- "Aggravated or<br>Simple Assault"
data$category[data$category == "Other Violent Offenses"] <- "Other Violent"
data$category[data$category == "Other or Unknown"] <- "Other or<br>Unknown"

# Define color for each category
data$color <- ifelse(data$category %in% c("Murder and Non-negligent<br>Manslaughter",
                                          "Rape or<br>Sexual Assault",
                                          "Aggravated or<br>Simple Assault",
                                          "Other Violent"),
                     purple, green2)

# Create packed bubble chart
highchart() %>%
  hc_chart(type = "packedbubble") %>%
  hc_title(text = "Packed Bubble Chart Example") %>%
  hc_series(
    list(
      name = "Categories",
      data = lapply(1:nrow(data), function(i) {
        list(
          name = data$category[i],
          value = data$value[i],
          color = data$color[i]
        )
      })
    )
  ) %>%
  hc_tooltip(pointFormat = "<b>{point.name}:</b> {point.value}") %>%
  hc_plotOptions(packedbubble = list(
    minSize = "30%",
    maxSize = "100%",
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
      align = "center", # Center data labels horizontally
      verticalAlign = "middle" # Center data labels vertically
    )
  ))
