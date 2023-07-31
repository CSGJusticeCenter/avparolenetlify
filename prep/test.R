# create a vector of all states
all_states <- state.name

# get parole eligibility information
parole_info_by_state_clean <- parole_info_by_state %>%
  select(state, abolished_discretionary_parole)

# parole_eligibility_table_2020 created in tab_parole_elgibility.R
# parole_info_by_state was imported in import.R
map_data <- parole_eligibility_table_2020 %>%

  # add missing states
  complete(state = all_states) %>%

  # format data and create tooltip
  mutate(current_perc = current_perc*100,
         future_1_5_years_perc  = future_1_5_years_perc*100,
         missing_perc = missing_perc*100,

         state_abb = state.abb[match(state, state.name)],

         tooltip =
           paste0("<b>", state, "</b><br><br>",
                  "Percent of Prison Population<br> Currently Eligible for Release:<br><b>",
                  paste(round(current_perc, 0), "%</b>", sep = ""), "<br><br>",
                  "Percent of Prison Population<br> Eligible for Parole in the Next 1-5 Years:<br><b>",
                  paste(round(future_1_5_years_perc, 0), "%</b>", sep = ""), "<br><br>",
                  "Percent of Prison Population<br> with Missing Parole Eligibility Data:<br><b>",
                  paste(round(missing_perc, 0), "%</b>", sep = ""), "<br><br>",
                  "Total Prison Population:<br><b>",
                  formattable::comma(yearendpop, digits = 0), "</b>"),
         tooltip = str_replace_all(tooltip, "NA%", "No Data")) %>%

  # create data labels
  mutate(datalabel = ifelse(is.na(current_perc),
                            paste0("", state_abb, ""),
                            paste0("<p style=", "text-align:center", ">",
                                   state_abb, "", "<br>",
                                   round(current_perc, 0), "%</p>")),

         currentperclabel = paste0(round(current_perc, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data")) %>%

  # add info about whether state abolished parole
  left_join(parole_info_by_state_clean, by = "state") %>%

  select(state, state_abb, current_count, datalabel, abolished_discretionary_parole)



# Define the gradient colors
gradient_colors <- c("#f7f7f7", "#fee0d2", "#fc9272", "#de2d26")
gradient_colors <- c("#D5F5F3", "#6AD0C9", "#00ABA0", "#006F8A", "#003474")

# Calculate the breaks for current_count
# breaks <- seq(min(map_data$current_count, na.rm = TRUE),
#               max(map_data$current_count, na.rm = TRUE),
#               length.out = length(gradient_colors) + 1)
num_breaks <- length(gradient_colors) + 1
breaks <- quantile(map_data$current_count, probs = seq(0, 1, length.out = num_breaks), na.rm = TRUE)


# Use mutate and case_when to assign gradient colors based on current_count values
map_data_breaks <- map_data %>%
  mutate(gradient_color = case_when(
    is.na(current_count) ~ NA_character_,
    current_count <= breaks[2] ~ gradient_colors[1],
    current_count <= breaks[3] ~ gradient_colors[2],
    current_count <= breaks[4] ~ gradient_colors[3],
    TRUE ~ gradient_colors[4]
  ))

# change color depending on value
map_data_new <- map_data_breaks %>%
  # mutate(gradient_color = case_when(
  #   is.na(gradient_color) & abolished_discretionary_parole == "Yes" ~ "gray",
  #   is.na(gradient_color) & abolished_discretionary_parole == "No" ~ "red",
  #   TRUE ~ gradient_color
  # )) %>%
  mutate(data_category = case_when(
    current_count >= 0 & current_count <= 8899 ~ "0 - 8,899",
    current_count >= 8900 & current_count <= 18701 ~ "8,900 - 18,701",
    current_count >= 18702 & current_count <= 51739 ~ "18,702 - 51,739",
    is.na(current_count) & abolished_discretionary_parole == "Yes" ~ "Abolished Parole",
    is.na(current_count) & abolished_discretionary_parole == "No" ~ "Missing Data"
  )) %>%
  mutate(data_category_num = case_when(
    current_count >= 0 & current_count <= 8899 ~ 0,
    current_count >= 8900 & current_count <= 18701 ~ 1,
    current_count >= 18702 & current_count <= 51739 ~ 2,
    is.na(current_count) & abolished_discretionary_parole == "Yes" ~ 3,
    is.na(current_count) & abolished_discretionary_parole == "No" ~ 4
  )) %>%
  select(state_abb, state, current_count, data_category, data_category_num, gradient_color)

map_data_new


highchart(height = 580) %>%

  hc_chart(marginTop = -6) %>%

  hc_add_series_map(
    map = hex_gj,
    df = map_data_new,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE, format = "{point.datalabel}",
                      style = list(fontSize = "14px",
                                   fontWeight = "regular",
                                   fontFamily = "Graphik",
                                   textOutline = 0)),
    nullColor = "#e8e8e8") %>%

  hc_colorAxis(dataClassColor="category",
               dataClasses = list(list(from=0, to=0, color="#6AD0C9", name="0 - 8,899"),
                                  list(from=1, to=1, color="#006F8A", name="8,900 - 18,701"),
                                  list(from=2, to=2, color="#", name="18,702 - 51,739"),
                                  list(from=3, to=3, color="#c376fb", name="Abolished Parole"),
                                  list(from=4, to=4, color="#e8e8e8", name="Missing Data"))) %>%
  hc_legend(enabled = TRUE)

  # hc_colorAxis(stops = map_data$gradient_color,
  #              reversed = FALSE,
  #              labels = list(format = "{value:,.0f}",
  #                            style = list(fontSize = "14px"))) %>%
  #
  # hc_legend(align = "right",
  #           verticalAlign = "bottom",
  #           layout = "vertical",
  #           symbolHeight = 200,
  #           symbolWidth = 25,
  #           x = -25,
  #           y = 0)
