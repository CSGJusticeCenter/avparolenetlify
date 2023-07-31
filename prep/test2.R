# create a vector of all states
all_states <- state.name

# get parole eligibility information
parole_info_by_state_clean <- parole_info_by_state %>%
  select(state, abolished_discretionary_parole)

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

  select(state, state_abb, current_perc, datalabel, abolished_discretionary_parole)



# define the gradient colors
gradient_colors <- c("#D5F5F3", "#6AD0C9", "#00ABA0", "#006F8A", "#003474")

# calculate the breaks for current_perc
num_breaks <- length(gradient_colors) + 1
breaks <- quantile(map_data$current_perc, probs = seq(0, 1, length.out = num_breaks), na.rm = TRUE)


# Use mutate and case_when to assign gradient colors based on current_perc values
map_data_breaks <- map_data %>%
  mutate(gradient_color = case_when(
    is.na(current_perc) ~ NA_character_,
    current_perc <= breaks[2] ~ gradient_colors[1],
    current_perc <= breaks[3] ~ gradient_colors[2],
    current_perc <= breaks[4] ~ gradient_colors[3],
    TRUE ~ gradient_colors[4]
  ))

# change color depending on value
map_data_new <- map_data_breaks %>%
  mutate(
    current_perc = round(current_perc, 0)) %>%
  group_by(gradient_color) %>%
  mutate(data_category = paste0(min(current_perc), "% - ",max(current_perc), "%")) %>%
  mutate(data_category = case_when(
    data_category == "NA% - NA%" & abolished_discretionary_parole == "Yes" ~ "Abolished Parole",
    data_category == "NA% - NA%" & abolished_discretionary_parole == "No" ~ "Missing Data",
    TRUE ~ data_category
  )) %>%
  mutate(data_category_num = case_when(
    data_category == "0% - 5%" ~ 0,
    data_category == "6% - 10%" ~ 1,
    data_category == "11% - 19%" ~ 2,
    data_category == "22% - 54%" ~ 3,
    data_category == "Abolished Parole" ~ 4,
    data_category == "Missing Data" ~ 5
  ))


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
               dataClasses = list(list(from = 0, to = 0, color="#D5F5F3", name = "0% - 5%"),
                                  list(from = 1, to = 1, color="#6AD0C9", name = "6% - 10%"),
                                  list(from = 2, to = 2, color="#00ABA0", name = "11% - 19%"),
                                  list(from = 3, to = 3, color="#006F8A", name = "22% - 54%"),
                                  list(from = 4, to = 4, color= yellow, name = "Abolished Parole"),
                                  list(from = 5, to = 5, color="#e8e8e8", name = "Missing Data")
                                  )) %>%
  hc_legend(align = "right",
          verticalAlign = "bottom",
          layout = "vertical",
          symbolHeight = 10,
          symbolWidth = 10,
          x = 0,
          y = -100)
