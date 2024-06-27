#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizatons for national trends page
#######################################


#------ Parole Eligibility Table ------#

# Get total prison population by state and year
total_pop_by_year <- ncrp_yearendpop %>%
  group_by(state, rptyear) %>%
  summarise(total_pop = n(), .groups = 'drop')

# Filter data to people in prison for a new court commitment 1-25 year sentence lengths
# Not including people who are failing supervision (parole return/revocation)
filtered_ncrp_yearendpop <- ncrp_yearendpop |>
  filter(admtype == "New court commitment",
         sentlgth %in% c("1-1.9 years", "2-4.9 years", "5-9.9 years", "10-24.9 years"))

# Get total prison population for new court commitments and sentence length 1-25 years
filtered_pop_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear) |>
  summarise(filtered_total_pop = n(), .groups = 'drop')

# Get number of people in prison by parole eligibility status for the specified criteria
# Get proportion of parole eligibility statuses out of everyone in the filtered population
filtered_parole_status_by_year <- filtered_ncrp_yearendpop |>
  group_by(state, rptyear, parelig_status) |>
  summarise(count = n(), .groups = 'drop') |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  mutate(proportion = count / filtered_total_pop)

# Reshape data for table
filtered_parole_elig_table_by_year <- filtered_parole_status_by_year |>
  pivot_longer(cols = c(count, proportion), names_to = "metric", values_to = "value") |>
  mutate(metric_name = case_when(
    metric == "count" ~ paste(parelig_status, "count"),
    metric == "proportion" ~ paste(parelig_status, "perc.")
  )) |>
  select(state, rptyear, metric_name, value) |>
  pivot_wider(names_from = metric_name, values_from = value) |>
  clean_names()

# Filter to select analysis year specified in the config file
filtered_parole_elig_table_analysis_year_with_missing_states <- filtered_parole_elig_table_by_year |>
  filter(rptyear == analysis_year)

# Find missing states and combine with the original dataframe
missing_states <- tibble(state = setdiff(state.name, filtered_parole_elig_table_analysis_year_with_missing_states$state),
                         rptyear = analysis_year)

# Add missing states to table so we have a complete table of 50 states
filtered_parole_elig_table_analysis_year <- filtered_parole_elig_table_analysis_year_with_missing_states |>
  bind_rows(missing_states) |>
  left_join(total_pop_by_year, by = c("state", "rptyear")) |>
  left_join(filtered_pop_by_year, by = c("state", "rptyear")) |>
  arrange(state) |>
  select(state, rptyear, total_pop, filtered_total_pop,
         contains("current"), contains("future_1_5_years"), contains("future_6_years"), contains("missing"))



#------ Parole Eligibility Maps (%) ------#

# Create a vector of all state names
all_states <- state.name

# Get parole status information by state
parole_info_by_state_clean <- parole_info_by_state |>
  select(state, abolished_discretionary_parole)

# Prepare map data for displaying counts
map_percent_data <- filtered_parole_elig_table_analysis_year %>%

  # Add missing states
  complete(state = all_states) %>%

  # Add info about whether state abolished parole release
  left_join(parole_info_by_state_clean, by = "state") %>%

  mutate(state_abb = state.abb[match(state, state.name)],
         all_na = rowSums(is.na(select(.,
                                       current_count,
                                       future_1_5_years_count,
                                       missing_count))) ==
           length(select(.,
                         current_count,
                         future_1_5_years_count,
                         missing_count))
  )




library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(jsonlite)

# Load the shapefile
hex_gj <- read_sf(paste0(config$sp_data_path, "/data/raw/us_states_hexgrid.geojson")) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC") %>%
  st_transform(3857)

# Convert to a data frame for manipulation
hex_gj_df <- st_as_sf(hex_gj) %>%
  mutate(state = state.name[match(state_abb, state.abb)])

# Convert parole_info_by_state_clean to a data frame
parole_info_by_state_clean <- as.data.frame(parole_info_by_state_clean)

# Join with hex data
hex_data <- hex_gj_df %>%
  left_join(parole_info_by_state_clean, by = "state")

# Define colors
highlight_colors <- c("No" = colors$red, "Yes" = colors$blue)
other_color <- "white"

# Base plot with gray tiles
base_plot <- ggplot() +
  geom_sf(data = hex_data, aes(geometry = geometry), fill = other_color, color = "black") +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "Graphik")
  )

# Map for states that have not abolished discretionary parole
map1 <- base_plot +
  geom_sf(data = hex_data %>% filter(abolished_discretionary_parole == "No"), aes(fill = abolished_discretionary_parole), color = "black") +
  # labs(
  #   title = "32",
  #   subtitle = "States with Parole\nand Parole Boards"
  # ) +
  scale_fill_manual(values = highlight_colors) +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )

# Map for states that have abolished discretionary parole
map2 <- base_plot +
  geom_sf(data = hex_data %>% filter(abolished_discretionary_parole == "Yes"), aes(fill = abolished_discretionary_parole), color = "black") +
  # labs(
  #   title = "16",
  #   subtitle = "States that Discretionary\nAbolished Parole"
  # ) +
  scale_fill_manual(values = highlight_colors) +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )

# Arrange the two maps side by side
combined_map <- plot_grid(map1, map2, ncol = 2)
print(combined_map)




# Define the file path for the high-resolution image
output_file <- "combined_map_high_res.png"

# Save the combined map
ggsave(filename = output_file, plot = combined_map, width = 20, height = 10, dpi = 300)

#------ Save Data ------#


theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(filtered_parole_elig_table_analysis_year, file = file.path(folder, "filtered_parole_elig_table_analysis_year.rds"))

}


