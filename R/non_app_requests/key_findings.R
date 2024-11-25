
#' Create an Icon Grid for the Homepage
#'
#' This function generates a grid of icons representing the Relative Rate Index (RRI),
#' with the first icon displayed in a distinct color (e.g., green), followed by a combination
#' of full, partial, and empty icons to represent the RRI value.
#'
#' @param rri_raw Numeric value of the RRI to represent.
#' @param rri_digits Integer specifying the number of decimal places to round the RRI.
#' @param fillcolor Character specifying the color for fully filled icons (default: "darkgray").
#' @param partialcolor Character specifying the color for partially filled icons (default: "white").
#' @param emptyhumans Logical indicating whether to include empty icons (default: TRUE).
#' @param emptycolor Character specifying the color for empty icons (default: "white").
#' @param infogs Integer specifying the total number of icons in the grid (default: `default_ncols`).
#' @param infogs_ncol Integer specifying the number of columns in the icon grid (default: `default_ncols`).
#' @param fillHoriz Logical indicating whether the icons should fill horizontally (default: FALSE).
#'
#' @return A ggplot object representing the grid of icons for the homepage.
#' @examples
#' # Generate a homepage icon grid for an RRI of 3.5
#' fnc_create_icons_homepage(rri_raw = 3.5, fillcolor = "blue")
fnc_create_icons_homepage <- function(rri_raw, rri_digits = 1, fillcolor = "darkgray", partialcolor = "white",
                                      emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
                                      infogs_ncol = default_ncols, fillHoriz = FALSE) {

  # Round the RRI value to the specified number of digits
  RRI <- round(rri_raw, digits = rri_digits)
  numfull <- floor(RRI)  # Number of fully filled icons
  numremain <- RRI - numfull  # Portion of the partially filled icon

  # Generate plot options for full, partial, and empty icons
  plot_opts <- fnc_icon_options(
    partialval = numremain,  # Partial fill value for partially filled icons
    empty = emptycolor,      # Color for empty icons
    fill = fillcolor,        # Color for fully filled icons
    partial = partialcolor,  # Color for partially filled icons
    fillHoriz = fillHoriz    # Direction of the fill (horizontal or vertical)
  )

  # Initialize a list to store the plots
  plot_list <- list()

  # Set the first icon to a distinct color (e.g., green)
  first_icon_color <- color4  # Customize this color as needed
  first_icon_opts <- fnc_icon_options(
    partialval = 0,          # No partial fill for the first icon
    empty = emptycolor,      # Color for empty background
    fill = first_icon_color, # Color for the first icon
    partial = first_icon_color, # Use the same color for partial fill
    fillHoriz = fillHoriz    # Direction of the fill
  )
  plot_list[[1]] <- first_icon_opts$full  # Add the first icon to the list

  # Create fully filled icons in gray for the remaining RRI value
  for (i in 2:numfull) {
    plot_list[[i]] <- plot_opts$full
  }

  # Add a partially filled icon if the RRI has a fractional part
  if (numremain > 0) {
    plot_list[[numfull + 1]] <- plot_opts$partial
  }

  # Add empty icons to complete the grid if needed
  if (emptyhumans && length(plot_list) < infogs) {
    for (i in (numfull + 2):infogs) {
      plot_list[[i]] <- plot_opts$empty
    }
  }

  # Determine the number of rows for the icon grid
  rows <- ifelse(infogs > infogs_ncol, ceiling(length(plot_list) / infogs_ncol), 1)

  # Return the grid of icons as a ggplot object
  plot_grid(plotlist = plot_list, nrow = rows)
}


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility

# In 2019, XXX Black, non-Hispanic individuals
# collectively spent YYYY years in prison past their parole eligibility year across
# UU states, serving an average of X years beyond parole eligibility, compared to X years for White individuals.
# ---------------------------------------------------------------------------- #

# Filter data to people currently eligible for parole or past parole eligibility
ncrp_current_pe <-   fnc_filter_pe_population_criteria(ncrp_yearendpop_not_consolidated,
                                                       exclude = states_to_exclude,
                                                       dont_filter = states_nofilter) |>
  filter(parelig_status == "Current") |>
  fnc_filter_exclude_high_missing_race(states_with_high_missing_race) |>
  # Join with which_overall_year to get the specific year for each state
  left_join(which_overall_year, by = "state") |>
  # Use mutate to create a year filter column and then filter
  mutate(year_to_filter = rptyear == year_to_use) |>
  filter(year_to_filter)

# Filter states that are using 2018 data
states_using_2018_data <- ncrp_current_pe |>
  filter(rptyear == 2018) |>
  pull(state) |>
  unique()

# Get average time between parole ligibility date and rpt year (years_to_estimated_pey) by state and race
avg_current_pe_race <- ncrp_current_pe |>
  filter(race %in% c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic, any race")) |>
  mutate(race = factor(race,
                       levels = c("Black, non-Hispanic", "White, non-Hispanic", "Hispanic, any race")),
         # All are negative or zero since they are past parole eligibility
         years_to_estimated_pey = abs(years_to_estimated_pey)) |>
  # Change negative to positive, negative means past parole eligibility year
  group_by(race) |>
  summarise(avg_years_to_estimated_pey = mean(years_to_estimated_pey, na.rm = TRUE),
            total_years_past_pe = sum(years_to_estimated_pey, na.rm = TRUE),
            people = n(),
            .groups = "drop")

# Number of states included and excluded
included_states <- unique(ncrp_current_pe$state)

# Separate states with high missing data and high missing data for race and ethnicity
high_missing_general_states <- unique(states_with_high_missing$state)
high_missing_race_states <- unique(states_with_high_missing_race$state)

# Combine both lists for the excluded states, ensuring no duplicates
excluded_states <- unique(c(high_missing_general_states, high_missing_race_states))

# Number of states in each category
count_high_missing_general_states <- length(high_missing_general_states)
count_high_missing_race_states <- length(high_missing_race_states)
count_excluded_states <- length(excluded_states)
count_included_states <- length(included_states)
count_abolished_states <- length(states_abolished_parole$state)

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "White, non-Hispanic"]),
    "White, non-Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "White, non-Hispanic"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years beyond parole eligibility.\n")

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "Black, non-Hispanic"]),
    "Black, non-Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "Black, non-Hispanic"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "Black, non-Hispanic"], 1),
    "years beyond parole eligibility, compared to",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years for White individuals.\n")

cat("In 2019,", comma(avg_current_pe_race$people[avg_current_pe_race$race == "Hispanic, any race"]),
    "Hispanic individuals collectively spent",
    comma(round(avg_current_pe_race$total_years_past_pe[avg_current_pe_race$race == "Hispanic, any race"], 1)),
    "years in prison past their parole eligibility year across",
    count_included_states, "states, serving an average of",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "Hispanic, any race"], 1),
    "years beyond parole eligibility, compared to",
    round(avg_current_pe_race$avg_years_to_estimated_pey[avg_current_pe_race$race == "White, non-Hispanic"], 1),
    "years for White individuals.\n")

# Add the note about states using 2018 data
cat("Note: The following states used 2018 data due to unreliable 2019 data:",
    paste(states_using_2018_data, collapse = ", "), ".\n")

# Print included and excluded states with counts
cat("Included states (", count_included_states, "):", paste(included_states, collapse = ", "), "\n")
cat("Excluded states (", count_excluded_states, "):", paste(excluded_states, collapse = ", "), "\n")

# Print states with high missing data and high missing data for race separately
cat("States with Unreliable PE Data (", count_high_missing_general_states, "):", paste(high_missing_general_states, collapse = ", "), "\n")
cat("States with High Missingness for Race/Ethnicity (", count_high_missing_race_states, "):", paste(high_missing_race_states, collapse = ", "), "\n")

# Print states that have abolished parole
cat("States Abolished Parole (", count_abolished_states, "):", paste(states_abolished_parole$state, collapse = ", "), "\n")


# ---------------------------------------------------------------------------- #
# Years Spent in Prison Past Parole Eligibility

# XXX people held past their parole eligibility year were convicted of non-violent
# offenses. Thousands of people who have not committed violent offenses remain incarcerated
# after their parole eligibility date in XX states. This includes XX people convicted
# of property offenses, XX convicted of drug offenses, and YY convicted of public order offenses.
# ---------------------------------------------------------------------------- #

# Categorize offenses into violent vs. nonviolent, keeping "Unknown" offenses for initial analysis
ncrp_current_pe_offense_initial <- ncrp_current_pe |>
  mutate(offense_group = case_when(
    fbi_index %in% c("Murder or Nonnegligent Manslaughter",
                     "Negligent Manslaughter",
                     "Rape or Sexual Assault",
                     "Robbery",
                     "Aggravated or Simple Assault",
                     "Other Violent Offenses") ~ "Violent",
    fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
    TRUE ~ fbi_index))

# Identify states with high missingness for offense type
states_with_high_missing_offense <- ncrp_current_pe_offense_initial |>
  group_by(state) |>
  summarise(unknown_offense_count = sum(offense_group == "Unknown"),
            total_count = n(),
            unknown_offense_percentage = (unknown_offense_count / total_count) * 100,
            .groups = "drop") |>
  filter(unknown_offense_percentage > 50) |>
  pull(state)  # Using a 50% threshold for high missingness

# Filter out "Unknown" offenses for further analysis
ncrp_current_pe_offense_filtered <- ncrp_current_pe_offense_initial |>
  filter(offense_group != "Unknown")

# Summarize counts for violent vs. nonviolent offenses
offense_summary <- ncrp_current_pe_offense_filtered |>
  group_by(offense_group) |>
  summarise(total_people = n(), .groups = "drop")

# Calculate percentages
total_people_overall <- sum(offense_summary$total_people)
violent_offense_count <- offense_summary$total_people[offense_summary$offense_group == "Violent"]
nonviolent_offense_count <- offense_summary$total_people[offense_summary$offense_group == "Nonviolent"]

percent_violent <- (violent_offense_count / total_people_overall) * 100
percent_nonviolent <- (nonviolent_offense_count / total_people_overall) * 100

# Check if each state has all nonviolent offense types (Property, Drug, Public Order)
states_with_all_offenses <- ncrp_current_pe_offense_filtered |>
  filter(offense_group == "Nonviolent") |>
  group_by(state) |>
  summarise(has_property = any(fbi_index == "Property"),
            has_drug = any(fbi_index == "Drug"),
            has_public_order = any(fbi_index == "Public Order"),
            .groups = "drop") |>
  filter(has_property & has_drug & has_public_order) |>
  pull(state)

# Count the number of states with all nonviolent offense types
count_states_with_all_offenses <- length(states_with_all_offenses)

# Combine high missingness states for different categories
all_high_missing_states <- unique(c(high_missing_general_states,
                                    high_missing_race_states,
                                    states_with_high_missing_offense))

# Separate states into included and excluded categories
excluded_states_with_offense <- unique(states_with_high_missing_offense)
excluded_states_overall <- unique(all_high_missing_states)
included_states_with_offense <- setdiff(included_states, excluded_states_overall)

# Update counts
count_excluded_states_overall <- length(excluded_states_overall)
count_included_states_with_offense <- length(included_states_with_offense)
count_high_missing_general_states <- length(high_missing_general_states)
count_high_missing_race_states <- length(high_missing_race_states)
count_high_missing_offense_states <- length(states_with_high_missing_offense)

# Print the updated results
cat(comma(nonviolent_offense_count), "people held past their parole eligibility year were convicted of non-violent offenses.\n")
cat("Thousands of people who have not committed violent offenses remain incarcerated after their parole eligibility date in",
    count_states_with_all_offenses, "states. This includes",
    comma(property_offense_count), "people convicted of property offenses,",
    comma(drug_offense_count), "convicted of drug offenses, and",
    comma(public_order_offense_count), "convicted of public order offenses.\n")

# Print percentages for violent and nonviolent offenses
cat("Overall, approximately", round(percent_violent, 1), "percent of individuals in prison past parole eligibility are serving time for violent offenses, while",
    round(percent_nonviolent, 1), "percent are serving time for non-violent offenses.\n")

# Print included and excluded states with details
cat("Included states with complete offense data (", count_included_states_with_offense, "):",
    paste(included_states_with_offense, collapse = ", "), "\n")
cat("Excluded states due to high missingness (", count_excluded_states_overall, "):",
    paste(excluded_states_overall, collapse = ", "), "\n")
cat("States with Unreliable PE Data (", count_high_missing_general_states, "):",
    paste(high_missing_general_states, collapse = ", "), "\n")
cat("States with High Missingness for Race/Ethnicity (", count_high_missing_race_states, "):",
    paste(high_missing_race_states, collapse = ", "), "\n")
cat("States with High Missingness for Offense Type (", count_high_missing_offense_states, "):",
    paste(states_with_high_missing_offense, collapse = ", "), "\n")

