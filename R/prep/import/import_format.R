################################################################################
# Project: AV Parole
# File: import_format.R
# Authors: Mari Roberts
# Date last updated: December 5, 2024 (MAR)
# Description:
#    This script handles the import, transformation, and integration of multiple
#    datasets for the AV Parole project. It includes:
#    - Importing and processing the hex map shape file for the interactive map on
#      National Snapshot page.
#    - Importing state-specific notes and rules for data exclusions, missingness,
#      and projections.
#    - Importing, combining, and preparing National Corrections Reporting Program (NCRP)
#      data files, including year end populations, releases, as well as projections and imputations
#      by Seba Guzman (CSG Research in Stata) for parole eligibility and prison populations.
#    - Importing and preparing Bureau of Justice Statistics (BJS) data for prison
#      populations by year, race, ethnicity, and sex. These numbers are more reliable
#      than NCRP and reflect what states would expect.
#    - Saving processed data objects for use in downstream analysis and visualization.
################################################################################

#------------------------------------------------------------------------------#
# Hex Map - National Snapshot Page
#------------------------------------------------------------------------------#

# Import hex map shapefile
# Remove the District of Columbia (DC) and transform the spatial data to EPSG:3857
hex_gj <- read_sf(file.path(sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
    select(state_abb = iso3166_2) |>
    filter(state_abb != "DC") |>
    st_transform(3857) |>
    sf_geojson() |>
    fromJSON(simplifyVector = FALSE)

#------------------------------------------------------------------------------#
# States Rules

# Import state-specific rules from an external Excel file.
# These rules guide the inclusion, exclusion, and handling of states based on data quality,
# filtering requirements, and special cases related to parole eligibility data.

# The state rules include:
# - States excluded from the tool entirely due to issues like abolished parole systems.
# - States not requiring filtering by admission type or sentence length due to missing data,
#   where the submitted data sufficiently represents the intended population.
# - States where caution is needed because data on people past parole eligibility is likely undercounted.
# - States excluded from state-specific reports but included in the National Snapshot page.
# - States where "Other race(s), non-Hispanic" populations are significant enough for disparities analysis.
# - States where the earliest parole eligibility year (PEY1) is more reliable than imputed values.
#------------------------------------------------------------------------------#

# Import xlsx file
state_rules <- read_excel(file.path(sp_data_path, "data/raw/NCRP Data Rules/state_rules.xlsx"))

# Identify states that do not require filtering by admission type and sentence length
# These states have high missingness in these variables, but the estimates for people past parole eligibility
# are reliable or align closely with the intended population.
states_nofilter <- state_rules |>
  filter(dont_filter_admtype_sentlength == 1)

# Identify states where the earliest parole eligibility year (PEY1) should be used
# instead of imputed values for parole eligibility due to reliability concerns.
states_earliest_pe <- state_rules |>
  filter(use_earliest_pey1 == 1)

# Identify states where the number of people past parole eligibility is likely undercounted
# due to limitations or inconsistencies in the data.
states_undercounted <- state_rules |>
  filter(likely_undercount == 1)

# Identify states to be included in the National Snapshot page but excluded from
# state-specific reports due to specific criteria or data quality issues.
states_national_page_only <- state_rules |>
  filter(exclude_from_reports == 1) |>
  select(state)

# Identify states where "Other race(s), non-Hispanic" populations are significant enough
# to include in the disparities analysis. Examples: Hawaii, Alaska, New Mexico, Oklahoma.
states_use_other_race_eth <- state_rules |>
  filter(use_other_race_ethnicity == 1)


#------------------------------------------------------------------------------#
# State-Specific Notes for "How is Parole Eligibility Determined?" and
# Methodology of Imputation for "Estimation Methodology" Section of Parole Eligibility Tab
#------------------------------------------------------------------------------#

# Import state-specific imputation methodology
# The file was created by Seba Guzman (CSG Research) in Stata and contains
# methodology details for imputing state-level parole eligibility information.
state_methodology <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/state_notes_2020.dta"))

# Clean source notes of leading and trailing white space
# Remove blank sources
state_methodology_clean <- state_methodology %>%
  mutate(across(starts_with("source_note"), str_trim)) |>
  mutate(across(starts_with("source_note"),
                ~ if_else(. == "³", NA_character_, .)))

#------------------------------------------------------------------------------#
# "How is Parole Eligibility Determined?"
#------------------------------------------------------------------------------#

# Import state notes
# This data was created by the Policy Team in the 'Release Systems by State' Word document.
# Includes:
# - "How is Parole Eligibility Determined?" information
# - Number of parole board members
# - Citations
state_notes_raw <- read.csv(file.path(sp_data_path, "data/raw/Carl State Notes/av_parole_state_notes.csv")) |>
  clean_names() |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(
    # Combine PE citations, skipping missing ones
    pe_citation = paste0(
      pe_citation_1,
      ifelse(!is.na(pe_citation_2) & pe_citation_2 != "", "<br><br>", ""),
      ifelse(!is.na(pe_citation_2) & pe_citation_2 != "", pe_citation_2, ""),
      ifelse(!is.na(pe_citation_3) & pe_citation_3 != "", "<br><br>", ""),
      ifelse(!is.na(pe_citation_3) & pe_citation_3 != "", pe_citation_3, "")
    ),

    # Combine PB citations, skipping missing ones
    pb_citation = paste0(
      pb_citation_1,
      ifelse(!is.na(pb_citation_2) & pb_citation_2 != "", "<br><br>", ""),
      ifelse(!is.na(pb_citation_2) & pb_citation_2 != "", pb_citation_2, "")
    ),

    # Add superscript 1 to PB citation
    pb_citation = paste("\u00B9", pb_citation, sep = " "),

    # Add superscript 2 to PE citation
    pe_citation = paste("\u00B2", pe_citation, sep = " "),

    # Add superscript 2 to `release_systems` for "How Parole Eligibility is Determined" section
    release_systems = paste0(release_systems, "\u00B2")
  )


#------------------------------------------------------------------------------#
# Combine All State Notes and Methodologies and Adjust Formatting
#------------------------------------------------------------------------------#

# Merge state notes with state-specific imputation methodology from Seba Guzman.
# Adjust formatting to ensure consistency with the Parole Eligibility tab display.
# Note on Superscripts:
# Superscripts are used to provide references and citations throughout the Parole
# Eligibility (PE) tab. Each type of note is assigned a unique superscript number:
# - Superscript 2 (²) is used for notes related to "How Parole Eligibility is Determined".
# - Seba Guzman's citations start at 1, but 1 is already being used in the PE tab, therefore
#   superscript numbers are incremented for methodology-related notes and citations:
#   - ¹ becomes ³, ² becomes ⁴, and so on.
# These adjustments ensure clarity and proper alignment with the visual layout of
# the tab. Superscripts are encoded as Unicode characters for compatibility:
# - \u00B9 (¹), \u00B2 (²), \u00B3 (³), \u2074 (⁴), \u2075 (⁵), etc.
state_notes <- state_notes_raw |>
  left_join(state_methodology_clean, by = "state") |>

  # Ensure proper formatting for notes:
  mutate(
    # Append a period to `matching_note` if missing
    matching_note = paste0(matching_note, "."),

    # Increment superscripts for notes by 1 to align with numbering conventions:
    # Superscript 1 (\u00B2) becomes 3 (\u00B3), and so on.
    estimation_note = gsub("\u00B9", "\u00B3", estimation_note),
    rules_note      = gsub("\u00B2", "\u2074", rules_note),
    projection_note = gsub("\u00B3", "\u2075", projection_note),

    # Apply the same logic to source note columns
    source_note1 = gsub("\u00B9", "\u00B3", source_note1),
    source_note2 = gsub("\u00B2", "\u2074", source_note2),
    source_note3 = gsub("\u00B3", "\u2075", source_note3),

    # Combine all imputation methodology details into a single HTML-compatible string
    methodology_notes = paste(estimation_note,
                              matching_note,
                              rules_note,
                              last_year_note,
                              manual_checks_note,
                              year_excluded_note,
                              projection_note,
                              sep = "<br><br>"),

    # Combine all citations into a single field for display
    citation = paste(pb_citation, pe_citation, source_note1, source_note2, source_note3, sep = "<br><br>"),

    # Remove unnecessary blank lines from the formatted output
    methodology_notes = gsub("<br><br><br>", "<br>", methodology_notes),
    citation = gsub("<br><br><br>", "<br><br>", citation)
  )

# Format citations and URLs
state_notes <- state_notes |>
  mutate(citation = sapply(citation, fnc_format_citation))

# Identify states where parole has been abolished.
# These states are excluded from the analysis and tool.
# Output will not include these states
states_abolished_parole <- state_notes |>
  filter(abolished_parole == "Y") |>
  select(state)

#------------------------------------------------------------------------------#
# Projections
#------------------------------------------------------------------------------#

# Load the dataset created by Seba Guzman that contains information on state-year exclusions
# 'excl_state_year = 1' indicates that the state for that given year should be excluded from analysis
projections_compl_2010_2020 <-
  read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_compl_2010_2020.dta"))

# Determine the most reliable year to use (2018 or 2019) for each state
# If both years are marked for exclusion, assign NA to indicate neither year should be used
# If one year is excluded, use the other
# If neither year is excluded, default to using 2019
# Reasoning: We decided 2019 data is the most reliable available data. Most recent 2020 data was during COVID.
which_overall_year <- projections_compl_2010_2020 |>
  select(state, year, excl_state_year) |> # Keep relevant columns for the analysis
  group_by(state) |> # Group data by state for processing state-specific rules
  mutate(year_to_use = case_when(
    state == "Michigan" ~ 2017, # Special rule: Use 2017 for Michigan (Seba Guzman decision)
    excl_state_year[year == 2018] == 1 & excl_state_year[year == 2019] == 1 ~ NA_integer_, # Exclude if both years are unreliable
    excl_state_year[year == 2018] == 1 ~ 2019, # Use 2019 if 2018 is excluded
    excl_state_year[year == 2019] == 1 ~ 2018, # Use 2018 if 2019 is excluded
    excl_state_year[year == 2018] == 0 & excl_state_year[year == 2019] == 0 ~ 2019 # Default to 2019 if both years are valid
  )) |>
  filter(!is.na(year_to_use)) |> # Remove states where neither year can be used
  select(state, year_to_use) |> # Keep only the state and the determined year
  distinct() # Remove duplicate rows to ensure unique state-year pairs

# Identify states with high missingness or unreliable data, defined as having excl_state_year = 1 for both 2018 and 2019
states_with_high_missing <- projections_compl_2010_2020 |>
  filter(year %in% c(2018, 2019)) |> # Consider only the years 2018 and 2019
  group_by(state) |>
  summarise(all_years_missing = all(excl_state_year == 1)) |> # Check if all years in the group are marked as excluded
  filter(all_years_missing) |> # Keep only states where both years are excluded
  select(state) |>
  ungroup()

# Final list of states to exclude from the tool
# Combine the list of states with high missingness and states that have abolished parole
# These states will not be included in state-specific reports or National Snapshot page
states_to_exclude <- states_with_high_missing |>
  bind_rows(states_abolished_parole) |> # Combine with another dataset containing states that abolished parole
  distinct() |>  # Ensure unique entries after combining the data
  filter(state != "Michigan") # Make sure Michigan is included


#------------------------------------------------------------------------------#
# Parole Eligibility Data:
# Missing parole eligibility information was imputed by Seba Guzman in Stata.
# Projections for 2021 to 2023 were also created in Stata.
# Seba Guzman's Imputed Data for NCRP 2010 to 2020
# Seba Guzman's NCRP Projections for 2021 to 2023
#------------------------------------------------------------------------------#

# Import 2023 projections created by Seba Guzman in Stata
# These numbers will be used in the National Snapshot page
# Will only be using 2023 numbers, does not have all years from 2010 to 2023
projections_key_years_2010_2020 <- read_excel(file.path(sp_data_path,"data/analysis/ncrp_results/projections_key_years_2010_2020.xlsx")) |>
  rename(state = State) |>
  mutate(proj_pcnt_ppey    = `Projected percent past PEY (excl. life and >1 yr. sentences)`,
         proj_pop_past_pey = `Projected pop. past PEY (filtered)`)

# Import parole eligibility projections created by Seba Guzman in Stata
# Has all years 2010 to 2023 for states
projections_short_2010_2020 <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_short_2010_2020.dta"))

# Import projections of prison populations created by Seba Guzman in Stata
# These projections include imputed data for jurisdictional and custodial prison population totals.
# When 2023 data is unavailable, use 2022 data.
# The variable 'total_prison_population' prioritizes 'jurtott_incl_und' (jurisdictional total) when available.
# If 'jurtott_incl_und' is missing, it defaults to 'custott_incl_und' (custodial total).
projections_compl_2010_2020 <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_compl_2010_2020.dta")) |>
  select(state, year, jurtott_incl_und, custott_incl_und) |>
  group_by(state) |>
  arrange(state, year) |>  # Ensure data is sorted by state and year for lag calculations
  # Handle missing data for the year 2023 by substituting values with those from 2022
  mutate(
    jurtott_incl_und1 = if_else(
      year == 2023 & is.na(jurtott_incl_und),
      lag(jurtott_incl_und, default = NA, order_by = year),
      jurtott_incl_und
    ),
    custott_incl_und1 = if_else(
      year == 2023 & is.na(custott_incl_und),
      lag(custott_incl_und, default = NA, order_by = year),
      custott_incl_und
    )
  ) |>
  ungroup() |>
  # Determine total prison population by prioritizing jurisdictional total over custodial total
  mutate(total_prison_population = case_when(
    is.na(jurtott_incl_und1) ~ custott_incl_und1,
    TRUE ~ jurtott_incl_und1
  ))

# Identify NCRP release and year-end population files with imputed data
# These files were created by Seba Guzman in Stata using original NCRP data and imputation methods.
release_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                            pattern = "ncrp_releases_\\d{4}_clean_w_imputation.dta", full.names = TRUE)
yearendpop_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                               pattern = "ncrp_yearendpop_\\d{4}_clean_w_imputation.dta", full.names = TRUE)

# Combine individual year files into a single dataset for both releases and year-end populations
ncrp_releases_combined   <- fnc_combine_files(release_files)
ncrp_yearendpop_combined <- fnc_combine_files(yearendpop_files)

# Identify consolidated NCRP files for releases and year-end populations
# These files include terms data and additional imputed values for parole eligibility and sentence lengths.
# For the most part, the tool primarily uses these files as there is more complete information by combining all NCRP data
release_consolidated_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                                         pattern = "ncrp_releases_\\d{4}_clean_w_imputation_consolidated.dta", full.names = TRUE)
yearendpop_consolidated_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                                            pattern = "ncrp_yearendpop_\\d{4}_clean_w_imputation_consolidated.dta", full.names = TRUE)

# Combine individual year files into a single dataset for consolidated releases and year-end populations
ncrp_releases_consolidated_combined   <- fnc_combine_files(release_consolidated_files)
ncrp_yearendpop_consolidated_combined <- fnc_combine_files(yearendpop_consolidated_files)

# Transform the data to align with state-specific rules for parole eligibility
# This step standardizes the data format for releases and year-end populations based on state-specific earliest parole eligibility dates.
# Final data sets are either consolidated or not consildated. Consolidated are used the most throughout the tool
ncrp_releases_not_consolidated   <- fnc_transform_ncrp_data(ncrp_releases_combined, states_earliest_pe)
ncrp_releases_consolidated       <- fnc_transform_ncrp_data(ncrp_releases_consolidated_combined, states_earliest_pe)
ncrp_yearendpop_not_consolidated <- fnc_transform_ncrp_data(ncrp_yearendpop_combined, states_earliest_pe)
ncrp_yearendpop_consolidated     <- fnc_transform_ncrp_data(ncrp_yearendpop_consolidated_combined, states_earliest_pe)

# Calculate time served for non-consolidated release data
# Adds a new variable `time_between_admission_release` by calculating the difference between the release year and admission year
# WARNING MESSAGE OK: Changes "NA" to actual NA
ncrp_releases_not_consolidated <- ncrp_releases_not_consolidated |>
  mutate(
    relyr = as.numeric(relyr), # Convert release year to numeric
    time_between_admission_release = as.numeric(relyr) - as.numeric(admityr) # Calculate time served
  )

# Adjust consolidated release data
# Replaces the 'relyr' variable with Seba Guzman's calculated 'releaseyr' and recalculates time served
# WARNING MESSAGE OK: Changes "NA" to actual NA
ncrp_releases_consolidated <- ncrp_releases_consolidated |>
  mutate(
    relyr = na_if(relyr, ""), # Replace empty strings with NA
    relyr = if_else(
      !is.na(relyr) & !is.na(as.numeric(relyr)), # Ensure relyr is numeric and not NA
      as.numeric(relyr),
      as.numeric(releaseyr) # Use releaseyr if relyr is NA or non-numeric
    ),
    time_between_admission_release = relyr - as.numeric(admityr) # Calculate time served
  ) |>
  select(-releaseyr)

# Identify states with high missingness in race and ethnicity data
# Focus on consolidated year-end population data for the years 2018 and 2019
# Define high missingness as more than 50% of individuals with "Unknown" or missing race
states_with_high_missing_race <- ncrp_yearendpop_consolidated |> # Use year-end population consolidated data
  group_by(state, rptyear) |>
  summarize(
    perc_missing_race = round(mean(race == "Unknown" | is.na(race)) * 100, 1), # Calculate percentage of missing race data
    .groups = "drop" # Avoid grouped output
  ) |>
  filter(rptyear %in% c(2018, 2019)) |> # Focus on specific years
  group_by(state) |> # Group by state to summarize across years
  summarise(all_years_missing = all(perc_missing_race > 50)) |> # Flag states where all years have >50% missing race
  filter(all_years_missing) |> # Retain only states with consistently high missingness
  select(state) |> # Keep only the state column
  ungroup() # Remove grouping



#------------------------------------------------------------------------------#
# BJS Prison Population by Year
# We use prison populations from BJS since these numbers are closer to the
# numbers expected by states. NCRP data is not as reliable.
#------------------------------------------------------------------------------#

# Get BJS Prisoners Series data from 2010 to 2022
# Define a list of filenames for different years along with the specific column needed for the data.
# The 'col' value in each list corresponds to the column that holds the total population in the CSV file.
file_info <- list(
  "2010" = list(file = "p10/p10at01.csv", col = "x_3"),                # Prisoners under the jurisdiction
  "2011" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_1"),  # Total state and federal prisoners
  "2012" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_5"),  # Total state and federal prisoners
  "2013" = list(file = "p13/p13t02.csv", col = "x_5"),                 # Prisoners under the jurisdiction
  "2014" = list(file = "p14/CSV tables/p14t02.csv", col = "x_5"),      # Prisoners under the jurisdiction
  "2015" = list(file = "p15/p15t02.csv", col = "x_6"),                 # Prisoners under the jurisdiction
  "2016" = list(file = "p16/p16t02.csv", col = "x_5"),                 # Prisoners under the jurisdiction
  "2017" = list(file = "p17/p17t02.csv", col = "x_5"),                 # Prisoners under the jurisdiction
  "2018" = list(file = "p18/p18t02.csv", col = "x_5"),                 # Prisoners under the jurisdiction
  "2019" = list(file = "p19/p19t02.csv", col = "x_5"),                 # Prisoners under the jurisdiction
  "2020" = list(file = "p20st/p20stt02.csv", col = "x_4"),             # Prisoners under the jurisdiction
  "2021" = list(file = "p21st/p21stt02.csv", col = "x_4"),             # Prisoners under the jurisdiction
  "2022" = list(file = "p22st/p22stt02.csv", col = "x_1")              # Prisoners under the jurisdiction
)

# Initialize an empty list to store the cleaned data for each year.
cleaned_data_list <- list()

# Loop through the years defined in file_info to read, process, and clean the data.
for (year in names(file_info)) {
  # Construct the file path and retrieve the column name for the specified year.
  file_path <- paste0(sp_data_path, "/data/raw/BJS Prison Pop/", file_info[[year]]$file)
  col_name <- file_info[[year]]$col

  # Read the CSV file, clean the column names, and select relevant columns (state and prison population).
  df <- read.csv(file_path) |>
    clean_names() |>
    select(state = x, bjs_prison_population = !!sym(col_name)) |>
    fnc_clean_bjs_data() |>
    mutate(rptyear = as.numeric(year))

  # Append the cleaned data to the list
  cleaned_data_list[[year]] <- df
}

# Combine all the cleaned datasets from different years into a single dataframe.
bjs_prison_pop_by_rptyear <- do.call(rbind, cleaned_data_list)
bjs_prison_pop_by_rptyear <- bjs_prison_pop_by_rptyear |>
  # Merge information about which year to use for each state
  left_join(which_overall_year, by = "state")
  # States with NA for year_to_use abolished parole or will not be included due to high missingness



#------------------------------------------------------------------------------#
# BJS Prison Population by Race, Ethnicity, and Sex
# We use prison populations from BJS since these numbers are closer to the
# numbers expected by states. NCRP data is not as reliable.
#------------------------------------------------------------------------------#

# Load BJS prison population by race, skip 10 rows, and clean state names
bjs_prison_pop_by_race_state_2018 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p18/p18at02.csv", 10, "jurisdiction")
bjs_prison_pop_by_race_state_2019 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p19/p19at02.csv", 10)
bjs_prison_pop_by_race_state_2020 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p20st/p20stat02.csv", 10, "jurisdiction")
bjs_prison_pop_by_race_state_2021 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p21st/p21stat01.csv", 10, "jurisdiction")
bjs_prison_pop_by_race_state_2022 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p22st/p22stat01.csv", 10, "jurisdiction")

# Select total populations by state and year
total_bjs_pop_2018 <- bjs_prison_pop_by_race_state_2018 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))
total_bjs_pop_2019 <- bjs_prison_pop_by_race_state_2019 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))
total_bjs_pop_2020 <- bjs_prison_pop_by_race_state_2020 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))
total_bjs_pop_2021 <- bjs_prison_pop_by_race_state_2021 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))
total_bjs_pop_2022 <- bjs_prison_pop_by_race_state_2022 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))

# Rename race and ethnicity labels
# Count number and proportion of people in prison by race and ethnicity
# WARNING MESSAGE OK: Changes "NA" to actual NA
bjs_prison_pop_by_race_2018 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2018, total_bjs_pop_2018) |> mutate(rptyear = 2018)
bjs_prison_pop_by_race_2019 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2019, total_bjs_pop_2019) |> mutate(rptyear = 2019)
bjs_prison_pop_by_race_2020 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2020, total_bjs_pop_2020) |> mutate(rptyear = 2020)
bjs_prison_pop_by_race_2021 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2021, total_bjs_pop_2021) |> mutate(rptyear = 2021)
bjs_prison_pop_by_race_2022 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2022, total_bjs_pop_2022) |> mutate(rptyear = 2022)

# Combine all years into a single dataset
bjs_prison_pop_by_race <- rbind(
  bjs_prison_pop_by_race_2018,
  bjs_prison_pop_by_race_2019,
  bjs_prison_pop_by_race_2020,
  bjs_prison_pop_by_race_2021,
  bjs_prison_pop_by_race_2022
)

# Filter data to most recent data available for now
bjs_prison_pop_by_race <- bjs_prison_pop_by_race |>
  filter(rptyear == 2022)

# Keep code - Can check column numbers for male and female by year
# test_2018 <- read.csv(file.path(sp_data_path, "data/raw/BJS Prison Pop/p19/p19t02.csv"))
# test_2019 <- read.csv(file.path(sp_data_path, "data/raw/BJS Prison Pop/p20st/p20stt02.csv"))
# test_2020 <- read.csv(file.path(sp_data_path, "data/raw/BJS Prison Pop/p20st/p20stt02.csv"))
# test_2021 <- read.csv(file.path(sp_data_path, "data/raw/BJS Prison Pop/p22st/p22stt02.csv"))
# test_2022 <- read.csv(file.path(sp_data_path, "data/raw/BJS Prison Pop/p22st/p22stt02.csv"))

# Load BJS prison population by sex, skip 10 rows, clean state names
# Select male and female columns and assign year variable
bjs_prison_pop_by_sex_2018 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p19/p19t02.csv", 10, "x_2", "x_3", 2018)
bjs_prison_pop_by_sex_2019 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p20st/p20stt02.csv", 10, "x_2", "x_3", 2019)
bjs_prison_pop_by_sex_2020 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p20st/p20stt02.csv", 10, "x_5", "x_6", 2020)
bjs_prison_pop_by_sex_2021 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p22st/p22stt02.csv", 10, "x_2", "x_3", 2021)
bjs_prison_pop_by_sex_2022 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p22st/p22stt02.csv", 10, "x_6", "x_7", 2022)

# Combine data
bjs_prison_pop_by_sex <- rbind(
  bjs_prison_pop_by_sex_2018,
  bjs_prison_pop_by_sex_2019,
  bjs_prison_pop_by_sex_2020,
  bjs_prison_pop_by_sex_2021,
  bjs_prison_pop_by_sex_2022
)

# Filter data to most recent data available for now
bjs_prison_pop_by_sex <- bjs_prison_pop_by_sex |>
  filter(rptyear == 2022)

#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  projections_key_years_2010_2020  = "projections_key_years_2010_2020.rds",
  projections_compl_2010_2020      = "projections_compl_2010_2020.rds",
  projections_short_2010_2020      = "projections_short_2010_2020.rds",
  ncrp_releases_not_consolidated   = "ncrp_releases_not_consolidated.rds",
  ncrp_releases_consolidated       = "ncrp_releases_consolidated.rds",
  ncrp_yearendpop_consolidated     = "ncrp_yearendpop_consolidated.rds",
  ncrp_yearendpop_not_consolidated = "ncrp_yearendpop_not_consolidated.rds",

  bjs_prison_pop_by_race           = "bjs_prison_pop_by_race.rds",
  bjs_prison_pop_by_sex            = "bjs_prison_pop_by_sex.rds",
  bjs_prison_pop_by_rptyear        = "bjs_prison_pop_by_rptyear.rds",

  hex_gj                           = "hex_gj.rds",
  states_abolished_parole          = "states_abolished_parole.rds",
  state_notes                      = "state_notes.rds",
  states_to_exclude                = "states_to_exclude.rds",
  states_nofilter                  = "states_nofilter.rds",
  states_undercounted              = "states_undercounted.rds",
  states_with_high_missing         = "states_with_high_missing.rds",
  states_with_high_missing_race    = "states_with_high_missing_race.rds",
  states_national_page_only        = "states_national_page_only.rds",
  states_use_other_race_eth        = "states_use_other_race_eth.rds",
  which_overall_year               = "which_overall_year.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

# # ARCHIVE CODE BUT KEEP FOR NOW FOR EASY DATA LOADING
# load(file = paste0(sp_data_path, "/data/analysis/app/projections_key_years_2010_2020.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/projections_compl_2010_2020.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/projections_short_2010_2020.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_not_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_not_consolidated.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_yearendpop_combined.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_releases_combined.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_race.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_sex.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/bjs_prison_pop_by_rptyear.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/hex_gj.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_abolished_parole.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/state_notes.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_to_exclude.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_nofilter.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_undercounted.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_with_high_missing_race.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_national_page_only.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/states_use_other_race_eth.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/which_overall_year.rds"))
