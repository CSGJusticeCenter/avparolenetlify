################################################################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: November 20, 2024 (MAR)
# Description:
#    This script handles the import, transformation, and integration of multiple
#    datasets for the AV Parole project. It includes:
#    - Importing and processing hex map shapefiles for visualizations.
#    - Loading and combining NCRP data files, including projections and imputations
#      for parole eligibility and prison populations.
#    - Integrating state-specific notes and rules for data exclusions, missingness,
#      and projections.
#    - Importing and processing Bureau of Justice Statistics (BJS) data for prison
#      populations by year, race, ethnicity, and sex.
#    - Applying state-level rules and exclusions to prepare clean datasets for
#      national and state-specific reports.
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
#------------------------------------------------------------------------------#

# Import rules for handling specific states in the analysis of parole eligibility.
# The rules are stored in an Excel file and include:
# - States to exclude overall
# - States that do not require filtering by admission type and sentence length
# - States to interpret with caution due to data quality concerns
# - States to exclude from projections
state_rules_v1 <- read_excel(file.path(sp_data_path, "data/raw/NCRP Data Rules/state_rules_v1.xlsx"))

# Define states that won't need to filter by admission type and sentence length
# These states have high missingness in variables for admission type and sentence length,
# but their estimates for people past parole eligibility are reliable enough for inclusion
# without filtering. Alternatively, the data submitted aligns closely with the expected
# filtered population.
states_nofilter <- state_rules_v1 |>
  filter(dont_filter_admtype_sentlength == 1)

# States where we should use the earliest parole eligibility year
# In these states, the earliest parole eligibility year (PEY1) is more reliable
# than imputed values for parole eligibility.
states_earliest_pe <- state_rules_v1 |>
  filter(use_earliest_pey1 == 1)

# For these states, the analysis indicates that the number of people identified
# as past parole eligibility is likely undercounted due to limitations in the data.
states_undercounted <- state_rules_v1 |>
  filter(likely_undercount == 1)

# These states should appear on the national snapshot page but are excluded
# from the state-specific reports.
states_national_page_only <- state_rules_v1 |>
  filter(exclude_from_reports == 1) |>
  select(state)

# Identify states where parole has been abolished.
# These states are excluded from specific analyses related to parole eligibility.
states_abolished_parole <- state_notes |>
  filter(abolished_parole == "Y") |>
  select(state)

# For specific states (e.g., Hawaii, Alaska, New Mexico, Oklahoma), the population
# sizes of "Other race(s), non-Hispanic" are significant enough to warrant inclusion
# in the analysis.
states_use_other_race_eth <- state_rules_v1 |>
  filter(use_other_race_ethnicity == 1)


#------------------------------------------------------------------------------#
# State-Specific Notes for "How is Parole Eligibility Determined?" and
# Methodology of Imputation for "Estimation Methodology" Section of Parole Eligibility Tab
#------------------------------------------------------------------------------#

# Import state-specific imputation methodology
# The file was created by Seba Guzman (CSG Research) in Stata and contains
# methodology details for imputing state-level parole eligibility information
state_methodology <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/state_notes_2020.dta"))

# Convert all entries in columns starting with "source_note" to NA
# if they contain only "³". This helps filter out irrelevant or placeholder notes.
state_methodology_clean <- state_methodology %>%
  mutate(across(starts_with("source_note"), str_trim)) |>
  mutate(across(starts_with("source_note"),
                ~ if_else(. == "³", NA_character_, .)))

#------------------------------------------------------------------------------#
# "How is Parole Eligibility Determined?"
#------------------------------------------------------------------------------#

# Import "How is Parole Eligibility Determined?" and number of parole board members
# Load data created by the Policy Team in the 'Release Systems by State' Word document.
# The dataset includes state-specific notes on parole eligibility determination
# and the number of parole board members.
state_notes_raw <- read.csv(file.path(sp_data_path, "data/raw/Carl State Notes/av_parole_state_notes_v2.csv")) |>
  clean_names() |>
  mutate(across(where(is.character), str_trim)) |> # Trim leading/trailing whitespace
  mutate(
    state = str_replace_all(state, "\\*", ""),      # Remove asterisks from state names
    citation = sapply(citation, fnc_format_citation) # Format citations for proper display
  )

#------------------------------------------------------------------------------#
# Combine Notes and Adjust Formatting
#------------------------------------------------------------------------------#

# Merge state notes from the Policy Team with imputation methodology from Seba Guzman.
# Adjust formatting to ensure consistency with the Parole Eligibility tab display.
# Note on Superscripts:
# Superscripts are used to provide references and citations throughout the Parole
# Eligibility tab. Each type of note is assigned a unique superscript number:
# - Superscript 1 (¹) is used for notes related to "How Parole Eligibility is Determined".
# - Superscript numbers are incremented for methodology-related notes and citations:
#   - ¹ becomes ², ² becomes ³, ³ becomes ⁴, and so on.
# These adjustments ensure clarity and proper alignment with the visual layout of
# the tab. Superscripts are encoded as Unicode characters for compatibility:
# - \u00B9 (¹), \u00B2 (²), \u00B3 (³), \u2074 (⁴), \u2075 (⁵), etc.
state_notes <- state_notes_raw |>
  left_join(state_methodology_clean, by = "state") |>

  # Ensure proper formatting for notes:
  mutate(
    # Append a period to `matching_note` if missing
    matching_note = paste0(matching_note, "."),

    # Add superscript 1 to `release_systems` for "How Parole Eligibility is Determined"
    release_systems = paste0(release_systems, "\u00B9"),

    # Add superscript 1 to `citation` for consistency
    citation = paste("\u00B9", citation, sep = " "),

    # Increment superscripts for notes by 1 to align with numbering conventions:
    # Superscript 1 (\u00B9) becomes 2 (\u00B2), 2 becomes 3 (\u00B3), and so on.
    estimation_note = gsub("\u00B9", "\u00B2", estimation_note),
    rules_note = gsub("\u00B2", "\u00B3", rules_note),
    projection_note = gsub("\u00B3", "\u2074", projection_note),

    # Apply the same logic to source note columns
    source_note1 = gsub("\u00B9", "\u00B2", source_note1),
    source_note2 = gsub("\u00B2", "\u00B3", source_note2),
    source_note3 = gsub("\u00B3", "\u2074", source_note3),

    # Combine all imputation methodology details into a single HTML-compatible string
    methodology_notes = paste(estimation_note, matching_note, rules_note,
                              last_year_note, year_excluded_note, projection_note,
                              sep = "<br><br>"),

    # Combine all citations into a single field for display
    citation = paste(citation, source_note1, source_note2, source_note3, sep = "<br><br>"),

    # Remove unnecessary blank lines from the formatted output
    methodology_notes = gsub("<br><br><br>", "<br>", methodology_notes),
    citation = gsub("<br><br><br>", "<br><br>", citation)############################# Will need to format when Seba provides updated data (11/20/24)
  ) |>

  # Remove the second duplicate entry for Louisiana (if it exists)
  filter(!(state == "Louisiana" & row_number() == which(state == "Louisiana")[2]))


#------------------------------------------------------------------------------#
# Projections
#------------------------------------------------------------------------------#

# Determine which states have high missingness and should be excluded from analysis/tool
# File created by Seba Guzman in Stata
# excl_state_year = 1 means the state for that year should be excluded
projections_compl_2010_2020 <-
  read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_compl_2010_2020.dta"))

# Determine which year is best by state
# Some should use 2019 and others should use 2018
# Some should use neither
which_overall_year <- projections_compl_2010_2020 |>
  select(state, year, excl_state_year) |>
  group_by(state) |>
  mutate(year_to_use = case_when(
    excl_state_year[year == 2018] == 1 & excl_state_year[year == 2019] == 1 ~ NA_integer_,
    excl_state_year[year == 2018] == 1 ~ 2019,
    excl_state_year[year == 2019] == 1 ~ 2018,
    excl_state_year[year == 2018] == 0 & excl_state_year[year == 2019] == 0 ~ 2019
  )) |>
  filter(!is.na(year_to_use)) |>
  select(state, year_to_use) |> distinct()

# Determine which years shouldn't be used by state due to unreliable data
which_years <- projections_compl_2010_2020 |>
  select(state, year, excl_state_year) |>
  distinct()

# Filter states with excl_state_year == 1 for both 2018 and 2019
states_with_high_missing <- projections_compl_2010_2020 |>
  filter(year %in% c(2018, 2019)) |>
  group_by(state) |>
  summarise(all_years_missing = all(excl_state_year == 1)) |>
  filter(all_years_missing) |>
  select(state) |>
  ungroup()

# Combine both dataframes of states to exclude: states with high missingness
# and states that abolished parole
# These states will not need state reports
states_to_exclude <- states_with_high_missing |>
  bind_rows(states_abolished_parole) |>
  distinct()






#------------------------------------------------------------------------------#
# Parole Eligibility Data:
# Seba Guzman's NCRP Projections for 2021 to 2023
# Seba Guzman's Imputed Data for NCRP 2010 to 2020
#------------------------------------------------------------------------------#

# Import parole eligibility projections created by Seba Guzman in Stata
ncrp_projections <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_short_2010_2020.dta"))

# Import projections of prison populations created by Seba Guzman in Stata
# The variable 'total_prison_population' represents the total number of individuals in prison.
# It prioritizes the value from 'jurtott_incl_und' (jurisdictional total, including unclassified categories)
# when available. If 'jurtott_incl_und' is missing, it defaults to 'custott_incl_und'
# (custodial total, including unclassified categories).
ncrp_population_projections <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/projections_compl_2010_2020.dta")) |>
  select(state, year, jurtott_incl_und, custott_incl_und) |>
  group_by(state) |>
  arrange(state, year) |>  # Ensure data is sorted by state and year
  # Checking if the data for 2023 is missing and substituting it with the data from 2022
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
  mutate(total_prison_population = case_when(is.na(jurtott_incl_und1) ~ custott_incl_und1,
                                             TRUE ~ jurtott_incl_und1))

# These are the NCRP files that were created by Sebastian (CSG Research) in Stata
# Original NCRP releases and yearendpop files were used to create imputations for missing data regarding
# parole eligibility and sentence lengths
release_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                            pattern = "ncrp_releases_\\d{4}_clean_w_imputation.dta", full.names = TRUE)
yearendpop_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                               pattern = "ncrp_yearendpop_\\d{4}_clean_w_imputation.dta", full.names = TRUE)

# Combine files
ncrp_releases_combined   <- fnc_combine_files(release_files)
ncrp_yearendpop_combined <- fnc_combine_files(yearendpop_files)

# These are the NCRP files that were created by Sebastian (CSG Research) in Stata
# Original NCRP releases and yearendpop files AND terms files were used to create imputations for missing data regarding
# parole eligibility and sentence lengths.
release_consolidated_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                                         pattern = "ncrp_releases_\\d{4}_clean_w_imputation_consolidated.dta", full.names = TRUE)
yearendpop_consolidated_files <- list.files(path = file.path(sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                                            pattern = "ncrp_yearendpop_\\d{4}_clean_w_imputation_consolidated.dta", full.names = TRUE)

# Combine files
ncrp_releases_consolidated_combined   <- fnc_combine_files(release_consolidated_files)
ncrp_yearendpop_consolidated_combined <- fnc_combine_files(yearendpop_consolidated_files)

# Transform the data for releases and year-end population
ncrp_releases_not_consolidated   <- fnc_transform_ncrp_data(ncrp_releases_combined, states_earliest_pe)
ncrp_releases_consolidated       <- fnc_transform_ncrp_data(ncrp_releases_consolidated_combined, states_earliest_pe)
ncrp_yearendpop_not_consolidated <- fnc_transform_ncrp_data(ncrp_yearendpop_combined, states_earliest_pe)
ncrp_yearendpop_consolidated     <- fnc_transform_ncrp_data(ncrp_yearendpop_consolidated_combined, states_earliest_pe)

# Calculate time served for both releases files
# Warning message OK - replacing "NA" with actual NA
ncrp_releases_not_consolidated <- ncrp_releases_not_consolidated |>
  mutate(relyr = as.numeric(relyr),
         time_between_admission_release = as.numeric(relyr) - as.numeric(admityr))
ncrp_releases_consolidated <- ncrp_releases_consolidated |>
  select(-relyr) |> # remove NCRP release year and use Seba Guzman's release year
  rename(relyr = releaseyr) |>
  mutate(relyr = as.numeric(relyr),
         time_between_admission_release = as.numeric(relyr) - as.numeric(admityr))

# States with high missingness for race and ethnicity
states_with_high_missing_race <- ncrp_yearendpop_consolidated |>######################## need for releases too?
  group_by(state, rptyear) |>
  summarize(
    perc_missing_race = round(mean(race == "Unknown" | is.na(race)) * 100, 1),
    .groups = "drop") |>
  filter(rptyear %in% c(2018, 2019)) |>
  group_by(state) |>
  summarise(all_years_missing = all(perc_missing_race > 50)) |>
  filter(all_years_missing) |>
  select(state) |>
  ungroup()



#------------------------------------------------------------------------------#
# BJS Prison Population by Year
#------------------------------------------------------------------------------#

# Get BJS Prisoners Series data from 2010 to 2022
# Define a list of filenames for different years along with the specific column needed for the data.
# The 'col' value in each list corresponds to the column that holds the relevant data in the CSV file.
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
  left_join(which_overall_year, by = "state")



#------------------------------------------------------------------------------#
# BJS Prison Population by Race, Ethnicity, and Sex
# 2018 and 2019
#------------------------------------------------------------------------------#

# Load and clean data
bjs_prison_pop_by_race_state_2018 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p18/p18at02.csv", 10, "jurisdiction")
bjs_prison_pop_by_race_state_2019 <- fnc_load_raceeth_data("data/raw/BJS Prison Pop/p19/p19at02.csv", 10)

# Select total populations by state
total_bjs_pop_2019 <- bjs_prison_pop_by_race_state_2019 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))
total_bjs_pop_2018 <- bjs_prison_pop_by_race_state_2018 |>
  select(state, total) |>
  mutate(total = as.numeric(str_replace_all(total, ",", "")))

# Process race and ethnicity data
# Warning messages ok, changes "NA" to actual NA
bjs_prison_pop_by_race_2018 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2018, total_bjs_pop_2019) |> mutate(rptyear = 2018)
bjs_prison_pop_by_race_2019 <- fnc_process_bjs_raceeth_data(bjs_prison_pop_by_race_state_2019, total_bjs_pop_2019) |> mutate(rptyear = 2019)
bjs_prison_pop_by_race <- rbind(bjs_prison_pop_by_race_2018, bjs_prison_pop_by_race_2019)


# Import BJS data by sex
bjs_prison_pop_by_sex_2019_raw <- read.csv(file.path(sp_data_path,
                                                     "data/raw/BJS Prison Pop/p19/p19t02.csv"))
bjs_prison_pop_by_sex_2019_raw <- bjs_prison_pop_by_sex_2019_raw[-(1:10), ]

# Process data for 2018 and 2019
bjs_prison_pop_by_sex_2019 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p19/p19t02.csv", 10, "x_6", "x_7", 2019)
bjs_prison_pop_by_sex_2018 <- fnc_process_bjs_sex_data("data/raw/BJS Prison Pop/p19/p19t02.csv", 10, "x_2", "x_3", 2018)

# Combine data
bjs_prison_pop_by_sex <- bind_rows(bjs_prison_pop_by_sex_2018, bjs_prison_pop_by_sex_2019)


#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  ncrp_projections                 = "ncrp_projections.rds",
  ncrp_population_projections      = "ncrp_population_projections.rds",
  ncrp_releases_not_consolidated   = "ncrp_releases_not_consolidated.rds",
  ncrp_yearendpop_consolidated     = "ncrp_yearendpop_consolidated.rds",
  ncrp_releases_consolidated       = "ncrp_releases_consolidated.rds",
  ncrp_yearendpop_not_consolidated = "ncrp_yearendpop_not_consolidated.rds",
  ncrp_yearendpop_combined         = "ncrp_yearendpop_combined.rds",
  ncrp_releases_combined           = "ncrp_releases_combined.rds",

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
  which_overall_year               = "which_overall_year.rds",
  which_years                      = "which_years.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))


# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_projections.rds"))
# load(file = paste0(sp_data_path, "/data/analysis/app/ncrp_population_projections.rds"))
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
# load(file = paste0(sp_data_path, "/data/analysis/app/which_years.rds"))
