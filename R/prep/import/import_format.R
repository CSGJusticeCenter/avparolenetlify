#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: November 18, 2024 (MAR)
# Description:
#    This script:
#######################################

#------------------------------------------------------------------------------#
# MAP - National Snapshot Page
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
# State-Specific Notes for "How is Parole Eligibility Determined?" and
# Methodology of Imputation for "State Notes" Section of Parole Eligibility Tab
#------------------------------------------------------------------------------#

# Import state-specific imputation methodology
# Created by Seba Guzman (CSG Research) in Stata
state_methodology <- read_dta(file.path(sp_data_path, "data/analysis/ncrp_results/state_notes_2020.dta"))

# Import "How is Parole Eligibility Determined?" and number of parole board members
# These were created by Policy Team in 'Release Systems by State' Word Document
state_notes_raw <- read.csv(file.path(sp_data_path, "data/raw/Carl State Notes/av_parole_state_notes_v2.csv")) |>
  clean_names() |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(
    state = str_replace_all(state, "\\*", ""),
    citation = sapply(citation, fnc_format_citation)) # format citations

# Combine state notes and state-specific imputation methodology
# Adjust formatting - superscript 1 is for "How Parole Eligibility is Determined" so these need
# to be increased by 1 because this text is near the bottom of the Parole Eligibility tab
# NEED TO UPDATE THIS WHEN SEBA COMPLETES#########################
state_notes <- state_notes_raw |>
  left_join(state_methodology, by = "state") |>
  # add period to matching note.
  mutate(matching_note = paste0(matching_note, "."),
         # add superscript 1 to release systems
         release_systems = paste0(release_systems, "\u00B9"),
         citation        = paste("\u00B9", citation, sep = " "),
         # Increase superscripts to account for 1^ above
         # Superscript 1: \u00B9
         # Superscript 2: \u00B2
         # Superscript 3: \u00B3
         # Superscript 4: \u2074
         # Superscript 5: \u2075
         # Superscript 6: \u2076
         estimation_note = gsub("\u00B9", "\u00B2", estimation_note),
         rules_note      = gsub("\u00B2", "\u00B3", rules_note),
         projection_note = gsub("\u00B3", "\u2074", projection_note),

         source_note1    = gsub("\u00B9", "\u00B2", source_note1),
         source_note2    = gsub("\u00B2", "\u00B3", source_note2),
         source_note3    = gsub("\u00B3", "\u2074", source_note3),
         # combine methodology info and citations
         methodology_notes = paste(estimation_note, matching_note, rules_note,
                                   last_year_note, year_excluded_note, projection_note, sep = "<br><br>"),
         citation = paste(citation, source_note1, source_note2, source_note3, sep = "<br><br>"),
         methodology_notes = gsub("<br><br><br>", "<br>", methodology_notes),
         citation = gsub("<br><br><br>", "<br><br>", citation)) |>
  filter(!(state == "Louisiana" & row_number() == which(state == "Louisiana")[2]))

#------------------------------------------------------------------------------#
# States Rules
#------------------------------------------------------------------------------#

# Import state rules:
# States to exclude overall, states that don't need the parole eligibility filtered by admission type and sentence length,
# states that should be interpreted with caution, states to exclude in projections
state_rules_v1 <- read_excel(file.path(sp_data_path, "data/raw/NCRP Data Rules/state_rules_v1.xlsx"))

# Define states that won't need to filter by admission type and sentence length
# These states have high missingness for these variables but their data can be used
# without filtering because the estimates for people past parole eligibility are reliable enough
# or the data submitted is close enough to what the expected filtered population would be
states_nofilter <- state_rules_v1 |>
  filter(dont_filter_admtype_sentlength == 1) #############################################################change to pull

# States where we should use the earliest parole eligibility year bc it is the more reliable
# than the imputations
states_earliest_pe <- state_rules_v1 |>
  filter(use_earliest_pey1 == 1)

# States where the people past PEY is likely undercounted
states_undercounted <- state_rules_v1 |>
  filter(likely_undercount == 1)

# States that should be on the national snapshot page but not the state reports
states_national_page_only <- state_rules_v1 |>
  filter(exclude_from_reports == 1) |>
  select(state)

# States that have abolished parole
states_abolished_parole <- state_notes |>
  filter(abolished_parole == "Y") |>
  select(state)

# States where "Other race(s), non-Hispanic" should be included due to population
# sizes in these states (Hawaii, Alaska, New Mexico, Oklahoma)
states_use_other_race_eth <- state_rules_v1 |>
  filter(use_other_race_ethnicity == 1)

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

