#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: September 24, 2024 (MAR)
# Description:
#    This script is responsible for importing, cleaning, and preparing various datasets
#    for the AV Parole project. It handles the following tasks:
#    1. Import NCRP data (admissions, population, year-end population) and BJS Prisoners data.
#    2. Process NCRP data by merging release and year-end files, calculating metrics,
#       and transforming variables for analysis.
#    3. Clean and structure BJS Prisoners data by race, ethnicity, and sex for different years.
#    4. Import hex map for the National Trends page.
#    5. Load external data from the Robina Institute and Carl Reynold's state notes on parole eligibility (PE).
#    6. Save cleaned and transformed datasets for use in analyses and visualizations.
#######################################

#------------------------------------------------------------------------------#
# MAP - National Trends Page
#------------------------------------------------------------------------------#

# Hex map for national trends page
# Load hexgrid shapefile and select only the 'state_abb' column
# Remove the District of Columbia (DC) and transform the spatial data to EPSG:3857
hex_gj <- read_sf(file.path(config$sp_data_path, "data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
    select(state_abb = iso3166_2) |>
    filter(state_abb != "DC") |>
    st_transform(3857) |>
    sf_geojson() |>
    fromJSON(simplifyVector = FALSE)


#------------------------------------------------------------------------------#
# State-Specific Notes for "How is Parole Eligibility Determined?" Section
# Methodology for Imputation for"End Notes" Section
#------------------------------------------------------------------------------#

# # Load Carl's state notes, which contains parole eligibility information for each state
# carl_state_notes <- read.xlsx(file.path(config$sp_data_path, "data/raw/Carl State Notes/carl_state_notes.xlsx")) |>
#   clean_names()
#
# # Load additional parole information by state from an Excel sheet (includes details such as the number of parole board members)
# parole_info_by_state <- read.xlsx(file.path(config$sp_data_path, "background/app/Parole Info by State.xlsx"),
#                                   sheet = "Overall") |>
#   clean_names()

# Import state-specific notes about parole eligibility and number of parole board members
state_notes_raw <- read.csv(file.path(config$sp_data_path, "data/raw/Carl State Notes/av_parole_state_notes_v1.csv")) |>
  clean_names() |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(
    state = str_replace_all(state, "\\*", ""),
    citation = sapply(citation, fnc_format_citation)) # format citations

# Import state-specific imputation methodology
state_methodology <- read_dta(file.path(config$sp_data_path, "data/analysis/ncrp_results/state_notes_2020.dta"))

# Combine these together
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
         methodology_notes = paste(estimation_note, matching_note, rules_note, projection_note, sep = "<br><br>"),
         citation = paste(citation, source_note1, source_note2, source_note3, sep = "<br><br>"))

#------------------------------------------------------------------------------#
# NCRP
# Seba Guzman's Imputed Data
#------------------------------------------------------------------------------#

# Define file paths for release and year-end population files (update paths if necessary)
release_files <- list.files(path = file.path(config$sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                            pattern = "ncrp_releases_\\d{4}_clean_w_imputation.dta", full.names = TRUE)

yearendpop_files <- list.files(path = file.path(config$sp_data_path, "data/analysis/clean_files/cleaning_processing"),
                               pattern = "ncrp_yearendpop_\\d{4}_clean_w_imputation.dta", full.names = TRUE)


# Read and combine release files
ncrp_releases_combined <- bind_rows(lapply(release_files, fnc_read_and_add_year))

# Read and combine yearendpop files
ncrp_yearendpop_combined <- bind_rows(lapply(yearendpop_files, fnc_read_and_add_year))

# Transform the combined release data
# Calculate PE metrics
# Combine past and current to get all who are currently eligible
# Factor variables
# Warning OK - changes "NA" to actual NA
        # Warning message:
        #   There were 2 warnings in `mutate()`.
        # The first warning was:
        #   ℹ In argument: `time_between_admisson_release = as.numeric(relyr) - as.numeric(admityr)`.
        # Caused by warning:
        #   ! NAs introduced by coercion
ncrp_releases <- ncrp_releases_combined |>
  mutate(offdetail = trimws(offdetail),
         time_between_ped_rptyear = years_to_estimated_pey,
         time_between_admisson_release =  as.numeric(relyr) - as.numeric(admityr),
         time_between_ped_release = as.numeric(relyr) - as.numeric(estimated_pey),
         parelig_status = case_when(estimated_pey_status %in% c("past", "current_year") ~ "Current",
                                    estimated_pey_status == "missing" ~ "Missing",
                                    estimated_pey_status == "future" ~ "Future",
                                    TRUE ~ estimated_pey_status)) |>
  # Replace NAs and "NAs" with "No Data"
  mutate_at(vars(race, agerlse, sex, admtype, sentlgth, offdetail), ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>
  fnc_create_fbi_index() |> # Redo offense types
  fnc_create_admtype() |>   # Redo admission types
  mutate(
    race = factor(race, levels = c("Unknown",
                                   "Other race(s), non-Hispanic",
                                   "White, non-Hispanic",
                                   "Hispanic, any race",
                                   "Black, non-Hispanic")),
    agerlse = factor(agerlse, levels = c("55+ years",
                                         "45-54 years",
                                         "35-44 years",
                                         "25-34 years",
                                         "18-24 years",
                                         "Unknown")),
    sentlgth = factor(sentlgth, levels = c("< 1 year",
                                           "1-1.9 years",
                                           "2-4.9 years",
                                           "5-9.9 years",
                                           "10-24.9 years",
                                           ">=25 years",
                                           "Life, LWOP, Life plus additional years, Death",
                                           "Unknown")))

# Similarly transform the year-end population data
# Calculate PE metrics
# Combine past and current to get all who are currently eligible
# Factor variables
ncrp_yearendpop <- ncrp_yearendpop_combined |>
  mutate(offdetail = trimws(offdetail),
         time_between_ped_rptyear = as.numeric(years_to_estimated_pey),
         parelig_status = case_when(estimated_pey_status %in% c("past", "current_year") ~ "Current",
                                    estimated_pey_status == "missing" ~ "Missing",
                                    estimated_pey_status == "future" ~ "Future",
                                    TRUE ~ estimated_pey_status)) |>
  # Replace NAs and "NAs" with "No Data"
  mutate_at(vars(race, ageyrend, sex, admtype, sentlgth, offdetail), ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>
  fnc_create_fbi_index() |> # Redo offense types
  fnc_create_admtype() |>   # Redo admission types
  mutate(
    race = factor(race,
                  levels = c("Unknown",
                             "Other race(s), non-Hispanic",
                             "White, non-Hispanic",
                             "Hispanic, any race",
                             "Black, non-Hispanic")),
    ageyrend = factor(ageyrend,
                      levels = c("55+ years",
                                 "45-54 years",
                                 "35-44 years",
                                 "25-34 years",
                                 "18-24 years",
                                 "Unknown")),
    sentlgth = factor(sentlgth,
                      levels = c(
                        "< 1 year",
                        "1-1.9 years",
                        "2-4.9 years",
                        "5-9.9 years",
                        "10-24.9 years",
                        ">=25 years",
                        "Life, LWOP, Life plus additional years, Death",
                        "Unknown")))




#------------------------------------------------------------------------------#
# BJS
#------------------------------------------------------------------------------#

# Import BJS prisoners data for 2020 and 2022 (not sure which one will be used yet).
# Skipping the first 10 rows due to headers and metadata.
bjs_prison_pop_by_race_state_2020 <- read.csv(file.path(config$sp_data_path,
                                                        "data/raw/BJS Prison Pop/p20st/p20stat02.csv"),
                                              skip = 10)

bjs_prison_pop_by_race_state_2022 <- read.csv(file.path(config$sp_data_path,
                                                        "data/raw/BJS Prison Pop/p22st/p22stat01.csv"),
                                              skip = 10)


# Define a list of filenames for different years along with the specific column needed for the data.
# The 'col' value in each list corresponds to the column that holds the relevant data in the CSV file.
file_info <- list(
  "2010" = list(file = "p10/p10at01.csv", col = "x_3"),
  "2011" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_1"),
  "2012" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_5"),
  "2013" = list(file = "p13/p13t02.csv", col = "x_5"),
  "2014" = list(file = "p14/CSV tables/p14t02.csv", col = "x_5"),
  "2015" = list(file = "p15/p15t02.csv", col = "x_6"),
  "2016" = list(file = "p16/p16t02.csv", col = "x_5"),
  "2017" = list(file = "p17/p17t02.csv", col = "x_5"),
  "2018" = list(file = "p18/p18t02.csv", col = "x_5"),
  "2019" = list(file = "p19/p19t02.csv", col = "x_5"),
  "2020" = list(file = "p20st/p20stt02.csv", col = "x_4"),
  "2021" = list(file = "p21st/p21stt02.csv", col = "x_4"),
  "2022" = list(file = "p22st/p22stt02.csv", col = "x_1")
)

# Initialize an empty list to store the cleaned data for each year.
cleaned_data_list <- list()

# Loop through the years defined in file_info to read, process, and clean the data.
for (year in names(file_info)) {
  # Construct the file path and retrieve the column name for the specified year.
  file_path <- paste0(config$sp_data_path, "/data/raw/BJS Prison Pop/", file_info[[year]]$file)
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

# Calculate total BJS prison population for 2020. Clean the 'total' column by removing commas.
total_bjs_pop_2020 <- bjs_prison_pop_by_race_state_2020 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(x, total) |>
  rename(state = x) |>
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

# Process BJS population by race and ethnicity for 2020
# Warning OK - characters like '~' turned to NA
bjs_prison_pop_by_race_2020 <- bjs_prison_pop_by_race_state_2020 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(-jurisdiction) |>
  rename(state = x) |>
  mutate(across(everything(), ~str_replace_all(., ",", ""))) |>
  mutate(across(-state, as.numeric)) |>
  # Pivot data from wide to long format to have race as a key variable and corresponding population as value.
  pivot_longer(cols = total:did_not_report,
               names_to = "race",
               values_to = "n") |>
  mutate(race = case_when(
    race == "total" ~ "Total Population",
    race == "white_a" ~ "White, non-Hispanic",
    race == "black_a" ~ "Black, non-Hispanic",
    race == "hispanic" ~ "Hispanic, any race",
    race %in% c("american_indian_alaska_native_a",
                "asian_a",
                "native_hawaiian_other_pacific_islander_a",
                "two_or_more_races_a",
                "other_a") ~ "Other race(s), non-Hispanic",
    race == "unknown" ~ "Unknown",
    race == "did_not_report" ~ "Unknown",
    TRUE ~ race
  )) |>
  filter(race != "Unknown" & race != "Total Population") |>
  group_by(state, race) |>
  summarise(n = sum(n, na.rm = TRUE)) |>
  left_join(total_bjs_pop_2020, by = "state") |>
  ungroup() |>
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") |>
  select(-total)

# Similar to 2020, process total population and population by race and ethnicity for 2022.
total_bjs_pop_2022 <- bjs_prison_pop_by_race_state_2022 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(x, total) |>
  rename(state = x) |>
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

# Pop by Race and Ethnicity
# Warning OK - characters like '~' turned to NA
bjs_prison_pop_by_race_2022 <- bjs_prison_pop_by_race_state_2022 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(-jurisdiction) |>
  rename(state = x) |>
  mutate(across(everything(), ~str_replace_all(., ",", ""))) |>
  mutate(across(-state, as.numeric)) |>
  pivot_longer(cols = total:did_not_report,
               names_to = "race",
               values_to = "n") |>
  mutate(race = case_when(
    race == "total" ~ "Total Population",
    race == "white_a" ~ "White, non-Hispanic",
    race == "black_a" ~ "Black, non-Hispanic",
    race == "hispanic" ~ "Hispanic, any race",
    race %in% c("american_indian_alaska_native_a",
                "asian_a",
                "native_hawaiian_other_pacific_islander_a",
                "two_or_more_races_a",
                "other_a") ~ "Other race(s), non-Hispanic",
    race == "unknown" ~ "Unknown",
    race == "did_not_report" ~ "Unknown",
    TRUE ~ race
  )) |>
  filter(race != "Unknown" & race != "Total Population") |>
  group_by(state, race) |>
  summarise(n = sum(n, na.rm = TRUE)) |>
  left_join(total_bjs_pop_2022, by = "state") |>
  ungroup() |>
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") |>
  select(-total)|>
  mutate(state = str_replace(state, "/.*", ""))


# Read and clean BJS population data by sex for 2022 ?????????????????????????????????????????????????????? CHECK IF I NEED THIS BEFORE DOING MORE WORK
# bjs_prison_pop_by_sex_2022_raw <- read.csv(file.path(config$sp_data_path,
#                                                         "data/raw/BJS Prison Pop/p22st/p22stt02.csv"),
#                                               skip = 10)
bjs_prison_pop_by_sex_2022_raw <- read.csv(file.path(config$sp_data_path,
                                                     "data/raw/BJS Prison Pop/p22st/p22stt02.csv"))
bjs_prison_pop_by_sex_2022_raw <- bjs_prison_pop_by_sex_2022_raw[-(1:10), ]

bjs_prison_pop_by_sex_2022 <- bjs_prison_pop_by_sex_2022_raw  |>
  clean_names() |>
  select(state = x, male = x_6, female = x_7) |>
  mutate(state = str_replace(state, "/.*", "")) %>%
  mutate(state = str_replace(state, "Alaskab", "Alaska")) %>%
  mutate(state = str_replace(state, "Utahc", "Utah")) %>%
  filter(state != "" &
           state != "State" &
           state != "Federal" &
           state != "District of Columbia" &
           state != "U.S. Total" &
           state != "U.S. total" &
           state != "U.S. tota") |>
  mutate(male = str_replace_all(male, "[^\\d]", "")) |>
  mutate(male = as.numeric(male)) |>
  mutate(female = str_replace_all(female, "[^\\d]", "")) |>
  mutate(female = as.numeric(female)) |>
  pivot_longer(cols = c(male, female), names_to = "sex", values_to = "population") |>
  group_by(state) |>
  mutate(
    n = population,
    prop = population/sum(population),
    yearendpop_ped = sum(population),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(population, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          sex, "</b><br>",
                          prop_label, "<br>")) |>
  mutate(sex = case_when(sex == "male" ~ "Male",
                            sex == "female" ~ "Female",
                            TRUE ~ sex))




#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

save(ncrp_yearendpop,             file = file.path(app_folder, "ncrp_yearendpop.rds"))
save(ncrp_releases,               file = file.path(app_folder, "ncrp_releases.rds"))
save(bjs_prison_pop_by_race_2020, file = file.path(app_folder, "bjs_prison_pop_by_race_2020.rds"))
save(bjs_prison_pop_by_race_2022, file = file.path(app_folder, "bjs_prison_pop_by_race_2022.rds"))
save(bjs_prison_pop_by_sex_2022,  file = file.path(app_folder, "bjs_prison_pop_by_sex_2022.rds"))
save(bjs_prison_pop_by_rptyear,   file = file.path(app_folder, "bjs_prison_pop_by_rptyear.rds"))
save(hex_gj,                      file = file.path(app_folder, "hex_gj.rds"))
save(state_notes,                 file = file.path(app_folder, "state_notes.rds"))
