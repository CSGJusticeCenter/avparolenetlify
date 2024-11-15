#######################################
# Project: AV Parole
# File: historical_ncrp_term_records.R
# Authors: Mari Roberts
# Date last updated: August 1, 2024 (MAR)
# Description:
#    This script processes and saves NCRP term records data
#    for the years 2014 to 2020. The data is loaded, cleaned,
#    and saved as separate Excel files for each year.
#    These data are the base files for imputation by Seba Guzman in Stata
#
# Steps:
#  1. Define years, ICPSR codes, and versions.
#  2. Loop through each year and ICPSR code to load the data.
#  3. Clean the data by mutating and trimming whitespace.
#  4. Save the cleaned data for each year as separate Excel files.
#  5. Import 2018 data which was updated by NCRP recently.
#######################################

# Define years, ICPSR codes, and versions
years <- 2014:2020
icpsr_codes <- c("36404", "36862", "37021", "37951", "37973", "38048", "38492")
versions <- c("V2", "V1", "V1", "V1", "V1", "V1", "V1")

# 10/1/2024
# Loop through each year and ICPSR code to load and process the data
# SAVES TO SHAREPOINT
# Note that Seba renamed to v1 manually
for (i in seq_along(years)) {
  load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_", icpsr_codes[i], "-",
              versions[i], "/ICPSR_", icpsr_codes[i], "/DS0001/", icpsr_codes[i], "-0001-Data.rda"))
  data_name <- ls(pattern = paste0("da", icpsr_codes[i]))
  ncrp_term_records <- get(data_name) |>
    mutate(rptyear = years[i]) |>
    clean_names() |>
    mutate(across(c(state), ~ str_sub(., 6, -1))) |>
    mutate(across(c(sex, admtype, offgeneral,
                    education, sentlgth, offdetail, race,
                    ageadmit, agerelease, timesrvd, reltype), ~ str_sub(., 5, -1))) |>
    mutate(across(everything(), trimws)) |>
    select(rptyear, state, everything())

  # Create the file name with the full path
  file_name <- paste0("ncrp_term_records_", years[i], "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  full_file_path <- file.path(paste0(config$sp_data_path, "/data/analysis/clean_files"), file_name)

  # # Save the data frame to a separate Excel file
  # write.xlsx(ncrp_term_records, full_file_path)

  # Remove the loaded data to free up memory
  rm(list = data_name)
}


# Just do 2018 since the data was updated on NCRP
# 10/17/2024
load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_37973-V2/ICPSR_37973/DS0001/37973-0001-Data.rda"))

ncrp_term_records_2018 <- da37973.0001 |>
  mutate(rptyear = 2018) |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(c(sex, admtype, offgeneral,
                  education, sentlgth, offdetail, race,
                  ageadmit, agerelease, timesrvd, reltype), ~ str_sub(., 5, -1))) |>
  mutate(across(everything(), trimws)) |>
  select(rptyear, state, everything())

# Create the file name with the full path
file_name <- paste0("ncrp_terms_", "2018", "_", "v1.csv")
full_file_path <- file.path(paste0(config$sp_data_path, "/data/analysis/clean_files"), file_name)

# # Save the data frame to a separate Excel file
# write.csv(ncrp_term_records_2018, full_file_path)
