#######################################
# Project: AV Parole
# File: historical_ncrp_term_records.R
# Authors: Mari Roberts
# Date last updated: August 1, 2024 (MAR)
# Description:
#    This script processes and saves NCRP term records data
#    for the years 2014 to 2020. The data is loaded, cleaned,
#    and saved as separate Excel files for each year.
#
# Steps:
#  1. Define years, ICPSR codes, and versions.
#  2. Loop through each year and ICPSR code to load the data.
#  3. Clean the data by mutating and trimming whitespace.
#  4. Save the cleaned data for each year as separate Excel files.
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















# # Define years, ICPSR codes, and versions
# years <- 2014:2020
# icpsr_codes <- c("36404", "36862", "37021", "37951", "37973", "38048", "38492")
# versions <- c("V2", "V1", "V1", "V1", "V1", "V1", "V1")
#
# # Initialize an empty list to store the data frames
# ncrp_term_records_list <- list()
#
# # Loop through each year and ICPSR code to load and process the data
# # Clean data - takes 10-15 minutes to clean
# for (i in seq_along(years)) {
#   load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_", icpsr_codes[i], "-",
#               versions[i], "/ICPSR_", icpsr_codes[i], "/DS0001/", icpsr_codes[i], "-0001-Data.rda"))
#   data_name <- ls(pattern = paste0("da", icpsr_codes[i]))
#   ncrp_term_records_list[[i]] <- get(data_name) |>
#     mutate(rptyear = years[i]) |>
#     clean_names() |>
#     mutate(across(c(state), ~ str_sub(., 6, -1))) |>
#     mutate(across(c(sex, admtype, offgeneral,
#                     education, sentlgth, offdetail, race,
#                     ageadmit, agerelease, timesrvd, reltype), ~ str_sub(., 5, -1))) |>
#     mutate(across(everything(), trimws)) |>
#     select(rptyear, state, everything())
#   rm(list = data_name)
# }
#
# # Combine all data frames into one
# ncrp_term_records <- do.call(rbind, ncrp_term_records_list)
#
# # Create the file name with date stamp and version number
# file_name <- paste0("ncrp_term_records_2014_2020_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
#
# # Combine the folder path and file name
# full_file_path <- file.path(paste0(config$sp_data_path, "/data/analysis/cleaned_files"), file_name)
#
# # Save the combined data frame to an Excel file
# write.xlsx(ncrp_term_records, file = full_file_path)



# # 2013 missing terms data
# # load(paste0(config$sp_data_path, "NCRP/ICPSR_36285-V1/ICPSR_36285/DS0001/36285-0001-Data.rda"))
# # load(paste0(config$sp_data_path, "NCRP/ICPSR_36285-V1/ICPSR_36285/DS0002/36285-0002-Data.rda"))
# # load(paste0(config$sp_data_path, "NCRP/ICPSR_36285-V1/ICPSR_36285/DS0003/36285-0003-Data.rda"))
#
# # 2014
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_36404-V2/ICPSR_36404/DS0001/36404-0001-Data.rda"))
# ncrp_term_records_rptyr2014 <- da36404.0001 |>
#   mutate(rptyear = 2014) |>
#   select(rptyear, everything()); rm(da36404.0001)
#
# # 2015
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_36862-V1/ICPSR_36862/DS0001/36862-0001-Data.rda"))
# ncrp_term_records_rptyr2015 <- da36862.0001 |>
#   mutate(rptyear = 2015) |>
#   select(rptyear, everything()); rm(da36862.0001)
#
# # 2016
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_37021-V1/ICPSR_37021/DS0001/37021-0001-Data.rda"))
# ncrp_term_records_rptyr2016 <- da37021.0001 |>
#   mutate(rptyear = 2016) |>
#   select(rptyear, everything()); rm(da37021.0001)
#
# # 2017
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_37951-V1/ICPSR_37951/DS0001/37951-0001-Data.rda"))
# ncrp_term_records_rptyr2017<- da37951.0001 |>
#   mutate(rptyear = 2017) |>
#   select(rptyear, everything()); rm(da37951.0001)
#
# # 2018
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_37973-V1/ICPSR_37973/DS0001/37973-0001-Data.rda"))
# ncrp_term_records_rptyr2018 <- da37973.0001 |>
#   mutate(rptyear = 2018) |>
#   select(rptyear, everything()); rm(da37973.0001)
#
# # 2019
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_38048-V1/ICPSR_38048/DS0001/38048-0001-Data.rda"))
# ncrp_term_records_rptyr2019 <- da38048.0001 |>
#   mutate(rptyear = 2019) |>
#   select(rptyear, everything()); rm(da38048.0001)
#
# # 2020
# load(paste0(config$sp_data_path, "/data/raw/NCRP/ICPSR_38492-V1/ICPSR_38492/DS0001/38492-0001-Data.rda"))
# ncrp_term_records_rptyr2020 <- da38492.0001 |>
#   mutate(rptyear = 2020) |>
#   select(rptyear, everything()); rm(da38492.0001)
#
# ncrp_term_records <- rbind(ncrp_term_records_rptyr2014,
#                            ncrp_term_records_rptyr2015,
#                            ncrp_term_records_rptyr2016,
#                            ncrp_term_records_rptyr2017,
#                            ncrp_term_records_rptyr2018,
#                            ncrp_term_records_rptyr2019,
#                            ncrp_term_records_rptyr2020)
#
#
#
