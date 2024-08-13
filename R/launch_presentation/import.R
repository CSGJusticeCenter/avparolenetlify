#######################################
# Project: JRI WV
# File: import.R
# Authors: Mari Roberts
# Date last updated: July 22, 2024 (MAR)
# Description:
#   Import publicly available data on WV
#   for launch presentation.
#######################################

# Load necessary libraries
library(openxlsx)
library(janitor)
library(testthat)

# Define function to read and clean data from Excel sheets
#' Read and Clean Excel Sheet
#'
#' This function reads data from a specified Excel sheet and cleans the column names.
#'
#' @param file_path Character. The path to the Excel file.
#' @param sheet_name Character. The name of the sheet to read from the Excel file.
#' @return A data frame with cleaned column names.
#' @examples
#' read_and_clean_sheet("path/to/file.xlsx", "Sheet1")
read_and_clean_sheet <- function(file_path, sheet_name) {
  if (!file.exists(file_path)) {
    stop("The file does not exist.")
  }
  data <- read.xlsx(file_path, sheet = sheet_name) |>
    clean_names()
  return(data)
}

# Define file path
file_path <- paste0(config$sp_data_path, "/data/raw/WV Criminal Justice PPT Data.xlsx")

# List of sheet names and corresponding variable names
sheets <- list(
  avg_daily_jail_pop = "Average Daily Jail Population",
  avg_daily_doc_pop = "Average Daily DOC Population",
  jail_pop_comp = "Jail Population Composition",
  doc_pop_comp = "DOC Population Composition",
  doc_commitments = "WVDCR Commitments",
  doc_offenses = "WVDOC Offenses",
  pretrial_pop_pct = "Pretrial Population Pct",
  pretrial_pop_n = "Pretrial Population N"
)

# Import and clean data
for (name in names(sheets)) {
  assign(name, read_and_clean_sheet(file_path, sheets[[name]]))
}

# Combine categories
pretrial_pop_n <- pretrial_pop_n |>
  filter(type != "Avg Daily Total") |>
  mutate(type_new = case_when(
    type == "DOC Inmates" ~ "DOC",
    type %in% c("CM Convicted Misd", "CF Convicted Felon") ~ "Federal and Convicted Misd/Fel",
    type %in% c("PTM Pretrial Misd", "PTF Pretrial Felon") ~ "Pretrial Misd/Fel",
    type %in% c("FP Federal Pretrial", "FS Federal Sentenced") ~ "Federal and Convicted Misd/Fel"
  ))
pretrial_pop_n |>
  group_by(type_new) |>
  summarise(total_2019 = sum(x2019),
            total_2020 = sum(x2020),
            total_2021 = sum(x2021),
            total_2022 = sum(x2022),
            total_2023 = sum(x2023))

# Example jails
jail_coordinates <-  read_excel(paste0(config$sp_data_path, "/data/raw/jail_coordinates.xlsx")) |>
  clean_names() |>
  mutate(jail = name)

# Example jails
jail_capacity <-  read_excel(paste0(config$sp_data_path, "/data/raw/jail_capacity.xlsx")) |>
  clean_names() |>
  slice(1:10) |>
  mutate(name = jail)

# Calculate overcapacity amount
jail_capacity <- jail_capacity |>
  mutate(
    capacity = as.numeric(gsub("[^0-9]", "", capacity)),
    population = as.numeric(gsub("[^0-9]", "", population)),
    overcapacity = population - capacity
  ) |>
  select(year, jail, name, capacity, overcapacity, population)

# Remove extra spaces and special characters
jail_coordinates$name <- str_squish(str_replace_all(jail_coordinates$name, "[^[:alnum:] ]", ""))
jail_capacity$name <- str_squish(str_replace_all(jail_capacity$name, "[^[:alnum:] ]", ""))

# State numbers
state_jail_capacity <- jail_capacity |>
  slice(11)

# County GEOID
county_geoid <- read_excel(paste0(config$sp_data_path, "/data/raw/wv_counties_geoid_corrected.xlsx")) |>
  clean_names()

# County overdoses
county_overdoses <- read_excel(paste0(config$sp_data_path, "/data/raw/county_overdoses.xlsx")) |>
  clean_names() |>
  left_join(county_geoid, by = "county_name") |>
  mutate(GEOID = geoid)


