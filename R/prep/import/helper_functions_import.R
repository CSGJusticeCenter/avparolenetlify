#------------------------------------------------------------------------------#
# IMPORT FUNCTIONS
#------------------------------------------------------------------------------#

#' Format citation by italicizing report titles and converting URLs to markdown links
#'
#' @param citation A string containing the citation.
#' @return A formatted string with report titles italicized and URLs as markdown links.
fnc_format_citation <- function(citation) {
  # Italicize the report title
  formatted_citation <- str_replace_all(
    citation,
    "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
    function(x) paste0("*", x, "*")
  )

  # Replace "pdf." with "pdf"
  formatted_citation <- str_replace_all(
    formatted_citation,
    "pdf\\.",
    "pdf"
  )

  # Convert URLs to markdown hyperlinks
  formatted_citation <- str_replace_all(
    formatted_citation,
    "(https?://[^\\s]+)",  # Match the URL pattern
    function(url) paste0("[", url, "](", url, ")")  # Convert to markdown link
  )

  # Ensure the period is outside the link
  formatted_citation <- str_replace(formatted_citation, "\\]\\(.*\\)\\.", "].")

  return(formatted_citation)
}

#' Read a Stata File and Add a Year Column
#'
#' This function reads a Stata file, extracts the year from the file name using a regular expression,
#' and adds it as a new column named `rptyear`. If the column `state_encoded` exists, it removes the
#' labels by converting it to numeric.
#'
#' @param file_path A string representing the file path to the Stata file.
#'
#' @return A data frame with an added `rptyear` column containing the year extracted from the file name.
#' @export
fnc_read_and_add_year <- function(file_path) {
  print(paste("Reading file:", file_path))

  # Read the data from Stata file
  data <- read_dta(file_path)

  # Extract year from file name using regular expression
  year <- sub(".*_(\\d{4})_.*", "\\1", file_path)

  # Add extracted year as rptyear column
  data <- data %>% mutate(rptyear = as.numeric(year))

  # Remove labels from state_encoded, if it exists
  if ("state_encoded" %in% colnames(data)) {
    data$state_encoded <- as.numeric(data$state_encoded)
  }

  print("Finished reading and processing data.")
  return(data)
}

#' Combine Files for Releases and Year-End Population
#'
#' This function reads multiple Stata files, processes each file using `fnc_read_and_add_year`,
#' and combines them into a single data frame.
#'
#' @param files A character vector of file paths to be read and combined.
#'
#' @return A combined data frame containing the data from all files, with each file's data
#'         processed by `fnc_read_and_add_year`.
#' @export
fnc_combine_files <- function(files) {
  bind_rows(lapply(files, fnc_read_and_add_year))
}

#' Create FBI index by categorizing offenses and adding custom order
#'
#' @param df A dataframe containing an offense detail column `offdetail`.
#' @return A dataframe with a new `fbi_index` column added based on `offdetail`.
fnc_create_fbi_index <- function(df) {
  print("Creating FBI index...")

  # Define custom order (in reverse)
  custom_order <- c("Drug",
                    "Public Order",
                    "Property",
                    "Aggravated or Simple Assault",
                    "Robbery",
                    "Rape or Sexual Assault",
                    "Negligent Manslaughter",
                    "Murder or Nonnegligent Manslaughter",
                    "Other Violent Offenses",
                    "Other or Unspecified",
                    "Unknown")

  df <- df |>
    mutate(fbi_index = case_when(
      offdetail == "Aggravated or simple assault" ~ "Aggravated or Simple Assault",
      offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder or Nonnegligent Manslaughter",
      offdetail == "Negligent manslaughter" ~ "Negligent Manslaughter",
      offdetail == "Other violent offenses" ~ "Other Violent Offenses",
      offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
      offdetail == "Public order" ~ "Public Order",
      offdetail == "Robbery" ~ "Robbery",
      offdetail == "Other/unspecified" ~ "Other or Unspecified",
      offdetail == "Drugs (includes possession, distribution, trafficking, other)" ~ "Drug",
      is.na(offdetail) | offgeneral == "NA" ~ "Unknown",
      TRUE ~ offgeneral
    )) |>
    mutate(fbi_index = factor(fbi_index, levels = custom_order))

  print("FBI index created.")
  return(df)
}

#' Create a simplified `admtype` column by grouping similar admission types
#'
#' @param df A dataframe containing an `admtype` column.
#' @return A dataframe with a transformed `admtype` column, consolidating admission types.
fnc_create_admtype <- function(df) {
  print("Transforming admtype...")

  df <- df |>
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other",
      is.na(admtype) ~ "Unknown",
      TRUE ~ admtype
    ))

  print("admtype transformation complete.")
  return(df)
}

#' Transform NCRP Data
#'
#' This function transforms NCRP data by modifying or imputing variables, standardizing formats,
#' and categorizing key fields. It is used to clean and prepare NCRP datasets for further analysis.
#'
#' @param df A data frame containing NCRP data to be transformed.
#' @param states_to_update A vector of state names where specific variables should be updated.
#' @return A transformed version of the input data frame with standardized and categorized variables.
#' @details
#' - Updates variables such as `estimated_pey_status` and `sentlgth`.
#' - Handles missing data, categorizes offense types and admission types, and applies age group transformations.
#' - Factors variables like `race` and `sentlgth` for consistent ordering in analysis.
#' @export
fnc_transform_ncrp_data <- function(df, states_to_update) {
  print("Transforming NCRP data...")

  # Ensure that `states_to_update` is available
  if (!exists("states_to_update")) {
    stop("The object 'states_to_update' is not defined in the global environment.")
  }

  # Define the columns to transform if they exist in the dataset
  columns_to_check <- c("race", "sex", "admtype", "sentlgth", "offdetail")
  existing_columns <- intersect(columns_to_check, colnames(df)) # Check for existing columns

  # Check if age variable is available and set the appropriate age variable
  if ("ageyrend" %in% colnames(df)) {
    age_var <- "ageyrend"
  } else if ("agerlse" %in% colnames(df)) {
    age_var <- "agerlse"
  } else {
    age_var <- NULL
  }

  # If age_var is not NULL, add it to the list of existing columns
  if (!is.null(age_var)) {
    existing_columns <- c(existing_columns, age_var)
  }

  # Print the columns identified for transformation
  print(paste("Existing columns to transform:", paste(existing_columns, collapse = ", ")))

  # Begin transformations
  df <- df |>
    mutate(
      # Update estimated parole eligibility status for specific states
      estimated_pey_status = if_else(state %in% states_to_update, earliest_pey1_status, estimated_pey_status),
      sentlgth_raw = sentlgth, # Backup original sentence length
      offdetail = trimws(offdetail), # Trim whitespace from offense details
      time_between_ped_rptyear = as.numeric(years_to_estimated_pey), # Rename and convert years to numeric

      # Create broader eligibility categories
      parelig_status = case_when(
        estimated_pey_status %in% c("past", "current_year") ~ "Current",
        estimated_pey_status == "missing" ~ "Missing",
        estimated_pey_status == "future" ~ "Future",
        TRUE ~ estimated_pey_status
      )
    ) |>

    # Replace "NA" or actual missing values with "Unknown" for specified columns
    mutate_at(all_of(existing_columns),
              ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>

    # Apply offense and admission type categorization functions
    fnc_create_fbi_index() |>
    fnc_create_admtype() |>
    mutate(
      # Categorize imputed sentence length values
      calc_sent_lgth = case_when(
        calc_sent_lgth_compl >= 0 & calc_sent_lgth_compl < 1 ~ "< 1 year",
        calc_sent_lgth_compl >= 1 & calc_sent_lgth_compl < 2 ~ "1-1.9 years",
        calc_sent_lgth_compl >= 2 & calc_sent_lgth_compl < 5 ~ "2-4.9 years",
        calc_sent_lgth_compl >= 5 & calc_sent_lgth_compl < 10 ~ "5-9.9 years",
        calc_sent_lgth_compl >= 10 & calc_sent_lgth_compl < 25 ~ "10-24.9 years",
        calc_sent_lgth_compl >= 25 ~ ">=25 years",
        is.na(calc_sent_lgth_compl) ~ "Life, LWOP, Life plus additional years, Death",
        TRUE ~ "Unknown"
      ),
      # Replace missing `sentlgth` with categorized imputed values
      sentlgth = case_when(sentlgth == "Unknown" ~ calc_sent_lgth, TRUE ~ sentlgth),

      # Factor race with specified levels
      race = factor(race, levels = c("Unknown", "Other race(s), non-Hispanic",
                                     "White, non-Hispanic", "Hispanic, any race",
                                     "Black, non-Hispanic")),
      # Factor sentence length with specified levels
      sentlgth = factor(sentlgth, levels = c("< 1 year",
                                             "1-1.9 years",
                                             "2-4.9 years",
                                             "5-9.9 years",
                                             "10-24.9 years",
                                             ">=25 years",
                                             "Life, LWOP, Life plus additional years, Death",
                                             "Unknown"))
    )

  # Apply transformations for age variable if it exists
  if (!is.null(age_var)) {
    print("Transforming age variable...")
    df <- df |>
      mutate(!!sym(age_var) := factor(!!sym(age_var),
                                      levels = c("18-24 years", "25-34 years",
                                                 "35-44 years", "45-54 years",
                                                 "55+ years", "Unknown")))
  }

  # Print completion message and return the transformed data
  print("NCRP data transformation complete.")
  return(df)
}

#' Clean BJS (Bureau of Justice Statistics) data by correcting state names and filtering out invalid rows
#'
#' @param df A dataframe containing BJS data with columns `state` and `bjs_prison_population`.
#' @return A cleaned dataframe with corrected state names and numeric prison population.
fnc_clean_bjs_data <- function(df) {
  print("Cleaning BJS data...")

  # Initial cleanup of state names
  df <- df |>
    # Remove anything after the state name in the `state` column
    mutate(state = str_replace(state, "/.*", "")) |>
    # Correct known misspelled state names
    mutate(state = str_replace_all(state, c(
      "Wisconsing" = "Wisconsin",
      "Idah" = "Idaho",
      "Idahoo" = "Idaho",
      "Alaskab" = "Alaska",
      "Utahc" = "Utah"
    ))) |>
    # Filter out invalid state names and totals
    filter(state != "" &
             state != "State" &
             state != "Federal" &
             state != "District of Columbia" &
             state != "U.S. Total" &
             state != "U.S. total" &
             state != "U.S. tota") |>
    # Remove non-numeric characters from `bjs_prison_population` and convert it to numeric
    mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) |>
    mutate(bjs_prison_population = as.numeric(bjs_prison_population))

  print("BJS data cleaned.")
  return(df)
}

#' Load and Clean Race/Ethnicity Data from BJS Files
#'
#' This function reads a CSV file containing race and ethnicity data from the Bureau of Justice Statistics (BJS),
#' cleans column names, filters rows, and renames specified columns for consistency.
#'
#' @param file_path A string representing the file path to the BJS race/ethnicity data file.
#' @param skip_rows An integer specifying the number of rows to skip when reading the CSV file.
#' @param rename_col Optional. A string representing the column to rename to `state_federal`.
#'
#' @return A cleaned data frame with filtered rows and updated column names.
fnc_load_raceeth_data <- function(file_path, skip_rows, rename_col = NULL) {
  data <- read.csv(file.path(sp_data_path, file_path), skip = skip_rows) |>
    clean_names()

  if (!is.null(rename_col)) {
    data <- data |> rename(state_federal = !!sym(rename_col))
  }

  data |>
    filter(state_federal == "") |>
    rename(state = x) |>
    mutate(state = sub("/.*", "", state)) |>
    select(-state_federal)
}

#' Process BJS Race/Ethnicity Prison Population Data
#'
#' This function processes BJS race/ethnicity data by cleaning values, converting columns,
#' and summarizing data for specific race categories.
#'
#' @param data A data frame containing raw race/ethnicity data.
#' @param total_data A data frame containing total population data by state.
#'
#' @return A cleaned and summarized data frame with race proportions and labels.
fnc_process_bjs_raceeth_data <- function(data, total_data) {
  data |>
    mutate(across(everything(), ~str_replace_all(., ",", ""))) |>
    mutate(across(-state, as.numeric)) |>
    pivot_longer(cols = total:did_not_report, names_to = "race", values_to = "n") |>
    mutate(
      race = case_when(
        race == "total" ~ "Total Population",
        race == "white_a" ~ "White, non-Hispanic",
        race == "black_a" ~ "Black, non-Hispanic",
        race == "hispanic" ~ "Hispanic, any race",
        race %in% c("american_indian_alaska_native_a", "asian_a",
                    "native_hawaiian_other_pacific_islander_a",
                    "two_or_more_races_a", "other_a") ~ "Other race(s), non-Hispanic",
        race %in% c("unknown", "did_not_report") ~ "Unknown",
        TRUE ~ race
      )) |>
    filter(!race %in% c("Unknown", "Total Population")) |>
    group_by(state, race) |>
    summarise(n = sum(n, na.rm = TRUE)) |>
    left_join(total_data, by = "state") |>
    ungroup() |>
    mutate(prop = (n / total) * 100,
           prop_label = paste0(round(prop, 0), "%"),
           n_label = formattable::comma(n, 0),
           population_type = "In Prison") |>
    select(-total)
}

#' Process BJS Population Data by Sex
#'
#' This function reads and processes BJS population data disaggregated by sex,
#' cleaning and summarizing the data for visualization or analysis.
#'
#' @param file_path A string representing the file path to the CSV data.
#' @param skip_rows An integer specifying the number of rows to skip in the file.
#' @param male_col A string representing the column name for male population counts.
#' @param female_col A string representing the column name for female population counts.
#' @param year An integer indicating the reporting year for the data.
#'
#' @return A data frame with processed sex-based population data including proportions and labels.
fnc_process_bjs_sex_data <- function(file_path, skip_rows, male_col, female_col, year) {
  read.csv(file.path(sp_data_path, file_path))[-(1:skip_rows), ] |>
    clean_names() |>
    select(state = x, male = !!sym(male_col), female = !!sym(female_col)) |>
    mutate(
      state = str_replace_all(state, "/.*", ""),
      state = str_replace_all(state, c("Alaskab" = "Alaska", "Utahc" = "Utah"))
    ) |>
    filter(
      !state %in% c("", "State", "Federal", "District of Columbia",
                    "U.S. Total", "U.S. total", "U.S. tota")
    ) |>
    mutate(
      male = as.numeric(str_replace_all(male, "[^\\d]", "")),
      female = as.numeric(str_replace_all(female, "[^\\d]", ""))
    ) |>
    pivot_longer(cols = c(male, female), names_to = "sex", values_to = "n") |>
    group_by(state) |>
    mutate(
      prop = (n / sum(n)) * 100,
      prop_label = paste0(round(prop, 0), "%"),
      n_label = formattable::comma(n, 0),
      sex = case_when(
        sex == "male" ~ "Male",
        sex == "female" ~ "Female",
        TRUE ~ sex
      ),
      rptyear = year
    ) |>
    ungroup()
}
