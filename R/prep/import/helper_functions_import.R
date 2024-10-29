
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#' Format citation by italicizing report titles and converting URLs to markdown links
#'
#' @param citation A string containing the citation.
#' @return A formatted string with report titles italicized and URLs as markdown links.
#' @examples
#' citation <- "Prison-Release Discretion and Prison Population Size: State Report: 2023 (https://example.com)."
#' formatted <- fnc_format_citation(citation)
#' print(formatted)
fnc_format_citation <- function(citation) {
  print("Formatting citation...")

  # Italicize the report title: Add * around report title
  formatted_citation <- str_replace_all(
    citation,
    "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
    function(x) paste0("*", x, "*")
  )
  print(paste("Title formatted:", formatted_citation))

  # Convert URLs to markdown hyperlinks and ensure the period is outside the link
  formatted_citation <- str_replace_all(
    formatted_citation,
    "(https?://[^\\s]+)\\.",  # Match the URL pattern followed by a period
    function(x) {
      url <- str_remove(x, "\\.$")  # Remove the period from the URL
      paste0("[", url, "](", url, ").")  # Place the period outside the link
    }
  )

  print(paste("Citation after URL formatting:", formatted_citation))
  return(formatted_citation)
}

#' Read a Stata file and add a year column extracted from the file name
#'
#' @param file_path A string representing the file path to the Stata file.
#' @return A dataframe with an added `rptyear` column containing the year extracted from the file name.
#' @examples
#' data <- fnc_read_and_add_year("file_2023_data.dta")
fnc_read_and_add_year <- function(file_path) {
  print(paste("Reading file:", file_path))

  # Read the data from Stata file
  data <- read_dta(file_path)
  print("Data successfully read.")

  # Extract year from file name using regular expression
  year <- sub(".*_(\\d{4})_.*", "\\1", file_path)
  print(paste("Extracted year:", year))

  # Add extracted year as rptyear column
  data <- data %>% mutate(rptyear = as.numeric(year))

  # Remove labels from state_encoded, if it exists
  if("state_encoded" %in% colnames(data)) {
    print("Removing labels from state_encoded.")
    data$state_encoded <- as.numeric(data$state_encoded)
  }

  print("Finished reading and processing data.")
  return(data)
}

#' Create FBI index by categorizing offenses and adding custom order
#'
#' @param df A dataframe containing an offense detail column `offdetail`.
#' @return A dataframe with a new `fbi_index` column added based on `offdetail`.
#' @examples
#' df <- data.frame(offdetail = c("Aggravated or simple assault", "Robbery"))
#' df <- fnc_create_fbi_index(df)
fnc_create_fbi_index <- function(df) {
  print("Creating FBI index...")

  # Define custom order (in reverse)
  custom_order <- c("Drug", "Public Order", "Property",
                    "Aggravated or Simple Assault", "Robbery", "Rape or Sexual Assault",
                    "Negligent Manslaughter", "Murder or Nonnegligent Manslaughter", "Other Violent Offenses",
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
#' @examples
#' df <- data.frame(admtype = c("Other admission (including unsentenced, transfer, AWOL/escapee return)", "Other"))
#' df <- fnc_create_admtype(df)
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

#' Clean BJS (Bureau of Justice Statistics) data by correcting state names and filtering out invalid rows
#'
#' @param df A dataframe containing BJS data with columns `state` and `bjs_prison_population`.
#' @return A cleaned dataframe with corrected state names and numeric prison population.
#' @examples
#' df <- data.frame(state = c("Alaskab", "Utahc", "U.S. Total"), bjs_prison_population = c("10,000", "5,000", "1,000,000"))
#' df <- fnc_clean_bjs_data(df)
fnc_clean_bjs_data <- function(df) {
  print("Cleaning BJS data...")

  df <- df |>
    # Remove anything after the state name in the `state` column
    mutate(state = str_replace(state, "/.*", "")) |>
    # Correct specific misspelled state names
    mutate(state = str_replace(state, "Alaskab", "Alaska")) |>
    mutate(state = str_replace(state, "Utahc", "Utah")) |>
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

#' Combine multiple files by reading and adding year columns to each dataframe
#'
#' @param files A vector of file paths to be read and combined.
#' @return A combined dataframe with an added `rptyear` column from each file.
#' @examples
#' combined_df <- combine_files(c("file_2023_data.dta", "file_2022_data.dta"))
combine_files <- function(files) {
  print("Combining files...")

  combined_data <- bind_rows(lapply(files, fnc_read_and_add_year))

  print("Files combined successfully.")
  return(combined_data)
}

#' Transform NCRP (National Corrections Reporting Program) data by modifying select columns
#'
#' @param df A dataframe containing NCRP data.
#' @return A transformed dataframe with updated race, sex, admtype, sentlgth, and offense details.
#' @examples
#' df <- fnc_transform_ncrp_data(ncrp_df)
fnc_transform_ncrp_data <- function(df) {
  print("Transforming NCRP data...")

  # Define the columns to transform if they exist in the dataset
  columns_to_check <- c("race", "sex", "admtype", "sentlgth", "offdetail")
  existing_columns <- intersect(columns_to_check, colnames(df))

  # Check if age variable is available
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

  print(paste("Existing columns to transform:", paste(existing_columns, collapse = ", ")))

  df <- df |>
    mutate(sentlgth_raw = sentlgth,
           offdetail = trimws(offdetail),
           time_between_ped_rptyear = as.numeric(years_to_estimated_pey),
           parelig_status = case_when(
             estimated_pey_status %in% c("past", "current_year") ~ "Current",
             estimated_pey_status == "missing" ~ "Missing",
             estimated_pey_status == "future" ~ "Future",
             TRUE ~ estimated_pey_status)) |>
    mutate_at(all_of(existing_columns),  # Use all_of() here
              ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>
    fnc_create_fbi_index() |>
    fnc_create_admtype() |>
    mutate(
      calc_sent_lgth = case_when(
        calc_sent_lgth_compl >= 0 & calc_sent_lgth_compl < 1 ~ "< 1 year",
        calc_sent_lgth_compl >= 1 & calc_sent_lgth_compl < 2 ~ "1-1.9 years",
        calc_sent_lgth_compl >= 2 & calc_sent_lgth_compl < 5 ~ "2-4.9 years",
        calc_sent_lgth_compl >= 5 & calc_sent_lgth_compl < 10 ~ "5-9.9 years",
        calc_sent_lgth_compl >= 10 & calc_sent_lgth_compl < 25 ~ "10-24.9 years",
        # calc_sent_lgth_compl >= 25 & calc_sent_lgth_compl != 200 ~ ">=25 years",
        calc_sent_lgth_compl >= 25  ~ ">=25 years",
        is.na(calc_sent_lgth_compl) ~ "Life, LWOP, Life plus additional years, Death",
        # calc_sent_lgth_compl == 200 ~ "Unknown",
        TRUE ~ "Unknown"),
      sentlgth = case_when(sentlgth == "Unknown" ~ calc_sent_lgth, TRUE ~ sentlgth),
      race = factor(race, levels = c("Unknown", "Other race(s), non-Hispanic",
                                     "White, non-Hispanic", "Hispanic, any race",
                                     "Black, non-Hispanic")),
      sentlgth = factor(sentlgth, levels = c("< 1 year",
                                             "1-1.9 years",
                                             "2-4.9 years",
                                             "5-9.9 years",
                                             "10-24.9 years",
                                             ">=25 years",
                                             "Life, LWOP, Life plus additional years, Death",
                                             "Unknown"))
    )

  # If age variable exists, apply transformation for age (it is already part of `existing_columns`)
  if (!is.null(age_var)) {
    print("Transforming age variable...")
    df <- df |>
      mutate(!!sym(age_var) := factor(!!sym(age_var),
                                      levels = c("18-24 years", "25-34 years",
                                                 "35-44 years", "45-54 years",
                                                 "55+ years", "Unknown")))
  }

  print("NCRP data transformation complete.")
  return(df)
}





# fnc_format_citation <- function(citation) {
#   # Italicize the report title: Add * around report title
#   formatted_citation <- str_replace_all(
#     citation,
#     "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
#     function(x) paste0("*", x, "*")
#   )
#
#   # Convert URLs to markdown hyperlinks and ensure the period is outside the link
#   formatted_citation <- str_replace_all(
#     formatted_citation,
#     "(https?://[^\\s]+)\\.",  # Match the URL pattern followed by a period
#     function(x) {
#       url <- str_remove(x, "\\.$")  # Remove the period from the URL
#       paste0("[", url, "](", url, ").")  # Place the period outside the link
#     }
#   )
#
#   return(formatted_citation)
# }
#
#
# fnc_read_and_add_year <- function(file_path) {
#   # Read the data from Stata file
#   data <- read_dta(file_path)
#
#   # Extract year from file name using regular expression
#   year <- sub(".*_(\\d{4})_.*", "\\1", file_path)
#
#   # Add extracted year as rptyear column
#   data <- data %>% mutate(rptyear = as.numeric(year))
#
#   # Remove labels from state_encoded, if it exists
#   if("state_encoded" %in% colnames(data)) {
#     data$state_encoded <- as.numeric(data$state_encoded)
#   }
#
#   return(data)
# }
#
# fnc_create_fbi_index <- function(df) {
#   # Define custom order (in reverse)
#   custom_order <- c("Drug", "Public Order", "Property",
#                     "Aggravated or Simple Assault", "Robbery", "Rape or Sexual Assault",
#                     "Negligent Manslaughter", "Murder or Nonnegligent Manslaughter", "Other Violent Offenses",
#                     "Unknown")
#   df |>
#     mutate(fbi_index = case_when(
#       offdetail == "Aggravated or simple assault" ~ "Aggravated or Simple Assault",
#       offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder or Nonnegligent Manslaughter",
#       offdetail == "Negligent manslaughter" ~ "Negligent Manslaughter",
#       offdetail == "Other violent offenses" ~ "Other Violent Offenses",
#       offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
#       offdetail == "Public order" ~ "Public Order",
#       offdetail == "Robbery" ~ "Robbery",
#       offdetail == "Other/unspecified" ~ "Other or Unspecified",
#       offdetail == "Drugs (includes possession, distribution, trafficking, other)" ~ "Drug",
#       is.na(offdetail) | offgeneral == "NA" ~ "Unknown",
#       TRUE ~ offgeneral
#     )) |>
#     mutate(fbi_index = factor(fbi_index, levels = custom_order))
# }
#
#
# fnc_create_admtype <- function(df) {
#   df |>
#     mutate(admtype = case_when(
#       admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other",
#       is.na(admtype) ~ "Unknown",
#       TRUE ~ admtype
#     ))
# }
#
#
# fnc_clean_bjs_data <- function(df) {
#   df <- df |>
#     # Remove anything after the state name in the `state` column
#     mutate(state = str_replace(state, "/.*", "")) |>
#     # Correct specific misspelled state names
#     mutate(state = str_replace(state, "Alaskab", "Alaska")) |>
#     mutate(state = str_replace(state, "Utahc", "Utah")) |>
#     # Filter out invalid state names and totals
#     filter(state != "" &
#              state != "State" &
#              state != "Federal" &
#              state != "District of Columbia" &
#              state != "U.S. Total" &
#              state != "U.S. total" &
#              state != "U.S. tota") |>
#     # Remove non-numeric characters from `bjs_prison_population` and convert it to numeric
#     mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) |>
#     mutate(bjs_prison_population = as.numeric(bjs_prison_population))
#
#   return(df)
# }
#
# # Read and combine files for releases and year-end population, both regular and consolidated
# combine_files <- function(files) {
#   bind_rows(lapply(files, fnc_read_and_add_year))
# }
#
# fnc_transform_ncrp_data <- function(df) {
#   # Define the columns to transform if they exist in the dataset
#   columns_to_check <- c("race", "sex", "admtype", "sentlgth", "offdetail")
#   existing_columns <- intersect(columns_to_check, colnames(df))
#
#   # Check if age variable is available
#   if ("ageyrend" %in% colnames(df)) {
#     age_var <- "ageyrend"
#   } else if ("agerlse" %in% colnames(df)) {
#     age_var <- "agerlse"
#   } else {
#     age_var <- NULL
#   }
#
#   # If age_var is not NULL, add it to the list of existing columns
#   if (!is.null(age_var)) {
#     existing_columns <- c(existing_columns, age_var)
#   }
#
#   df <- df |>
#     mutate(sentlgth_raw = sentlgth,
#            offdetail = trimws(offdetail),
#            time_between_ped_rptyear = as.numeric(years_to_estimated_pey),
#            parelig_status = case_when(
#              estimated_pey_status %in% c("past", "current_year") ~ "Current",
#              estimated_pey_status == "missing" ~ "Missing",
#              estimated_pey_status == "future" ~ "Future",
#              TRUE ~ estimated_pey_status)) |>
#     mutate_at(all_of(existing_columns),  # Use all_of() here
#               ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>
#     fnc_create_fbi_index() |>
#     fnc_create_admtype() |>
#     mutate(
#       calc_sent_lgth = case_when(
#         calc_sent_lgth_compl >= 0 & calc_sent_lgth_compl < 1 ~ "< 1 year",
#         calc_sent_lgth_compl >= 1 & calc_sent_lgth_compl < 2 ~ "1-1.9 years",
#         calc_sent_lgth_compl >= 2 & calc_sent_lgth_compl < 5 ~ "2-4.9 years",
#         calc_sent_lgth_compl >= 5 & calc_sent_lgth_compl < 10 ~ "5-9.9 years",
#         calc_sent_lgth_compl >= 10 & calc_sent_lgth_compl < 25 ~ "10-24.9 years",
#         calc_sent_lgth_compl >= 25 & calc_sent_lgth_compl != 200 ~ ">=25 years",
#         is.na(calc_sent_lgth_compl) ~ "Life, LWOP, Life plus additional years, Death",
#         calc_sent_lgth_compl == 200 ~ "Unknown",
#         TRUE ~ "Unknown"),
#       sentlgth = case_when(sentlgth == "Unknown" ~ calc_sent_lgth, TRUE ~ sentlgth),
#       race = factor(race, levels = c("Unknown", "Other race(s), non-Hispanic",
#                                      "White, non-Hispanic", "Hispanic, any race",
#                                      "Black, non-Hispanic")),
#       sentlgth = factor(sentlgth, levels = c("< 1 year", "1-1.9 years", "2-4.9 years",
#                                              "5-9.9 years", "10-24.9 years",
#                                              ">=25 years", "Life, LWOP, Life plus additional years, Death",
#                                              "Unknown"))
#     )
#
#   # If age variable exists, apply transformation for age (it is already part of `existing_columns`)
#   if (!is.null(age_var)) {
#     df <- df |>
#       mutate(!!sym(age_var) := factor(!!sym(age_var),
#                                       levels = c("18-24 years", "25-34 years",
#                                                  "35-44 years", "45-54 years",
#                                                  "55+ years", "Unknown")))
#   }
#
#   return(df)
# }
