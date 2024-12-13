#' #------------------------------------------------------------------------------#
#' # IMPORT FUNCTIONS
#' #------------------------------------------------------------------------------#
#'
#' #' Format citation by italicizing report titles and converting URLs to markdown links
#' #'
#' #' @param citation A string containing the citation.
#' #' @return A formatted string with report titles italicized and URLs as markdown links.
#' fnc_format_citation <- function(citation) {
#'   # Italicize the report title
#'   formatted_citation <- str_replace_all(
#'     citation,
#'     "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
#'     function(x) paste0("*", x, "*")
#'   )
#'
#'   # Replace "pdf." with "pdf"
#'   formatted_citation <- str_replace_all(
#'     formatted_citation,
#'     "pdf\\.",
#'     "pdf"
#'   )
#'
#'   # Convert URLs to markdown hyperlinks
#'   formatted_citation <- str_replace_all(
#'     formatted_citation,
#'     "(https?://[^\\s]+)",  # Match the URL pattern
#'     function(url) paste0("[", url, "](", url, ")")  # Convert to markdown link
#'   )
#'
#'   # Ensure the period is outside the link
#'   formatted_citation <- str_replace(formatted_citation, "\\]\\(.*\\)\\.", "].")
#'
#'   return(formatted_citation)
#' }
#'
#' #' Read a Stata File and Add a Year Column
#' #'
#' #' This function reads a Stata file, extracts the year from the file name using a regular expression,
#' #' and adds it as a new column named `rptyear`. If the column `state_encoded` exists, it removes the
#' #' labels by converting it to numeric.
#' #'
#' #' @param file_path A string representing the file path to the Stata file.
#' #'
#' #' @return A data frame with an added `rptyear` column containing the year extracted from the file name.
#' #' @export
#' fnc_read_and_add_year <- function(file_path) {
#'   print(paste("Reading file:", file_path))
#'
#'   # Read the data from Stata file
#'   data <- read_dta(file_path)
#'
#'   # Extract year from file name using regular expression
#'   year <- sub(".*_(\\d{4})_.*", "\\1", file_path)
#'
#'   # Add extracted year as rptyear column
#'   data <- data %>% mutate(rptyear = as.numeric(year))
#'
#'   # Remove labels from state_encoded, if it exists
#'   if ("state_encoded" %in% colnames(data)) {
#'     data$state_encoded <- as.numeric(data$state_encoded)
#'   }
#'
#'   print("Finished reading and processing data.")
#'   return(data)
#' }
#'
#' #' Combine Files for Releases and Year-End Population
#' #'
#' #' This function reads multiple Stata files, processes each file using `fnc_read_and_add_year`,
#' #' and combines them into a single data frame.
#' #'
#' #' @param files A character vector of file paths to be read and combined.
#' #'
#' #' @return A combined data frame containing the data from all files, with each file's data
#' #'         processed by `fnc_read_and_add_year`.
#' #' @export
#' fnc_combine_files <- function(files) {
#'   bind_rows(lapply(files, fnc_read_and_add_year))
#' }
#'
#' #' Create FBI index by categorizing offenses and adding custom order
#' #'
#' #' @param df A dataframe containing an offense detail column `offdetail`.
#' #' @return A dataframe with a new `fbi_index` column added based on `offdetail`.
#' fnc_create_fbi_index <- function(df) {
#'   print("Creating FBI index...")
#'
#'   # Define custom order (in reverse)
#'   custom_order <- c("Drug",
#'                     "Public Order",
#'                     "Property",
#'                     "Aggravated or Simple Assault",
#'                     "Robbery",
#'                     "Rape or Sexual Assault",
#'                     "Negligent Manslaughter",
#'                     "Murder or Nonnegligent Manslaughter",
#'                     "Other Violent Offenses",
#'                     "Other or Unspecified",
#'                     "Unknown")
#'
#'   df <- df |>
#'     mutate(fbi_index = case_when(
#'       offdetail == "Aggravated or simple assault" ~ "Aggravated or Simple Assault",
#'       offdetail == "Murder (including non-negligent manslaughter)" ~ "Murder or Nonnegligent Manslaughter",
#'       offdetail == "Negligent manslaughter" ~ "Negligent Manslaughter",
#'       offdetail == "Other violent offenses" ~ "Other Violent Offenses",
#'       offdetail == "Rape/sexual assault" ~ "Rape or Sexual Assault",
#'       offdetail == "Public order" ~ "Public Order",
#'       offdetail == "Robbery" ~ "Robbery",
#'       offdetail == "Other/unspecified" ~ "Other or Unspecified",
#'       offdetail == "Drugs (includes possession, distribution, trafficking, other)" ~ "Drug",
#'       is.na(offdetail) | offgeneral == "NA" ~ "Unknown",
#'       TRUE ~ offgeneral
#'     )) |>
#'     mutate(fbi_index = factor(fbi_index, levels = custom_order))
#'
#'   print("FBI index created.")
#'   return(df)
#' }
#'
#' #' Create a simplified `admtype` column by grouping similar admission types
#' #'
#' #' @param df A dataframe containing an `admtype` column.
#' #' @return A dataframe with a transformed `admtype` column, consolidating admission types.
#' fnc_create_admtype <- function(df) {
#'   print("Transforming admtype...")
#'
#'   df <- df |>
#'     mutate(admtype = case_when(
#'       admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other",
#'       is.na(admtype) ~ "Unknown",
#'       TRUE ~ admtype
#'     ))
#'
#'   print("admtype transformation complete.")
#'   return(df)
#' }
#'
#' #' Transform NCRP Data
#' #'
#' #' This function transforms NCRP data by modifying or imputing variables, standardizing formats,
#' #' and categorizing key fields. It is used to clean and prepare NCRP datasets for further analysis.
#' #'
#' #' @param df A data frame containing NCRP data to be transformed.
#' #' @param states_to_update A vector of state names where specific variables should be updated.
#' #' @return A transformed version of the input data frame with standardized and categorized variables.
#' #' @details
#' #' - Updates variables such as `estimated_pey_status` and `sentlgth`.
#' #' - Handles missing data, categorizes offense types and admission types, and applies age group transformations.
#' #' - Factors variables like `race` and `sentlgth` for consistent ordering in analysis.
#' #' @export
#' fnc_transform_ncrp_data <- function(df, states_to_update) {
#'   print("Transforming NCRP data...")
#'
#'   # Ensure that `states_to_update` is available
#'   if (!exists("states_to_update")) {
#'     stop("The object 'states_to_update' is not defined in the global environment.")
#'   }
#'
#'   # Define the columns to transform if they exist in the dataset
#'   columns_to_check <- c("race", "sex", "admtype", "sentlgth", "offdetail")
#'   existing_columns <- intersect(columns_to_check, colnames(df)) # Check for existing columns
#'
#'   # Check if age variable is available and set the appropriate age variable
#'   if ("ageyrend" %in% colnames(df)) {
#'     age_var <- "ageyrend"
#'   } else if ("agerlse" %in% colnames(df)) {
#'     age_var <- "agerlse"
#'   } else {
#'     age_var <- NULL
#'   }
#'
#'   # If age_var is not NULL, add it to the list of existing columns
#'   if (!is.null(age_var)) {
#'     existing_columns <- c(existing_columns, age_var)
#'   }
#'
#'   # Print the columns identified for transformation
#'   print(paste("Existing columns to transform:", paste(existing_columns, collapse = ", ")))
#'
#'   # Begin transformations
#'   df <- df |>
#'     mutate(
#'       # Update estimated parole eligibility status for specific states
#'       estimated_pey_status = if_else(state %in% states_to_update, earliest_pey1_status, estimated_pey_status),
#'       sentlgth_raw = sentlgth, # Backup original sentence length
#'       offdetail = trimws(offdetail), # Trim whitespace from offense details
#'       time_between_ped_rptyear = as.numeric(years_to_estimated_pey), # Rename and convert years to numeric
#'
#'       # Create broader eligibility categories
#'       parelig_status = case_when(
#'         estimated_pey_status %in% c("past", "current_year") ~ "Current",
#'         estimated_pey_status == "missing" ~ "Missing",
#'         estimated_pey_status == "future" ~ "Future",
#'         TRUE ~ estimated_pey_status
#'       )
#'     ) |>
#'
#'     # Replace "NA" or actual missing values with "Unknown" for specified columns
#'     mutate_at(all_of(existing_columns),
#'               ~ ifelse(. == "NA" | is.na(.), "Unknown", .)) |>
#'
#'     # Apply offense and admission type categorization functions
#'     fnc_create_fbi_index() |>
#'     fnc_create_admtype() |>
#'     mutate(
#'       # Categorize imputed sentence length values
#'       calc_sent_lgth = case_when(
#'         calc_sent_lgth_compl >= 0 & calc_sent_lgth_compl < 1 ~ "< 1 year",
#'         calc_sent_lgth_compl >= 1 & calc_sent_lgth_compl < 2 ~ "1-1.9 years",
#'         calc_sent_lgth_compl >= 2 & calc_sent_lgth_compl < 5 ~ "2-4.9 years",
#'         calc_sent_lgth_compl >= 5 & calc_sent_lgth_compl < 10 ~ "5-9.9 years",
#'         calc_sent_lgth_compl >= 10 & calc_sent_lgth_compl < 25 ~ "10-24.9 years",
#'         calc_sent_lgth_compl >= 25 ~ ">=25 years",
#'         is.na(calc_sent_lgth_compl) ~ "Life, LWOP, Life plus additional years, Death",
#'         TRUE ~ "Unknown"
#'       ),
#'       # Replace missing `sentlgth` with categorized imputed values
#'       sentlgth = case_when(sentlgth == "Unknown" ~ calc_sent_lgth, TRUE ~ sentlgth),
#'
#'       # Factor race with specified levels
#'       race = factor(race, levels = c("Unknown",
#'                                      "Other race(s), non-Hispanic",
#'                                      "White, non-Hispanic",
#'                                      "Hispanic, any race",
#'                                      "Black, non-Hispanic")),
#'       # Factor sentence length with specified levels
#'       sentlgth = factor(sentlgth, levels = c("< 1 year",
#'                                              "1-1.9 years",
#'                                              "2-4.9 years",
#'                                              "5-9.9 years",
#'                                              "10-24.9 years",
#'                                              ">=25 years",
#'                                              "Life, LWOP, Life plus additional years, Death",
#'                                              "Unknown"))
#'     )
#'
#'   # Apply transformations for age variable if it exists
#'   if (!is.null(age_var)) {
#'     print("Transforming age variable...")
#'     df <- df |>
#'       mutate(!!sym(age_var) := factor(!!sym(age_var),
#'                                       levels = c("18-24 years",
#'                                                  "25-34 years",
#'                                                  "35-44 years",
#'                                                  "45-54 years",
#'                                                  "55+ years",
#'                                                  "Unknown")))
#'   }
#'
#'   # Print completion message and return the transformed data
#'   print("NCRP data transformation complete.")
#'   return(df)
#' }
#'
#' #' Clean BJS (Bureau of Justice Statistics) data by correcting state names and filtering out invalid rows
#' #'
#' #' @param df A dataframe containing BJS data with columns `state` and `bjs_prison_population`.
#' #' @return A cleaned dataframe with corrected state names and numeric prison population.
#' fnc_clean_bjs_data <- function(df) {
#'   print("Cleaning BJS data...")
#'
#'   # Initial cleanup of state names
#'   df <- df |>
#'     # Remove anything after the state name in the `state` column
#'     mutate(state = str_replace(state, "/.*", "")) |>
#'     # Correct known misspelled state names
#'     mutate(state = str_replace_all(state, c(
#'       "Wisconsing" = "Wisconsin",
#'       "Idah" = "Idaho",
#'       "Idahoo" = "Idaho",
#'       "Alaskab" = "Alaska",
#'       "Utahc" = "Utah"
#'     ))) |>
#'     # Filter out invalid state names and totals
#'     filter(state != "" &
#'              state != "State" &
#'              state != "Federal" &
#'              state != "District of Columbia" &
#'              state != "U.S. Total" &
#'              state != "U.S. total" &
#'              state != "U.S. tota") |>
#'     # Remove non-numeric characters from `bjs_prison_population` and convert it to numeric
#'     mutate(bjs_prison_population = str_replace_all(bjs_prison_population, "[^\\d]", "")) |>
#'     mutate(bjs_prison_population = as.numeric(bjs_prison_population))
#'
#'   print("BJS data cleaned.")
#'   return(df)
#' }
#'
#' #' Load and Clean Race/Ethnicity Data from BJS Files
#' #'
#' #' This function reads a CSV file containing race and ethnicity data from the Bureau of Justice Statistics (BJS),
#' #' cleans column names, filters rows, and renames specified columns for consistency.
#' #'
#' #' @param file_path A string representing the file path to the BJS race/ethnicity data file.
#' #' @param skip_rows An integer specifying the number of rows to skip when reading the CSV file.
#' #' @param rename_col Optional. A string representing the column to rename to `state_federal`.
#' #'
#' #' @return A cleaned data frame with filtered rows and updated column names.
#' fnc_load_raceeth_data <- function(file_path, skip_rows, rename_col = NULL) {
#'   data <- read.csv(file.path(sp_data_path, file_path), skip = skip_rows) |>
#'     clean_names()
#'
#'   if (!is.null(rename_col)) {
#'     data <- data |> rename(state_federal = !!sym(rename_col))
#'   }
#'
#'   data |>
#'     filter(state_federal == "") |>
#'     rename(state = x) |>
#'     mutate(state = sub("/.*", "", state)) |>
#'     select(-state_federal)
#' }
#'
#' #' Process BJS Race/Ethnicity Prison Population Data
#' #'
#' #' This function processes BJS race/ethnicity data by cleaning values, converting columns,
#' #' and summarizing data for specific race categories.
#' #'
#' #' @param data A data frame containing raw race/ethnicity data.
#' #' @param total_data A data frame containing total population data by state.
#' #'
#' #' @return A cleaned and summarized data frame with race proportions and labels.
#' fnc_process_bjs_raceeth_data <- function(data, total_data) {
#'   data |>
#'     mutate(across(everything(), ~str_replace_all(., ",", ""))) |>
#'     mutate(across(-state, as.numeric)) |>
#'     pivot_longer(cols = total:did_not_report, names_to = "race", values_to = "n") |>
#'     mutate(
#'       race = case_when(
#'         race == "total" ~ "Total Population",
#'         race == "white_a" ~ "White, non-Hispanic",
#'         race == "black_a" ~ "Black, non-Hispanic",
#'         race == "hispanic" ~ "Hispanic, any race",
#'         race %in% c("american_indian_alaska_native_a", "asian_a",
#'                     "native_hawaiian_other_pacific_islander_a",
#'                     "two_or_more_races_a", "other_a") ~ "Other race(s), non-Hispanic",
#'         race %in% c("unknown", "did_not_report") ~ "Unknown",
#'         TRUE ~ race
#'       )) |>
#'     filter(!race %in% c("Unknown", "Total Population")) |>
#'     group_by(state, race) |>
#'     summarise(n = sum(n, na.rm = TRUE)) |>
#'     left_join(total_data, by = "state") |>
#'     ungroup() |>
#'     mutate(prop = (n / total) * 100,
#'            prop_label = paste0(round(prop, 0), "%"),
#'            n_label = formattable::comma(n, 0),
#'            population_type = "In Prison") |>
#'     select(-total)
#' }
#'
#' #' Process BJS Population Data by Sex
#' #'
#' #' This function reads and processes BJS population data disaggregated by sex,
#' #' cleaning and summarizing the data for visualization or analysis.
#' #'
#' #' @param file_path A string representing the file path to the CSV data.
#' #' @param skip_rows An integer specifying the number of rows to skip in the file.
#' #' @param male_col A string representing the column name for male population counts.
#' #' @param female_col A string representing the column name for female population counts.
#' #' @param year An integer indicating the reporting year for the data.
#' #'
#' #' @return A data frame with processed sex-based population data including proportions and labels.
#' fnc_process_bjs_sex_data <- function(file_path, skip_rows, male_col, female_col, year) {
#'   # Read the CSV file and skip the specified number of rows for headers/footers
#'   read.csv(file.path(sp_data_path, file_path))[-(1:skip_rows), ] |>
#'     clean_names() |>  # Clean column names to ensure consistent formatting
#'     select(
#'       state = x,                # Select the state column (renamed as "state")
#'       male = !!sym(male_col),   # Select the male population column
#'       female = !!sym(female_col) # Select the female population column
#'     ) |>
#'     # Clean the state names and handle special cases for Alaska and Utah
#'     mutate(
#'       state = str_replace_all(state, "/.*", ""),  # Remove content after "/"
#'       state = str_replace_all(state, c("Alaskab" = "Alaska", "Utahc" = "Utah"))
#'     ) |>
#'     # Filter out rows with invalid or non-state values
#'     filter(
#'       !state %in% c("", "State", "Federal", "District of Columbia",
#'                     "U.S. Total", "U.S. total", "U.S. tota")
#'     ) |>
#'     # Clean and convert male and female population columns to numeric
#'     mutate(
#'       male = as.numeric(str_replace_all(male, "[^\\d]", "")),  # Remove non-digit characters
#'       female = as.numeric(str_replace_all(female, "[^\\d]", "")) # Remove non-digit characters
#'     ) |>
#'     # Reshape the data from wide to long format for easier analysis
#'     pivot_longer(
#'       cols = c(male, female),  # Specify columns to pivot
#'       names_to = "sex",        # Create a new column "sex" for male/female
#'       values_to = "n"          # Create a new column "n" for population counts
#'     ) |>
#'     # Group by state to calculate proportions and labels
#'     group_by(state) |>
#'     mutate(
#'       prop = (n / sum(n)) * 100,                     # Calculate percentage for each sex
#'       prop_label = paste0(round(prop, 0), "%"),      # Create a label for percentage
#'       n_label = formattable::comma(n, 0),            # Format the population count with commas
#'       sex = case_when(
#'         sex == "male" ~ "Male",                      # Standardize "male" to "Male"
#'         sex == "female" ~ "Female",                  # Standardize "female" to "Female"
#'         TRUE ~ sex                                   # Leave other values unchanged
#'       ),
#'       rptyear = year                                 # Add reporting year
#'     ) |>
#'     ungroup() # Remove grouping for final output
#' }
#'
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#'
#' #######################################
#' # Project: AV Parole
#' # File: national_trends.R
#' # Authors: Mari Roberts
#' # Date last updated: November 4, 2024 (MAR)
#' # Description:
#' #    Parole eligibility map, tables, and other visualizations for national trends page
#' #######################################
#'
#' #------------------------------------------------------------------------------#
#' # Parole Eligibility Table
#' #------------------------------------------------------------------------------#
#'
#' # Filter NCRP projections for the specified projection year and calculate rounded values
#' # - Exclude states listed in `states_to_exclude`
#' # - Calculate projected population past parole eligibility year (PEY) rounded to nearest power
#' # - Round percentage past PEY to the nearest whole number
#' # - Select only relevant columns for output
#' parole_eligibility_table_projection_year <- ncrp_projections |>
#'   filter(year == projection_year) |>
#'   # filter(!state %in% states_abolished_parole$state) |>
#'   filter(!state %in% states_to_exclude$state) |>
#'   mutate(proj_pop_past_pey_rounded = fnc_round_to_power(proj_pop_past_pey),
#'          proj_pcnt_ppey_rounded = round(proj_pcnt_ppey, 0)) |>
#'   select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded)
#'
#' # OPTION 1)
#' # Calculate the total projected population past parole eligibility (PE) across all states
#' proj_past_pe <- ncrp_projections |>
#'   filter(year == projection_year) |>
#'   summarise(past_pe_pop = sum(proj_pop_past_pey, na.rm = TRUE))
#'
#' # Round the total projected population past PE to the nearest power
#' proj_past_pe_count_rounded <- proj_past_pe |>
#'   mutate(past_pe_pop_rounded = fnc_round_to_power(past_pe_pop)) |>
#'   pull(past_pe_pop_rounded)
#'
#' # Extract the unrounded total projected population past PE for further calculations
#' proj_past_pe <- proj_past_pe |>
#'   pull(past_pe_pop)
#'
#' # Calculate the total projected prison population for the specified projection year
#' proj_prison_pop <- ncrp_population_projections |>
#'   filter(year == projection_year) |>
#'   summarise(total_prison_pop = sum(total_prison_population, na.rm = TRUE)) |>
#'   pull(total_prison_pop)
#'
#' # Calculate the ratio of total prison population to population past PE (1 in X individuals)
#' proj_past_pe_1_in_x <- round(proj_prison_pop/proj_past_pe, 0)
#'
#' #-------------------------------------------------------------------------------
#' # PEOPLE INFOGRAPHICS
#' #-------------------------------------------------------------------------------
#'
#' # General setup
#' wd <- getwd()
#' whichimage <- "person-2745706-bw"
#'
#' # Set up colors
#' light_color  <- "white"
#' empty_color   <- "#FFFFFF"
#' default_ncols <- ceiling(proj_past_pe_1_in_x)
#'
#' # Image setup
#' if (whichimage == "person-2745706-bw") {
#'   px_h <- 521
#'   px_w <- 323
#'   ex_h <- 0.005
#'   ex_w <- 0.02
#'   img_ar_hw <- (px_h * (1 + ex_h)) / (px_w * (1 + ex_w))
#'   img_ar_wh <- (px_w * (1 + ex_w)) / (px_h * (1 + ex_h))
#'   rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
#'   img <- ifelse(rawimg == 0, 1, 0)
#' }
#'
#' # Create 1 in X infographic
#' fnc_create_icons_homepage(proj_past_pe_1_in_x, emptyhumans = TRUE)
#'
#' # Save the infographic with the formatted state name
#' file_path <- file.path("img/pe_1_in_x.png")
#' ggsave(file_path, plot = last_plot(), width = 8, height = 6, dpi = 300)
#'
#' # Load, crop, and save the image
#' img <- image_read(file_path)
#' img_cropped <- image_trim(img)
#' image_write(img_cropped, file_path)
#'
#' #------------------------------------------------------------------------------#
#' # Parole Board Members by State
#' #------------------------------------------------------------------------------#
#'
#' # Get parole status information by state
#' # Get number of parole board members
#' states_parole <- state_notes |>
#'   select(state, abolished_parole, members)
#'
#'
#' #------------------------------------------------------------------------------#
#' # Parole Eligibility Table
#' #------------------------------------------------------------------------------#
#'
#' # Only include states that abolished parole + Lousiana (high PE population)
#' parole_eligibility_table <- parole_eligibility_table_projection_year |>
#'   left_join(states_parole, by = "state") |>
#'   filter(abolished_parole == "N" | state == "Louisiana") |>
#'   select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded, members)
#'
#' # Rename variables for downloadable table
#' parole_eligibility_table_download <- parole_eligibility_table |>
#'   select(State = state,
#'          `2023 Projection: In Prison Past Parole Eligibility (N)` = proj_pop_past_pey_rounded,
#'          `2023 Projection: In Prison Past Parole Eligibility (%)` = proj_pcnt_ppey_rounded,
#'          `Parole Board Members` = members)
#'
#'
#' #------------------------------------------------------------------------------#
#' # Parole Eligibility Map
#' #------------------------------------------------------------------------------#
#'
#' # Create a vector of all state names
#' all_states <- state.name
#'
#' # Define the gradient colors for categories
#' gradient_colors <- c(gradient1, gradient2, gradient3, gradient4, blue)
#'
#' # Prepare tooltips and map data
#' # Prepare data for national maps
#' map_data <- parole_eligibility_table_projection_year |>
#'
#'   # add missing states
#'   complete(state = all_states) |>
#'
#'   # add info about whether state abolished parole release
#'   left_join(states_parole, by = "state") |>
#'
#'   # Format data and create tooltip
#'   mutate(
#'     state_abb = state.abb[match(state, state.name)],
#'
#'     all_na = ifelse(is.na(proj_pop_past_pey_rounded)
#'                     , TRUE, FALSE),
#'
#'     # Create tooltips
#'     tooltip = case_when(
#'
#'       state == "Louisiana" ~
#'         paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
#'                "Percentage of People: ", paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
#'                "Number of People: ", formattable::comma(proj_pop_past_pey_rounded, 0),
#'                "<br>Louisiana is listed among the states with parole systems, despite<br>
#'                its recent abolition of parole, due to a substantial population<br>
#'                that remains eligible for parole release under the previous system.<br>"),
#'
#'       all_na == TRUE & abolished_parole == "N" ~
#'         paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
#'                "Parole eligibility data is not available.<br>"),
#'
#'       all_na == TRUE & abolished_parole == "Y" ~
#'         paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
#'                state, " abolished discretionary parole.<br>"),
#'
#'       all_na == FALSE & abolished_parole == "Y" ~
#'         paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
#'                state, " abolished discretionary parole.<br>"),
#'
#'       all_na == FALSE & abolished_parole == "N" ~
#'         paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
#'                "Percentage of People: ",
#'                paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
#'                "Number of People: ",
#'                formattable::comma(proj_pop_past_pey_rounded, 0))
#'     ),
#'
#'     tooltip = str_replace_all(tooltip, "NA%", "No Data"),
#'     tooltip = str_replace_all(tooltip, "NA", "No Data")
#'   ) |>
#'
#'   # create data labels
#'   mutate(change_label = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
#'          # change_label = str_replace_all(change_label, "NA%", "-"),
#'          change_label = str_replace_all(change_label, "NA%", " "),
#'
#'          currentperclabel = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
#'          currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))
#'
#'
#' # Calculate the breaks for the percent of people eligible for parole
#' num_breaks <- length(gradient_colors) - 1
#' breaks <- quantile(map_data$proj_pcnt_ppey_rounded, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
#' breaks[1] <- 0  # Set the first break to 0
#' breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
#' breaks <- cummax(breaks)  # Ensure breaks are strictly increasing
#'
#' # Process map_data to include gradient color and data category
#' map_data_breaks <- map_data |>
#'   mutate(
#'     gradient_color = findInterval(proj_pcnt_ppey_rounded, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
#'     gradient_color = ifelse(is.na(proj_pcnt_ppey_rounded), NA, gradient_colors[gradient_color]),
#'     proj_pcnt_ppey_rounded = round(proj_pcnt_ppey_rounded, 0),
#'     data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
#'   ) |>
#'   group_by(gradient_color) |>
#'   mutate(
#'     data_category = case_when(
#'       # state == "Louisiana" ~ "No Discretionary Parole",
#'       gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
#'       gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
#'       gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
#'       gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
#'       gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
#'     ),
#'     data_category = case_when(
#'       is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
#'       is.na(data_category) & abolished_parole == "Y" ~ "No Discretionary Parole",
#'       # state == "Louisiana" ~ "No Discretionary Parole",
#'       TRUE ~ data_category
#'     ),
#'     gradient_color = case_when(
#'       is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,
#'       is.na(gradient_color) & data_category == "No Discretionary Parole" ~ "white",
#'       # state == "Louisiana" ~ "white",
#'       TRUE ~ gradient_color
#'     ),
#'     data_category_num = case_when(
#'       is.na(data_category_num) & data_category == "Missing Data" ~ 6,
#'       is.na(data_category_num) & data_category == "No Discretionary Parole" ~ 5,
#'       # state == "Louisiana" ~ 5,
#'       TRUE ~ data_category_num
#'     )
#'   )
#'
#' # create hex map
#' map_percent <- highchart(height = 625) |>
#'
#'   hc_chart(marginTop = 50,
#'            marginBottom = 50,
#'            marginRight = 50) |>
#'
#'   hc_add_series_map(
#'     map = hex_gj,
#'     df = map_data_breaks,
#'     joinBy = "state_abb",
#'     value = "data_category_num",
#'     dataLabels = list(enabled = TRUE,
#'                       useHTML = TRUE,
#'                       align = "center",
#'                       formatter = JS("function() {
#'                           return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
#'                       }"),
#'                       style = list(fontSize = "16px",
#'                                    fontWeight = "regular",
#'                                    align = "center",
#'                                    fontFamily = "Graphik",
#'                                    textOutline = 0)),
#'
#'     borderColor = darkgray,
#'     borderWidth = 0.5,
#'     nullColor = lightgray) |>
#'
#'   hc_colorAxis(dataClassColor = "category",
#'                dataClasses = list(
#'                  list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
#'                  list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
#'                  list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
#'                  list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
#'                  list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
#'                       marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
#'                  list(from = 6, to = 6, color = darkgray, name = "Missing Data")
#'                )
#'   ) |>
#'
#'   hc_xAxis(title = "") |>
#'   hc_yAxis(title = "") |>
#'
#'   hc_add_theme(base_hc_theme) |>
#'
#'   hc_plotOptions(series = list(
#'     animation = FALSE,
#'     cursor = "pointer",
#'     borderWidth = 3,
#'     accessibility = list(
#'       enabled = TRUE,
#'       keyboardNavigation = list(enabled = TRUE),
#'       pointDescriptionFormatter = JS("function(point) {
#'         return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
#'       }")
#'     )
#'   ),
#'   accessibility = list(
#'     enabled = TRUE,
#'     keyboardNavigation = list(enabled = TRUE),
#'     linkedDescription =
#'       paste0("This hexagonal map visualizes the projected proportion of people in prison past their parole eligibility across different U.S. states in 2023. ",
#'              "States are represented as hexagons, with color gradients indicating different percentage ranges of prison populations past parole eligibility. ",
#'              "The map also includes a category for states that have abolished discretionary parole and those with missing data."),
#'     landmarkVerbosity = "one"
#'   ),
#'   area = list(accessibility = list(description =
#'                                      paste0("This chart visually compares parole eligibility status across U.S. states, using colors to denote different percentage ranges.")))
#'   ) |>
#'
#'   hc_tooltip(
#'     borderWidth = 1,
#'     borderRadius = 0,
#'     backgroundColor = '#FFFFFF', # Fully opaque white background
#'     outside = TRUE, # Ensure tooltip is rendered outside
#'     useHTML = TRUE,
#'     formatter = JS("function() {
#'           return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
#'           '<div style=\"text-align:left;\">' +
#'           '<span style=\"font-weight:normal; font-size: 1em;\">' + this.point.tooltip + '</span>' +
#'           '</div></div>';
#'     }")
#'   ) |>
#'
#'   hc_title(text = "Percentage of People in Prison Past Parole Eligibility",
#'            align = "center",
#'            style = list(fontSize = "1.75em", fontWeight = "bold")) |>
#'
#'   hc_exporting(enabled = FALSE, filename = "proj_past_parole_eligibility_2023") |>
#'
#'   hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
#'              y = 0) |>
#'
#'   hc_legend(align = "right",
#'             verticalAlign = "bottom",
#'             layout = "vertical",
#'             symbolHeight = 15,
#'             symbolWidth = 15,
#'             x = 0,
#'             y = -30,
#'             itemMarginTop = 2,
#'             itemMarginBottom = 2)
#'
#' # Add JavaScript to apply a gray border to the "No Discretionary Parole" legend item
#' map_percent <- onRender(map_percent, "
#'   function(el, x) {
#'     // Add CSS to target the circle symbol of the second legend item
#'     var style = document.createElement('style');
#'     style.innerHTML = `
#'       .highcharts-legend-item:nth-child(5) .highcharts-point {
#'         stroke: gray;
#'         stroke-width: 1px;
#'       }
#'     `;
#'     document.head.appendChild(style);
#'   }
#' ")
#'
#' # View map
#' map_percent
#'
#' # KEEP THIS CODE FOR NOW
#' # DOWNLOAD MAP OPTION
#' map_percent_download <- highchart(height = 625,
#'                                   width = 1000) |>
#'
#'   hc_chart(marginTop = 50,
#'            marginBottom = 50,
#'            marginRight = 50) |>
#'
#'   hc_add_series_map(
#'     map = hex_gj,
#'     df = map_data_breaks,
#'     joinBy = "state_abb",
#'     value = "data_category_num",
#'     dataLabels = list(enabled = TRUE,
#'                       useHTML = TRUE,
#'                       align = "center",
#'                       formatter = JS("function() {
#'                           return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
#'                       }"),
#'                       style = list(fontSize = "16px",
#'                                    fontWeight = "regular",
#'                                    align = "center",
#'                                    fontFamily = "Graphik",
#'                                    textOutline = 0)),
#'
#'     borderColor = darkgray,
#'     borderWidth = 0.5,
#'     nullColor = lightgray) |>
#'
#'   hc_colorAxis(dataClassColor = "category",
#'                dataClasses = list(
#'                  list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
#'                  list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
#'                  list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
#'                  list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
#'                  list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
#'                       marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
#'                  list(from = 6, to = 6, color = darkgray, name = "Missing Data")
#'                )
#'   ) |>
#'
#'   hc_legend(align = "right",
#'             verticalAlign = "bottom",
#'             layout = "vertical",
#'             symbolHeight = 15,
#'             symbolWidth = 15,
#'             x = 0,
#'             y = -40,
#'             itemMarginTop = 2,
#'             itemMarginBottom = 2) |>
#'
#'   hc_xAxis(title = "") |>
#'   hc_yAxis(title = "") |>
#'
#'   hc_add_theme(base_hc_theme) |>
#'
#'   hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
#'            align = "center",
#'            style = list(fontSize = "1.75em", fontWeight = "bold")) |>
#'
#'   hc_exporting(
#'     enabled = FALSE) |>
#'
#'   hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
#'              y = 0)
#'
#' # Add JavaScript to apply a gray border to the "Abolished Discretionary Parole" legend item
#' map_proj_past_parole_eligibility_2023 <- onRender(map_percent_download, "
#'   function(el, x) {
#'     // Add CSS to target the circle symbol of the second legend item
#'     var style = document.createElement('style');
#'     style.innerHTML = `
#'       .highcharts-legend-item:nth-child(5) .highcharts-point {
#'         stroke: gray;
#'         stroke-width: 1px;
#'       }
#'     `;
#'     document.head.appendChild(style);
#'   }
#' ")
#'
#' # Save map_proj_past_parole_eligibility_2023 as a temporary HTML file
#' saveWidget(map_proj_past_parole_eligibility_2023, file = "temp.html", selfcontained = TRUE)
#'
#' # Use webshot to take a screenshot and save it as a PNG
#' webshot2::webshot(
#'   url = "temp.html",
#'   file = file.path("img/map_proj_past_parole_eligibility_2023.png"),
#'   delay = 1,
#'   vwidth = 1200,
#'   vheight = 500,
#'   cliprect = c(0, 0, 1000, 625)
#' )
#'
#' #------------------------------------------------------------------------------#
#' # Save Data
#' #------------------------------------------------------------------------------#
#'
#' # Define the data objects and their corresponding file names
#' data_files <- list(
#'   map_percent                       = "map_percent.rds",
#'   proj_past_pe_count_rounded        = "proj_past_pe_count_rounded.rds",
#'   proj_past_pe_1_in_x               = "proj_past_pe_1_in_x.rds",
#'   parole_eligibility_table          = "parole_eligibility_table.rds",
#'   parole_eligibility_table_download = "parole_eligibility_table_download.rds"
#' )
#'
#' # Loop through the list and save each data object to its corresponding file
#' invisible(lapply(names(data_files), function(obj) {
#'   save(list = obj, file = file.path(app_folder, data_files[[obj]]))
#' }))
#'
#' # Define file name
#' file_name <- "parole_eligibility_by_state_2023_estimates.csv"
#'
#' # Construct the full path
#' file_path <- file.path(app_folder, file_name)
#'
#' # Example of writing to this path
#' write.csv(parole_eligibility_table_download, file_path, row.names = FALSE)
#'
#'
#'
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # Analysis Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Filter Prison Population Based on Parole Eligibility Criteria
#' #'
#' #' This function filters the prison population data to include only individuals
#' #' who meet specific criteria related to admission type and sentence length.
#' #' It also excludes states with high missingness or abolished parole systems
#' #' and skips filtering for states that don't require these criteria.
#' #'
#' #' @param data A data frame containing prison population data to be filtered.
#' #' @param exclude A data frame or vector containing states to exclude due to high missingness or abolished parole systems.
#' #' @param dont_filter A data frame or vector containing states that don't require filtering for admission type or sentence length.
#' #' @return A filtered data frame that includes only individuals and states meeting the specified criteria.
#' #' @details
#' #' - Excludes states in the `exclude` list.
#' #' - Filters individuals based on `admtype` ("New court commitment") and `sentlgth` (1+ years, except life),
#' #'   unless the state is in the `dont_filter` list.
#' #' @examples
#' #' filtered_data <- fnc_filter_pe_population_criteria(data, states_to_exclude, states_nofilter)
#' #' @export
#' fnc_filter_pe_population_criteria <- function(data, exclude, dont_filter) {
#'   # Extract the list of states to exclude (e.g., due to missing data or abolished parole)
#'   exclude <- exclude |> pull(state)
#'
#'   # Extract the list of states that don't need filtering for admission type or sentence length
#'   dont_filter <- dont_filter |> pull(state)
#'
#'   # Apply filtering criteria to the data
#'   # 1. Exclude states in the `exclude` list
#'   # 2. For other states:
#'   #    - Filter individuals with "New court commitment" as `admtype`
#'   #    - Include only those with sentence lengths of 1+ years, excluding life sentences
#'   #    - Skip filtering entirely for states in the `dont_filter` list
#'   filtered_data <- data |>
#'     filter(!(state %in% exclude)) |> # Exclude states with missing data or no parole system
#'     filter(
#'       (state %in% dont_filter) | # Include states in dont_filter without further filtering
#'         (admtype == "New court commitment" & # Filter for "New court commitment" admission type
#'            sentlgth %in% c("1-1.9 years", "2-4.9 years", # Include specific sentence length categories
#'                            "5-9.9 years", "10-24.9 years", ">=25 years"))
#'     )
#'
#'   # Return the filtered dataset
#'   return(filtered_data)
#' }
#'
#' #' Create Tooltip for Highcharts Visualization
#' #'
#' #' Generates a tooltip column for a data frame, containing formatted text for use
#' #' in Highcharts visualizations. The tooltip includes variable labels, the variable value,
#' #' the number of people, and their percentage representation.
#' #'
#' #' @param df A data frame containing the data to which tooltips will be added.
#' #' @param variable_label A string representing the label to display in the tooltip for the variable.
#' #' @param variable The variable whose values will appear in the tooltip.
#' #' @return A modified data frame with a new `tooltip` column containing the formatted tooltip text.
#' #' @details
#' #' - The tooltip text includes the variable label, variable value, number of people (formatted with commas),
#' #'   and the percentage of people (rounded to the nearest whole number).
#' #' - Tooltips are designed for use with Highcharts data visualizations.
#' #' @examples
#' #' tooltip_df <- fnc_create_tooltip(data, "Parole Eligibility Status", parelig_status)
#' #' @export
#' fnc_create_tooltip <- function(df, variable_label, variable) {
#'   df |>
#'     dplyr::mutate(
#'       tooltip = paste0(
#'         "<b>", variable_label, ":</b> ", {{ variable }}, "<br>", # Add variable label and value
#'         "<b>People:</b> ", formattable::comma(n, 0), "<br>", # Add number of people with comma formatting
#'         "<b>Percentage of People:</b> ", round(prop, 0), "%" # Add percentage of people rounded to whole number
#'       )
#'     )
#' }
#'
#' #' Filter Data by Year Based on State-Specific Year Selection
#' #'
#' #' Filters a data frame to include only rows where the reporting year (`rptyear`) matches
#' #' the year determined to be most reliable (`year_to_use`) for each state.
#' #'
#' #' @param df A data frame containing data to be filtered.
#' #' @param which_state_year A data frame containing state-year mapping with columns:
#' #'   - `state`: State name or code.
#' #'   - `year_to_use`: The year to use for filtering data for each state.
#' #' @return A filtered data frame containing only rows where `rptyear` matches `year_to_use`.
#' #' @details
#' #' - Joins the input data with the `which_state_year` data frame to add the `year_to_use` column.
#' #' - Filters the input data to include only rows where `rptyear` equals `year_to_use`.
#' #' @examples
#' #' filtered_data <- fnc_filter_by_year(data, state_year_mapping)
#' #' @export
#' fnc_filter_by_year <- function(df, which_state_year) {
#'   df |>
#'     # Join the input data with `which_state_year` to add the `year_to_use` column
#'     left_join(which_state_year, by = "state") |>
#'     # Filter rows where the reporting year (`rptyear`) matches the selected year (`year_to_use`)
#'     filter(rptyear == year_to_use)
#' }
#'
#' #' Summarize Data with Counts and Proportions
#' #'
#' #' Summarizes the input data frame by grouping it by `state` and `rptyear`, and
#' #' calculates counts, proportions, and labels for a specified column. Optionally excludes
#' #' "Unknown" values for columns other than `race`.
#' #'
#' #' @param df A data frame containing the data to be summarized.
#' #' @param count_column A string representing the column name to group by and count.
#' #' @return A summarized data frame with the following columns:
#' #' - `state`: The state name or identifier.
#' #' - `rptyear`: The reporting year.
#' #' - `<count_column>`: The grouped column values.
#' #' - `n`: Count of observations for each group.
#' #' - `prop`: Proportion of observations for each group (in percentages).
#' #' - `n_total`: Total count for each state and reporting year.
#' #' - `prop_label`: Proportion formatted as a percentage label.
#' #' - `n_label`: Count formatted with commas for readability.
#' #' @details
#' #' - Filters out missing values (`NA`) from the specified column.
#' #' - Conditionally excludes "Unknown" values unless the column is `race`.
#' #' @examples
#' #' summarized_data <- fnc_summarize_data(data, "race")
#' #' @export
#' fnc_summarize_data <- function(df, count_column) {
#'   # Convert the string column name to a symbol for use in dplyr operations
#'   count_column <- sym(count_column)
#'
#'   # Summarize the data, grouping by state and reporting year
#'   df1 <- df |>
#'     group_by(state, rptyear) |>
#'
#'     # Filter out missing values and optionally exclude "Unknown" values
#'     # - Always exclude `NA`.
#'     # - Exclude "Unknown" unless the column is `race`.
#'     filter(
#'       !is.na(!!count_column) &                                   # Exclude missing values
#'         (!(deparse(substitute(count_column)) != "race" &           # For non-"race" columns:
#'              (!!count_column == "Unknown")))                         # Exclude "Unknown".
#'     ) |>
#'
#'     # Count occurrences of each value in the specified column
#'     count(!!count_column) |>
#'
#'     # Calculate proportions and add formatted labels for visualization
#'     mutate(
#'       prop = (n / sum(n)) * 100,                # Calculate the proportion of each group
#'       n_total = sum(n),                         # Calculate the total count for the group
#'       prop_label = paste0(round(prop, 0), "%"), # Format proportion as a percentage string
#'       n_label = formattable::comma(n, 0)        # Format counts with commas
#'     ) |>
#'     ungroup() # Remove grouping for a flat data frame structure
#'
#'   # Return the summarized data frame
#'   return(df1)
#' }
#'
#' #' Filter Out States with High Missing Race Data
#' #'
#' #' Excludes rows in the input data frame where the `state` column matches states
#' #' listed in `states_with_high_missing_race`. This ensures the data only includes states
#' #' with sufficient race data for analysis.
#' #'
#' #' @param data A data frame containing the data to be filtered, with a `state` column.
#' #' @param states_with_high_missing_race A character vector or list of state names
#' #'   (or codes) to exclude due to high missingness in race data.
#' #' @return A filtered data frame that excludes rows for states in `states_with_high_missing_race`.
#' #' @details
#' #' - Converts `states_with_high_missing_race` to a character vector if it's provided as a list.
#' #' - Prints the list of excluded states for debugging purposes.
#' #' @examples
#' #' filtered_data <- fnc_filter_exclude_high_missing_race(data, c("Georgia", "Alabama"))
#' #' @export
#' fnc_filter_exclude_high_missing_race <- function(data, states_with_high_missing_race) {
#'   # Convert `states_with_high_missing_race` to a character vector if it's provided as a list
#'   if (is.list(states_with_high_missing_race)) {
#'     states_with_high_missing_race <- unlist(states_with_high_missing_race)
#'   }
#'
#'   # Debugging step: Print the list of states to be excluded for verification
#'   print("States with high missing race data:")
#'   print(states_with_high_missing_race)
#'
#'   # Ensure that both the `state` column in `data` and `states_with_high_missing_race` are in the same format
#'   # Filter out rows where `state` matches any of the states in `states_with_high_missing_race`
#'   filtered_data <- data |>
#'     filter(!(state %in% states_with_high_missing_race))
#'
#'   # Return the filtered data frame
#'   return(filtered_data)
#' }
#'
#' #' Group Offense Types into Broad Categories
#' #'
#' #' Categorizes offenses into broad groups such as "Violent," "Nonviolent," and "Other or Unknown"
#' #' based on the `fbi_index` column in the input data frame.
#' #'
#' #' @param data A data frame containing an `fbi_index` column with offense types.
#' #' @return A data frame with an additional column `offense_group`, categorizing the offenses.
#' #' @details
#' #' - The "Violent" group includes serious offenses such as murder, rape, and assault.
#' #' - The "Nonviolent" group includes drug, public order, and property offenses.
#' #' - Offenses not matching these groups are categorized as "Other or Unknown."
#' #' @examples
#' #' grouped_data <- fnc_group_offense_type(data)
#' #' @export
#' fnc_group_offense_type <- function(data) {
#'   data %>%
#'     # Add a new column `offense_group` based on the `fbi_index` offense type
#'     mutate(offense_group = case_when(
#'       # Categorize serious offenses as "Violent"
#'       fbi_index %in% c("Murder or Nonnegligent Manslaughter",
#'                        "Negligent Manslaughter",
#'                        "Rape or Sexual Assault",
#'                        "Robbery",
#'                        "Aggravated or Simple Assault",
#'                        "Other Violent Offenses") ~ "Violent",
#'
#'       # Categorize nonviolent offenses as "Nonviolent"
#'       fbi_index %in% c("Drug", "Public Order", "Property") ~ "Nonviolent",
#'
#'       # Default category for unknown or uncategorized offenses
#'       TRUE ~ "Other or Unknown"
#'     ))
#' }
#'
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # Visualization Styles and Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Common Style Elements
#' #'
#' #' This list defines the common style elements used across different themes,
#' #' including font family, color, font size, and font weight.
#' #'
#' #' @return A list of common style elements to maintain consistent appearance across visualizations.
#' #' @export
#' common_style <- list(
#'   fontFamily = "Graphik",
#'   color = "black",
#'   fontSize = "14px",
#'   fontWeight = "regular"
#' )
#'
#' #' Common Chart Style
#' #'
#' #' This list defines the common chart style elements used across different themes,
#' #' specifically for chart text formatting.
#' #'
#' #' @return A list of common chart style elements for Highcharts.
#' #' @export
#' common_chart_style <- list(
#'   fontFamily = "Graphik",
#'   fontSize = "14px",
#'   color = "black"
#' )
#'
#' #' Common Title Style
#' #'
#' #' This list defines the common title style elements, including the font family,
#' #' weight, and color, ensuring consistency across chart titles.
#' #'
#' #' @return A list of common title style elements for charts.
#' #' @export
#' common_title_style <- list(
#'   fontFamily = "Graphik",
#'   fontWeight = "bold",
#'   color = "black"
#' )
#'
#' #' Base Highcharts Theme
#' #'
#' #' This theme serves as the base for other themes in Highcharts.
#' #' It sets common styling elements like colors, chart layout, axis labels,
#' #' legend positioning, and data label styling.
#' #' @export
#' base_hc_theme <- hc_theme(
#'   colors = c(color1, color2, color3, color4, color5),
#'   chart = list(style = common_chart_style),
#'   title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
#'   subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
#'   legend = list(
#'     align = "center",
#'     verticalAlign = "top",
#'     itemStyle = common_style
#'   ),
#'   xAxis = list(
#'     labels = list(enabled = TRUE, style = common_style
#'     ),
#'     gridLineColor = "transparent",
#'     lineColor = "black",
#'     minorGridLineColor = "transparent",
#'     tickColor = "black"
#'   ),
#'   yAxis = list(
#'     labels = list(enabled = FALSE,
#'                   style = common_style
#'     ),
#'     gridLineColor = "transparent",
#'     lineColor = "transparent",
#'     majorGridLineColor = "transparent",
#'     minorGridLineColor = "transparent",
#'     tickColor = "transparent"
#'   ),
#'   plotOptions = list(
#'     series = list(
#'       events = list(
#'         legendItemClick = JS("function() { return false; }")  # Disables clicking on legend items
#'       )
#'     ),
#'     column = list(
#'       dataLabels = list(
#'         style = common_style
#'       )
#'     )
#'   ),
#'   caption = list(
#'     align = "left",
#'     style = list(
#'       fontSize = "10px",
#'       color = "#555555"
#'     )
#'   ),
#'   exporting = list(
#'     buttons = list(
#'       contextButton = list(
#'         menuItems = list(
#'           "downloadPNG"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' #' Highcharts Theme with Line Chart Support
#' #'
#' #' Custom Highcharts theme that builds on the base theme, adding specific support for line charts.
#' #'
#' #' @return A Highcharts theme configuration object.
#' #' @export
#' hc_theme_with_line <- hc_theme(
#'   colors = c(color1, color2, color3, color4, color5),
#'   chart = list(style = common_chart_style),
#'   title = list(align = "center", style = modifyList(common_title_style, list(fontSize = "16px"))),
#'   subtitle = list(align = "center", style = modifyList(common_title_style, list(fontSize = "14px"))),
#'   legend = list(align = "center", verticalAlign = "top", itemStyle = common_style),
#'   xAxis = list(
#'     labels = list(enabled = TRUE, style = common_style),
#'     tickmarkPlacement = 'on',
#'     tickLength = 5,
#'     tickWidth = 1,
#'     tickColor = "white",
#'     lineColor = "black"
#'   ),
#'   yAxis = list(
#'     labels = list(enabled = TRUE, style = common_style)
#'   ),
#'   caption = list(
#'     align = "left",
#'     style = list(
#'       fontSize = "10px",
#'       color = "#555555"
#'     )
#'   ),
#'   plotOptions = list(
#'     column = list(
#'       dataLabels = list(
#'         style = list(color = "black")
#'       )
#'     )
#'   )
#' )
#'
#' #' Highchart Labels JS Code
#' #'
#' #' JavaScript code that splits long labels (over 23 characters) into multiple rows
#' #' to prevent labels from being cut off.
#' #'
#' #' @return A JavaScript function to split long labels into multiple rows in Highcharts.
#' js_code <- "function() {
#'                     var label = this.value;
#'                     var maxLength = 23;
#'                     if (label.length > maxLength) {
#'                       var words = label.split(' ');
#'                       var result = [];
#'                       var line = [];
#'                       var lineLength = 0;
#'
#'                       words.forEach(function(word) {
#'                         if (lineLength + word.length > maxLength) {
#'                           result.push(line.join(' '));
#'                           line = [];
#'                           lineLength = 0;
#'                         }
#'                         line.push(word);
#'                         lineLength += word.length + 1;
#'                       });
#'                       if (line.length > 0) {
#'                         result.push(line.join(' '));
#'                       }
#'                       return result.join('<br>');
#'                     } else {
#'                       return label;
#'                     }
#'                   }"
#'
#' # ---------------------------------------------------------------------------- #
#' # Highcharter Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Add Accessibility to Highcharts Object
#' #'
#' #' Adds accessibility options to a Highcharts object, including keyboard navigation
#' #' and a descriptive label for screen readers.
#' #'
#' #' @param hc_object A Highcharts object to which accessibility features will be added.
#' #' @param accessibility_text A string of text used for accessibility descriptions.
#' #'
#' #' @return A Highcharts object with accessibility options enabled.
#' #' @export
#' fnc_add_hc_accessibility <- function(hc_object, accessibility_text) {
#'   hc_object |>
#'     hc_chart(accessibility = list(
#'       enabled = TRUE,
#'       keyboardNavigation = list(enabled = TRUE),
#'       description = accessibility_text,
#'       landmarkVerbosity = "one"
#'     )) |>
#'     hc_plotOptions(series = list(
#'       animation = FALSE,
#'       cursor = "pointer",
#'       borderWidth = 3,
#'       minPointLength = 4,
#'       accessibility = list(
#'         description = accessibility_text
#'       )
#'     ))
#' }
#'
#' #' Create Highcharts Pie Chart
#' #'
#' #' Generates Highcharts pie charts for each state in the input data frame, visualizing
#' #' the distribution of a given variable such as parole eligibility status.
#' #'
#' #' @param df A data frame containing data for multiple states.
#' #' @param variable The variable to visualize in the pie chart (e.g., "parelig_status").
#' #' @param source A string providing the source information for the chart caption (default: `ncrp_csg_source`).
#' #' @return A named list of Highcharts objects, one for each state in the data frame.
#' #' @details
#' #' - Iterates over states in the data frame and creates a pie chart for each.
#' #' - Adds accessibility text to describe the chart for screen readers.
#' #' - Outputs charts with exporting options enabled for saving.
#' #' @export
#' fnc_hc_pie_chart <- function(df, variable, source1 = ncrp_source, source2 = csg_source) {
#'   # Get unique states from the data
#'   states <- unique(df$state)
#'
#'   # Iterate over each state to generate pie charts
#'   all_pie_charts <- map(states, function(state_name) {
#'     # Filter the data for the current state
#'     df1 <- df |>
#'       ungroup() |> # Remove grouping to ensure accurate filtering
#'       filter(state == state_name) |> # Select data for the current state
#'       mutate(color = case_when( # Assign colors based on parole eligibility status
#'         parelig_status == "Will Be Eligible Next Year" ~ color2,
#'         parelig_status == "Missing" ~ darkgray,
#'         parelig_status == "Past Parole Eligibility at End of Year" ~ color4
#'       ))
#'
#'     # Extract the reporting year for the current state (assumes it's consistent within the state)
#'     select_year <- unique(df1$rptyear)
#'
#'     # Generate descriptive accessibility text for the pie chart
#'     category_counts <- df1 |>
#'       group_by(!!sym(variable)) |> # Group by the specified variable
#'       summarise(percentage = round(sum(n) / sum(df1$n) * 100, 0)) |> # Calculate percentage for each category
#'       arrange(desc(percentage)) # Sort categories by descending percentage
#'
#'     # Build a textual description of the chart for accessibility
#'     accessibility_text <- paste(
#'       "This pie chart shows the distribution of the prison population by", variable, "in", select_year, ".",
#'       paste(
#'         category_counts |>
#'           transmute(text = paste0(!!sym(variable), ": ", percentage, "%")) |> # Combine category and percentage
#'           pull(text), # Extract the formatted text
#'         collapse = ", " # Join all categories into a single string
#'       )
#'     )
#'
#'     # Create the Highcharts pie chart
#'     highchart() |>
#'       hc_chart(type = "pie") |>
#'       hc_plotOptions(pie = list(
#'         dataLabels = list( # Define label formatting for the chart
#'           enabled = TRUE,
#'           format = '<span style="font-size:1em; font-weight:normal">{point.name}: </span><br><span style="font-size:2em; font-weight:normal">{point.percentage:.0f}%</span>'
#'         ),
#'         colorByPoint = FALSE # Use custom colors defined in the data
#'       )) |>
#'       hc_series(list( # Add data to the chart
#'         data = list_parse(df1 |> mutate(y = n) |> transmute(
#'           name = !!sym(variable), y, color, tooltip
#'         ))
#'       )) |>
#'       hc_add_theme(base_hc_theme) |> # Add a base theme for consistency
#'       hc_tooltip(formatter = JS("function () { return this.point.tooltip; }")) |> # Custom tooltip formatting
#'       # hc_title(text = paste0("Prison Population by Parole Eligibility Status, ", select_year)) |> # Chart title
#'       # hc_title(text = "Prison Population by Parole Eligibility Status, Most Recent Year Available") |>
#'       hc_title(text = "Prison Population by Parole Eligibility Status") |>
#'       hc_exporting(enabled = TRUE, filename = paste0("prison_population_", state_name, "_", select_year)) |> # Enable export
#'       hc_caption(text = paste0(source1, ", ", select_year, " and ", source2)) |> # Add chart caption with source information
#'       fnc_add_hc_accessibility(accessibility_text) # Add accessibility text
#'   })
#'
#'   # Assign state names to the charts list for clarity
#'   all_pie_charts <- setNames(all_pie_charts, states)
#'
#'   return(all_pie_charts)
#' }
#'
#' #' Create Highcharts Column or Bar Chart
#' #'
#' #' Generates a Highcharts column or bar chart for a specific state, visualizing
#' #' metrics like percentages or proportions by a given variable.
#' #'
#' #' @param state_var The state for which the chart is being created.
#' #' @param df A data frame containing the data for multiple states.
#' #' @param x_var The variable to use on the x-axis.
#' #' @param y_var The variable to use on the y-axis (e.g., percentages or proportions).
#' #' @param metric A label for the variable being visualized (e.g., "Race").
#' #' @param type The type of data being visualized (e.g., "Releases").
#' #' @param title_type A title prefix for the chart (e.g., "Prison Population").
#' #' @param source A string providing the source information for the chart caption (default: `ncrp_csg_source`).
#' #' @param orientation The orientation of the chart ("vertical" for column, "horizontal" for bar).
#' #' @return A Highcharts object visualizing the data for the specified state.
#' #' @details
#' #' - Adjusts orientation and label alignment based on the `orientation` parameter.
#' #' - Includes accessibility text and exporting functionality.
#' #' @export
#' fnc_hc_columnchart <- function(state_var, df, x_var, y_var, metric, type, title_type,
#'                                source1, source2 = NULL,
#'                                orientation = "vertical") {
#'
#'   # Filter the data for the specified state
#'   df1 <- df |>
#'     filter(state == state_var) |> # Filter by state
#'     fnc_create_tooltip(variable_label = metric, variable = !!sym(x_var)) # Add tooltips for better interactivity
#'
#'   # Extract the reporting year for the state
#'   year <- unique(df1$rptyear)
#'
#'   # Conditionally arrange data by proportions for certain variables
#'   if (x_var %in% c("race", "fbi_index", "sex")) {
#'     df1 <- df1 |> arrange(desc(prop)) # Arrange by descending proportions
#'   }
#'
#'   # Construct the chart title
#'   title <- paste0(title_type, " by ", metric)
#'
#'   # Generate accessibility text describing the chart
#'   accessibility_text <- paste0("This graph shows the percentage of ", type,
#'                                " by ", tolower(metric), " in ",
#'                                year, " in the state of ", state_var, ".")
#'
#'   # Define the x-axis order based on the data
#'   xaxis_order <- df1[[x_var]]
#'
#'   # Determine chart type based on orientation
#'   chart_type <- ifelse(orientation == "horizontal", "bar", "column")
#'
#'   # Adjust label alignment for horizontal orientation
#'   label_alignment <- ifelse(orientation == "horizontal", "right", "center")
#'
#'   # Create the Highcharts chart
#'   highcharts <- highchart() |>
#'     hc_add_series(df1, # Add the data series
#'                   type = chart_type, # Use bar or column based on orientation
#'                   hcaes(x = !!sym(x_var), y = !!sym(y_var)), # Map x and y variables
#'                   dataLabels = list(enabled = TRUE, # Enable data labels
#'                                     format = "{point.prop_label}",
#'                                     style = list(fontWeight = "regular",
#'                                                  fontSize = "14px",
#'                                                  fontFamily = "Graphik",
#'                                                  textOutline = 0))) |>
#'     hc_xAxis(categories = xaxis_order, # Set x-axis categories
#'              labels = list(
#'                useHTML = TRUE,
#'                enabled = TRUE,
#'                formatter = JS(js_code), # Format labels with JavaScript
#'                style = list(fontSize = "14px", fontFamily = "Graphik",
#'                             textAlign = label_alignment) # Align labels based on orientation
#'              )) |>
#'     hc_yAxis(max = 100, # Set y-axis maximum to 100% for proportions
#'              labels = list(
#'                formatter = JS("function() { return this.value + '%'; }") # Append % to y-axis labels
#'              )) |>
#'     hc_add_theme(base_hc_theme) |> # Apply the base theme
#'     hc_tooltip(formatter = JS("function(){return(this.point.tooltip)}")) |> # Add custom tooltip formatter
#'     hc_legend(enabled = FALSE) |> # Disable the legend
#'     # hc_title(text = paste0(title, ", ", year)) |> # Add the chart title
#'     hc_title(text = title) |> # Add the chart title
#'     hc_exporting(enabled = TRUE, # Enable exporting functionality
#'                  filename = paste0(gsub(" ", "_", tolower(title)), "_", year)) |>
#'     fnc_add_hc_accessibility(accessibility_text) |>  # Add accessibility text
#'     hc_caption(
#'       text = paste0(
#'         source1, ", ", year,
#'         if (!is.null(source2)) paste0(" and ", source2) else ""
#'       )
#'     )
#'
#'   return(highcharts) # Return the generated Highchart
#' }
#'
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # Sentences and Visualization Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Generate Projection Sentence for Past Parole Eligibility Trends
#' #'
#' #' Creates a summary sentence describing trends in the percentage of people in prison
#' #' past parole eligibility for a given state, based on historical and projected data.
#' #'
#' #' @param state_name A string representing the state name.
#' #' @param data A data frame containing past and projected data for parole eligibility percentages,
#' #'   with columns such as `state`, `year`, `pct_past_pe`, `proj_pct_past_pe`, and `used_projected_flag`.
#' #' @return A string summarizing trends in past and projected parole eligibility percentages for the state.
#' #' @details
#' #' - Calculates the percentage change for both past and projected data.
#' #' - Includes a note if projections were used for 2019 or 2020.
#' #' - Handles missing or insufficient data gracefully, providing alternative text where necessary.
#' #' @examples
#' #' sentence <- fnc_generate_projection_sentence("Georgia", pe_proj_pop)
#' #' @export
#' fnc_generate_projection_sentence <- function(state_name, data) {
#'   # Filter data for the specified state
#'   state_data <- data |> filter(state == state_name)
#'
#'   # Extract years with valid past and projected data
#'   valid_past_years <- state_data |> filter(!is.na(pct_past_pe)) |> pull(year)
#'   valid_proj_years <- state_data |> filter(!is.na(proj_pct_past_pe)) |> pull(year)
#'
#'   # Determine earliest and latest years for past and projected data
#'   earliest_year_past <- min(valid_past_years, na.rm = TRUE) # First year with valid past data
#'   latest_year_past <- max(valid_past_years, na.rm = TRUE) # Last year with valid past data
#'   earliest_year_proj <- if (length(valid_proj_years) > 0) min(valid_proj_years, na.rm = TRUE) else NA # First projection year
#'   latest_year_proj <- if (length(valid_proj_years) > 0) max(valid_proj_years, na.rm = TRUE) else NA # Last projection year
#'
#'   # Extract percentage values for the earliest and latest past years
#'   pct_earliest <- state_data |> filter(year == earliest_year_past) |> pull(pct_past_pe)
#'   pct_latest <- state_data |> filter(year == latest_year_past) |> pull(pct_past_pe)
#'
#'   # Calculate the percentage change for past data (if available)
#'   change_past <- if (!is.na(pct_earliest) && !is.na(pct_latest)) {
#'     round(((pct_latest - pct_earliest) / pct_earliest) * 100, 0)
#'   } else NA
#'
#'   # Extract percentage values for the earliest and latest projected years
#'   proj_earliest <- if (!is.na(earliest_year_proj)) state_data |> filter(year == earliest_year_proj) |> pull(proj_pct_past_pe) else NA
#'   proj_latest <- if (!is.na(latest_year_proj)) state_data |> filter(year == latest_year_proj) |> pull(proj_pct_past_pe) else NA
#'
#'   # Calculate the percentage change for projected data (if available)
#'   change_proj <- if (!is.na(proj_earliest) && !is.na(proj_latest)) {
#'     round(((proj_latest - proj_earliest) / proj_earliest) * 100, 0)
#'   } else NA
#'
#'   # Generate a note if projections were used for specific years
#'   note <- case_when(
#'     state_data |> filter(year == 2019) |> pull(used_projected_flag) ~ " Note: 2019 data uses projections.",
#'     state_data |> filter(year == 2020) |> pull(used_projected_flag) ~ " Note: 2020 data uses projections.",
#'     TRUE ~ ""
#'   )
#'
#'   # Construct the summary sentence
#'   sentence <- paste0(
#'     "From ", earliest_year_past, " to ", latest_year_past,
#'     ", the percent of people in prison past parole eligibility ",
#'     if (!is.na(change_past)) {
#'       if (change_past > 0) paste0("increased by ", change_past, " percent. ")
#'       else if (change_past < 0) paste0("decreased by ", abs(change_past), " percent. ")
#'       else "remained the same. "
#'     } else "has insufficient data to determine a change. ",
#'     if (!is.na(earliest_year_proj) && !is.na(latest_year_proj)) {
#'       paste0(
#'         # "We've projected that from ", earliest_year_proj, " to ", latest_year_proj,
#'         # ", the percent of people past parole eligibility ",
#'         "Our forcasting model projects that the percentage of people past their initial parole eligibility ",
#'         if (!is.na(change_proj)) {
#'           if (change_proj > 0) paste0("will increase by ", change_proj, " percent")
#'           else if (change_proj < 0) paste0("will decrease by ", abs(change_proj), " percent")
#'           # else "will not change (0 percent change)"
#'           else paste0("will remain around ", round(proj_latest, 0), " percent")
#'         } else "has insufficient data to project a change",
#'         "."
#'       )
#'     } else "Projected data is insufficient to provide a future change.",
#'     note
#'   )
#'
#'   return(sentence)
#' }
#'
#' #' Generate Bar Charts for Multiple States
#' #'
#' #' Creates a collection of bar charts for each state based on the input data,
#' #' visualizing a specified metric grouped by a given variable.
#' #'
#' #' @param data A data frame containing the data to visualize, with a `state` column.
#' #' @param x_var A string representing the variable to use on the x-axis (e.g., "fbi_index").
#' #' @param metric A string representing the label for the metric being visualized.
#' #' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
#' #' @param title_type A string representing the title prefix for the chart.
#' #' @param y_var A string representing the variable to use on the y-axis (default: "prop").
#' #' @param source A string providing the source information for the chart caption.
#' #' @return A named list of Highcharts bar or column charts, one for each state.
#' #' @details
#' #' - Automatically determines the orientation (horizontal or vertical) based on the `x_var`.
#' #' - Passes the source and orientation dynamically to the chart creation function.
#' #' @examples
#' #' charts <- fnc_generate_bar_charts(data, "fbi_index", "Crime Type", "Releases", "Release Trends", "prop", "CSG Data Source")
#' #' @export
#' fnc_generate_bar_charts <- function(data, x_var, metric, type_desc, title_type, y_var = "prop", source1, source2 = NULL) {
#'   # Extract unique states from the data
#'   states <- unique(data$state)
#'
#'   # Generate charts for each state
#'   charts <- map(states, function(state_name) {
#'     # Determine chart orientation dynamically
#'     orientation <- if (x_var == "fbi_index") "horizontal" else "vertical"
#'
#'     # Call the column chart creation function for each state
#'     fnc_hc_columnchart(
#'       state_var  = state_name,   # Current state
#'       df         = data,         # Filtered data
#'       x_var      = x_var,        # X-axis variable
#'       y_var      = y_var,        # Y-axis variable (default: "prop")
#'       metric     = metric,       # Metric label
#'       type       = type_desc,    # Type description (e.g., "Releases")
#'       title_type = title_type,   # Title prefix
#'       orientation = orientation, # Determine horizontal or vertical orientation
#'       source1 = source1,
#'       source2 = source2
#'     )
#'   })
#'
#'   # Assign state names to the generated charts
#'   setNames(charts, states)
#' }
#'
#' #' Generate Summary Sentences for Multiple States
#' #'
#' #' Creates a collection of summary sentences for each state based on the input data,
#' #' describing trends or distributions for a specified variable.
#' #'
#' #' @param data A data frame containing the data to summarize, with a `state` column.
#' #' @param x_var A string representing the variable to summarize (e.g., "fbi_index").
#' #' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
#' #' @return A named list of sentences, one for each state.
#' #' @details
#' #' - Uses `fnc_generate_columnchart_sentence` to create state-specific summaries.
#' #' @examples
#' #' sentences <- fnc_generate_sentences(data, "fbi_index", "Releases")
#' #' @export
#' fnc_generate_sentences <- function(data, x_var, type_desc) {
#'   # Extract unique states from the data
#'   states <- unique(data$state)
#'
#'   # Generate sentences for each state
#'   sentences <- map(states, function(state_name) {
#'     # Call the sentence generation function for each state
#'     fnc_generate_columnchart_sentence(
#'       state_var = state_name, # Current state
#'       df        = data,      # Filtered data
#'       x_var     = x_var,     # X-axis variable for grouping
#'       type      = type_desc  # Type description (e.g., "Releases")
#'     )
#'   })
#'
#'   # Assign state names to the generated sentences
#'   setNames(sentences, states)
#' }
#'
#' #' Generate a Column Chart Summary Sentence
#' #'
#' #' Creates a summary sentence describing trends or distributions based on the input data
#' #' for a specific state and a given variable.
#' #'
#' #' @param state_var A string representing the state name or code.
#' #' @param df A data frame containing data to summarize, with a `state` column.
#' #' @param x_var A string representing the variable to summarize (e.g., "fbi_index").
#' #' @param type_desc A string describing the type of data (e.g., "Releases" or "Admissions").
#' #' @return A string summarizing trends or distributions for the specified state and variable.
#' #' @details
#' #' - Handles special cases for `fbi_index`, `sex`, age-related variables, and sentence length.
#' #' - Dynamically adjusts wording and formatting based on the input variable.
#' #' - Ensures robust handling of missing or incomplete data.
#' #' @examples
#' #' sentence <- fnc_generate_columnchart_sentence("Georgia", data, "fbi_index", "Releases")
#' #' @export
#' fnc_generate_columnchart_sentence <- function(state_var, df, x_var, type_desc) {
#'   # Filter the data for the specified state and arrange by proportion in descending order
#'   df1 <- df |>
#'     filter(state == state_var) |>
#'     arrange(-prop)
#'
#'   # Extract the unique reporting year for the state
#'   year <- unique(df1$rptyear)
#'
#'   # Handle cases where data is missing or insufficient
#'   if (nrow(df1) < 1 || is.na(df1$prop[1])) {
#'     return(paste0("Data for ", state_var, " is missing or incomplete."))
#'   }
#'
#'   # Convert values to lowercase for "sex" variable
#'   if (x_var == "sex") {
#'     df1[[x_var]] <- tolower(df1[[x_var]])
#'   }
#'
#'   # Special handling for "fbi_index" variable
#'   if (x_var == "fbi_index") {
#'     # Identify the top categories based on the highest proportion
#'     max_prop <- max(round(df1$prop, 0))
#'     top_categories <- df1 |>
#'       filter(round(prop, 0) == max_prop) |>
#'       arrange(desc(prop))
#'
#'     # Create sentences for top offense categories
#'     fbi_sentences <- top_categories |>
#'       mutate(fbi_sentence = paste0(tolower(fbi_index), " (", round(prop, 0), " percent)")) |>
#'       pull(fbi_sentence)
#'
#'     # Format the final sentence using commas and "and" for readability
#'     fbi_sentence_final <- if (length(fbi_sentences) > 1) {
#'       paste(paste(fbi_sentences[-length(fbi_sentences)], collapse = ", "),
#'             ", and ", fbi_sentences[length(fbi_sentences)], sep = "")
#'     } else {
#'       fbi_sentences
#'     }
#'
#'     # Summarize violent and nonviolent offense proportions
#'     current_ped_offense_group <- df |>
#'       select(state, fbi_index, offense_group, n) |>
#'       filter(offense_group == "Violent" | offense_group == "Nonviolent") |>
#'       group_by(state, offense_group) |>
#'       summarise(total_offenses = sum(n), .groups = 'drop') |>
#'       group_by(state) |>
#'       mutate(prop = total_offenses / sum(total_offenses))
#'
#'     violent_prop <- current_ped_offense_group |>
#'       filter(state == state_var, offense_group == "Violent") |>
#'       pull(prop) |>
#'       round(2) * 100
#'     nonviolent_prop <- current_ped_offense_group |>
#'       filter(state == state_var, offense_group == "Nonviolent") |>
#'       pull(prop) |>
#'       round(2) * 100
#'
#'     # Construct the final sentence for "fbi_index"
#'     sentences <- paste0(#"In ", year, ", ",
#'       violent_prop, " percent of people ", type_desc,
#'       " were in prison for violent offenses and ",
#'       nonviolent_prop, " percent for nonviolent offenses. ",
#'       "Most people ", type_desc, " were incarcerated for ", fbi_sentence_final, " offenses.")
#'   }
#'   # Special handling for age-related variables
#'   else if (x_var == "ageyrend" | x_var == "agerlse") {
#'     age_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
#'     sentences <- paste0(#"In ", year, ", ",
#'       round(df1$prop[1], 0),
#'       " percent of people ", type_desc, " were between the ages of ",
#'       age_range[1], " and ", age_range[2], " old.")
#'   }
#'   # Special handling for sentence length variables
#'   else if (x_var == "sentlgth") {
#'     sent_range <- strsplit(as.character(df1[[x_var]][1]), "-")[[1]]
#'     sentences <- paste0(#"In ", year, ", ",
#'       round(df1$prop[1], 0),
#'       " percent of people ", type_desc, " had sentence lengths between ",
#'       sent_range[1], " and ", sent_range[2], ".")
#'   }
#'   # General case for other variables
#'   else {
#'     sentences <- paste0(#"In ", year, ", ",
#'       round(df1$prop[1], 0),
#'       " percent of people ", type_desc, " were ",
#'       df1[[x_var]][1], ".")
#'   }
#'
#'   return(sentences)
#' }
#'
#'
#'
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # Disparities Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Filter Data by State and Year
#' #'
#' #' This function filters a dataset to include only rows corresponding to the
#' #' specified state and the most recent reporting year (`rptyear`) for that state.
#' #'
#' #' @param df A data frame containing at least `state` and `rptyear` columns.
#' #' @param state_var A string specifying the state to filter.
#' #' @return A list containing:
#' #'   - `data`: A filtered data frame for the specified state and year.
#' #'   - `year`: The most recent reporting year (`rptyear`) for the specified state.
#' #' @examples
#' #' filtered <- fnc_filter_data_by_state_year(df, "Georgia")
#' #' head(filtered$data)  # View filtered data
#' #' filtered$year        # View the most recent year
#' #' @export
#' fnc_filter_data_by_state_year <- function(df, state_var) {
#'
#'   # Extract the most recent year for the specified state
#'   year <- df |>
#'     filter(state == state_var) |>
#'     pull(rptyear) |>
#'     max(na.rm = TRUE)
#'
#'   # Filter the data frame to include only rows for the specified state and year
#'   df_filtered <- df |>
#'     ungroup() |>  # Ensure no grouping to avoid filtering issues
#'     filter(state == state_var) |>
#'     filter(rptyear == year)
#'
#'   # Return the filtered data and the year as a list
#'   list(data = df_filtered, year = year)
#' }
#'
#' #' Create a Lollipop Chart
#' #'
#' #' This function generates a lollipop chart for visualizing average values
#' #' (e.g., time served) by a specified group variable (e.g., sex, race) for a given state.
#' #'
#' #' @param df A data frame containing the data to visualize.
#' #' @param group_var A string indicating the grouping variable (`"sex"` or `"race"`).
#' #' @param state_name A string specifying the state for which the chart is generated.
#' #' @param height An integer defining the chart height in pixels. Default is 200.
#' #' @param source A string specifying the data source for the chart caption.
#' #' @return A `highchart` object representing the lollipop chart.
#' #' @examples
#' #' chart <- fnc_create_lollipop_chart(data, "race", "Georgia", source = "NCRP")
#' #' @export
#' fnc_create_lollipop_chart <- function(df, group_var, state_name, height = 200, source) {
#'
#'   # Define consistent group labels, colors, and shapes
#'   if (group_var == "sex") {
#'     group_labels <- c("Male", "Female")
#'     colors <- c(teal, purple)  # Colors for male and female
#'     shapes <- c("circle", "triangle")  # Shapes for male and female
#'   } else {
#'     group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
#'     colors <- c(teal, blue, purple, red)  # Colors for race groups
#'     shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
#'   }
#'
#'   # Filter data for the specified state
#'   df1 <- df |>
#'     ungroup() |>
#'     filter(state == state_name) |>
#'     arrange(desc(average_los)) |>
#'     mutate(group_num = row_number(),
#'            color = case_when(
#'              !!sym(group_var) == group_labels[1] ~ colors[1],
#'              !!sym(group_var) == group_labels[2] ~ colors[2],
#'              !!sym(group_var) == group_labels[3] ~ colors[3],
#'              !!sym(group_var) == group_labels[4] ~ colors[4]
#'            ))
#'
#'   year <- unique(df1$rptyear)
#'
#'   # Determine the title based on the group_var
#'   chart_title <- if (group_var == "sex") {
#'     # paste("Average Time Served by Sex,", year)
#'     paste("Average Time Served by Sex")
#'   } else if (group_var == "race") {
#'     # paste("Average Time Served by Race and Ethnicity,", year)
#'     paste("Average Time Served by Race and Ethnicity")
#'   } else {
#'     # paste("Average Time Served by", group_var, ",", year)
#'     paste("Average Time Served by", group_var)
#'   }
#'
#'   # Generate accessibility text based on the data
#'   accessibility_text <- paste0("The chart below shows the average time served for different ",
#'                                group_var, " groups in ", state_name, ". ",
#'                                group_labels[1], " spent on average ", df1$average_los[df1$group_num == 1],
#'                                " years, followed by ", group_labels[2], " with ", df1$average_los[df1$group_num == 2],
#'                                " years, ", group_labels[3], " with ", df1$average_los[df1$group_num == 3],
#'                                " years, and ", group_labels[4], " with ", df1$average_los[df1$group_num == 4],
#'                                " years.")
#'
#'   max_los <- max(df1$average_los, na.rm = TRUE)
#'
#'   # Create a named list for y-axis labels
#'   y_labels <- as.list(setNames(as.character(df1[[group_var]]), df1$group_num))
#'
#'   # Create the dataframe for lines in the lollipop chart
#'   df_lines <- df1 |>
#'     mutate(start_x = 0, end_x = average_los) |>
#'     select(group_num, start_x, end_x, !!sym(group_var))
#'
#'   # Reshape data for highcharter
#'   df_lines <- df_lines |>
#'     gather(key = "point", value = "value", start_x, end_x)
#'
#'   # Initialize the highchart object
#'   highcharts <- highchart() |>
#'     hc_title(text = chart_title) |>
#'     hc_add_series(
#'       df_lines,
#'       type = 'line',
#'       hcaes(x = value, y = group_num, group = !!sym(group_var)),
#'       lineWidth = 1,
#'       color = "black",
#'       dashStyle = "solid",
#'       opacity = 1,
#'       marker = list(enabled = FALSE),
#'       enableMouseTracking = FALSE,
#'       showInLegend = FALSE
#'     )
#'
#'   # Add scatter series for each group with appropriate marker symbols
#'   for (i in seq_along(group_labels)) {
#'     highcharts <- highcharts |>
#'       hc_add_series(
#'         df1 %>% filter(!!sym(group_var) == group_labels[i]),
#'         type = 'scatter',
#'         color = colors[i],
#'         hcaes(x = average_los, y = group_num, group = !!sym(group_var), name = !!sym(group_var)),
#'         marker = list(
#'           radius = 5,
#'           symbol = shapes[i]  # Use unique shape for each group
#'         ),
#'         dataLabels = list(
#'           enabled = TRUE,
#'           format = '{point.x:.1f} Years',
#'           align = "left",
#'           y = 9,
#'           x = 8,
#'           style = list(color = 'black', fontWeight = "regular", fontSize = "12px")
#'         )
#'       )
#'   }
#'
#'   # Add y-axis and x-axis customizations
#'   highcharts <- highcharts |>
#'     hc_add_theme(base_hc_theme) |>
#'     hc_yAxis(
#'       labels = list(
#'         enabled = TRUE,
#'         style = list(
#'           color = 'black',
#'           fontWeight = "regular",
#'           fontSize = "12px"
#'         )
#'       ),
#'       title = list(text = ""),
#'       majorGridLineColor = "transparent",
#'       gridLineColor = "transparent",
#'       lineColor = "transparent",
#'       tickColor = "white",
#'       categories = y_labels
#'     ) |>
#'     hc_xAxis(
#'       title = list(text = ""),
#'       labels = list(enabled = FALSE),
#'       lineColor = "transparent",
#'       tickLength = 0,
#'       gridLineColor = "transparent",
#'       tickColor = "transparent",
#'       max = max_los * 1.5
#'     ) |>
#'     hc_exporting(enabled = TRUE,
#'                  filename = paste0(gsub(" ", "_", tolower(chart_title)), "_",
#'                                    year)) |>
#'     hc_tooltip(enabled = FALSE) |>
#'     hc_legend(enabled = FALSE) |>
#'     hc_size(height = height) |>
#'     fnc_add_hc_accessibility(accessibility_text) |>
#'     hc_caption(text = paste0(source, ", ", year))
#'
#'   return(highcharts)
#' }
#'
#' #' Generate Lollipop Charts for All States
#' #'
#' #' This function generates lollipop charts for all states in the provided data
#' #' by iterating over the unique states.
#' #'
#' #' @param df A data frame containing the data to visualize.
#' #' @param compare_var A string indicating the grouping variable (`"sex"` or `"race"`).
#' #' @param height An integer defining the chart height in pixels. Default is 200.
#' #' @return A named list of `highchart` objects, where each element corresponds
#' #'   to a state.
#' #' @examples
#' #' charts <- fnc_generate_lollipop_charts(data, "race")
#' #' charts$Georgia  # View the chart for Georgia
#' #' @export
#' fnc_generate_lollipop_charts <- function(df, compare_var, height = 200) {
#'
#'   # Extract unique states to iterate over
#'   states <- unique(df$state)
#'
#'   # Generate lollipop chart for each state
#'   all_charts <- purrr::map(states, function(state_var) {
#'     fnc_create_lollipop_chart(
#'       df = df,
#'       group_var = compare_var,
#'       state_name = state_var,
#'       source = ncrp_source,
#'       height = height
#'     )
#'   })
#'
#'   # Assign state names as the list names for easy access
#'   all_charts <- setNames(all_charts, states)
#'
#'   return(all_charts)
#' }
#'
#' #' Generate Scatter Charts by State
#' #'
#' #' This function generates scatter charts visualizing disparities in measures such as
#' #' average time served or years past parole eligibility by offense type for each state.
#' #' The visualizations highlight group differences (e.g., by race or sex) and are customized
#' #' with dynamic labels, colors, and accessibility features.
#' #'
#' #' @param df A data frame containing the data to be visualized, including offense type,
#' #'   grouping variables (e.g., race or sex), and the measure (e.g., `average_los`).
#' #' @param group_var A string specifying the grouping variable (`"sex"` or `"race"`).
#' #' @param measure A string specifying the measure variable (e.g., `"average_los"`).
#' #' @param source A string for the chart's source caption (default is `ncrp_csg_source`).
#' #' @return A named list of Highcharts objects, each corresponding to a state.
#' #' @export
#' fnc_create_scatter_charts_by_state <- function(df, group_var, measure, source1, source2 = NULL) {
#'
#'   # Extract unique states to iterate over
#'   states <- unique(df$state)
#'
#'   # Iterate through each state to generate scatter charts
#'   all_charts <- purrr::map(.x = states, .f = function(state_name) {
#'
#'     # Define group-specific labels, colors, and shapes
#'     if (group_var == "sex") {
#'       group_labels <- c("Male", "Female")
#'       colors <- c(teal, purple)  # Colors for male and female
#'       shapes <- c("circle", "triangle")  # Shapes for male and female
#'     } else {
#'       group_labels <- c("Black, non-Hispanic", "Hispanic, any race", "Other race(s), non-Hispanic", "White, non-Hispanic")
#'       colors <- c(teal, blue, purple, red)  # Colors for race groups
#'       shapes <- c("square", "circle", "diamond", "triangle")  # Shapes for race groups
#'     }
#'
#'     # Filter data for the specific state and prepare for visualization
#'     df1 <- df |>
#'       ungroup() |>
#'       filter(state == state_name) |>
#'       arrange(desc(!!sym(measure))) |>
#'       mutate(group_num = row_number(),  # Add group numbering
#'              color = case_when(  # Assign colors dynamically
#'                !!sym(group_var) == group_labels[1] ~ colors[1],
#'                !!sym(group_var) == group_labels[2] ~ colors[2],
#'                !!sym(group_var) == group_labels[3] ~ colors[3],
#'                !!sym(group_var) == group_labels[4] ~ colors[4]
#'              ))
#'
#'     # Extract the year of the data for labeling
#'     year <- unique(df1$rptyear)
#'
#'     # Define dynamic titles and labels for the chart
#'     x_axis_title <- ifelse(measure == "average_los", "Average Time Served (Years)", "Average Years Past Parole Eligibility")
#'     chart_title <- paste0("Average ", ifelse(measure == "average_los", "Time Served", "Years Past Parole Eligibility"),
#'                           " by Offense and ", ifelse(group_var == "sex", "Sex", "Race and Ethnicity"))
#'
#'     # Generate accessibility text for the chart
#'     accessibility_measure <- ifelse(measure == "average_los", "average length of stay", "average years past parole eligibility")
#'     accessibility_text <- paste0("The chart shows the ", accessibility_measure, " for different ",
#'                                  group_var, " groups in ", state_name, ". ", group_labels[1],
#'                                  " spent an average of ", df1[[measure]][df1$group_num == 1],
#'                                  " years, followed by ", group_labels[2], " with ",
#'                                  df1[[measure]][df1$group_num == 2], " years, and ",
#'                                  group_labels[3], " with ", df1[[measure]][df1$group_num == 3], " years.")
#'
#'     # Set maximum value for scaling
#'     max_los <- max(df1[[measure]], na.rm = TRUE)
#'
#'     # Define the desired order of offense types
#'     desired_order <- c(
#'       "Drug",
#'       "Public Order",
#'       "Property",
#'       "Aggravated or Simple Assault",
#'       "Robbery",
#'       "Rape or Sexual Assault",
#'       "Negligent Manslaughter",
#'       "Murder or Nonnegligent Manslaughter",
#'       "Other Violent Offenses"
#'     )
#'
#'     # Map offense types to their positions
#'     y_labels <- setNames(as.list(desired_order), seq_along(desired_order))
#'
#'     # Initialize Highcharts object
#'     highcharts <- highchart() |>
#'       hc_title(text = chart_title) |>
#'       hc_yAxis(
#'         title = list(text = ""),  # Y-axis title
#'         labels = list(enabled = TRUE, style = list(color = "black")),  # Style Y-axis labels
#'         categories = y_labels,  # Map categories to offense types
#'         gridLineColor = "transparent",  # Remove grid lines
#'         reversed = TRUE  # Reverse order for better readability
#'       ) |>
#'       hc_xAxis(
#'         title = list(text = x_axis_title, style = list(color = "black")),  # X-axis title
#'         labels = list(style = list(color = "black")),  # Style X-axis labels
#'         gridLineDashStyle = "Dash",  # Dashed grid lines
#'         gridLineWidth = 1,  # Set grid line width
#'         gridLineColor = "lightgray",  # Set grid line color
#'         tickLength = 0  # Remove tick marks
#'       ) |>
#'       hc_tooltip(
#'         useHTML = TRUE,
#'         formatter = JS("function() {
#'           return '<b>' + this.series.name + '</b><br/>' +
#'                  'Offense: ' + (this.point.fbi_index || 'Unknown') + '<br/>' +
#'                  'Average Years: ' + this.point.x.toFixed(1) + '<br/>' +
#'                  'People: ' + (this.point.people ? this.point.people.toLocaleString() : 'N/A');
#'         }")  # Tooltip with offense, years, and people count
#'       ) |>
#'       hc_legend(layout = "horizontal", verticalAlign = "top") |>
#'       hc_add_theme(base_hc_theme) |>
#'       fnc_add_hc_accessibility(accessibility_text) |>
#'       hc_caption(
#'         text = paste0(
#'           source1, ", ", year,
#'           if (!is.null(source2)) paste0(" and ", source2) else ""
#'         )
#'       ) |>
#'       hc_exporting(enabled = TRUE,
#'                    filename = paste0(gsub(" ", "_", tolower(chart_title)), "_",
#'                                      year))
#'
#'     # Add scatter series for each group dynamically
#'     for (i in seq_along(group_labels)) {
#'       highcharts <- highcharts |>
#'         hc_add_series(
#'           df1 |> filter(!!sym(group_var) == group_labels[i]),  # Filter for the group
#'           type = 'scatter',  # Scatter plot
#'           color = colors[i],  # Assign color
#'           hcaes(x = !!sym(measure), y = as.numeric(factor(fbi_index)), group = !!sym(group_var)),
#'           marker = list(symbol = shapes[i], radius = 5)  # Assign marker shape and size
#'         )
#'     }
#'
#'     return(highcharts)
#'   })
#'
#'   # Assign state names to the resulting charts list
#'   all_charts <- setNames(all_charts, states)
#'
#'   return(all_charts)
#' }
#'
#' #' Generate Disparity Sentences
#' #'
#' #' This function generates disparity sentences comparing average time served or
#' #' years past parole eligibility by race or sex for a given dataset. The function
#' #' calculates differences between groups and constructs descriptive sentences to
#' #' summarize these disparities.
#' #'
#' #' @param df A data frame containing the dataset with necessary variables.
#' #' @param type A string indicating the type of analysis: either `"in prison"`
#' #'   (for time served) or `"past parole eligibility"`.
#' #' @param compare_var A string specifying the comparison variable: `"race"` or
#' #'   `"sex"`.
#' #' @param los_col A string specifying the column name for the length of stay
#' #'   (LOS) or years past parole eligibility.
#' #' @return A named list of sentences, with state names as keys and the corresponding
#' #'   sentences as values.
#' #' @examples
#' #' disparity_sentences <- fnc_generate_disparity_sentences(df, "in prison", "race", "average_los")
#' #' disparity_sentences$Georgia
#' #' @export
#' # fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#' #
#' #   # Extract unique states for iteration
#' #   states <- unique(df$state)
#' #
#' #   # Generate sentences for each state
#' #   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#' #
#' #     # Use helper function to filter data by state and year
#' #     filtered_data <- fnc_filter_data_by_state_year(df, state_var)
#' #     df1 <- filtered_data$data
#' #     year <- filtered_data$year
#' #
#' #     # Handle missing data for the state
#' #     if (nrow(df1) == 0) {
#' #       return(paste0("No data available for ", state_var))
#' #     }
#' #
#' #     # --- Handle Sex Comparison ---
#' #     if (compare_var == "sex") {
#' #       # Generate sentence using helper function for sex comparisons
#' #       return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
#' #
#' #     } else if (compare_var == "race") {
#' #
#' #       # --- Handle Race Comparison ---
#' #       # Standardize race categories for consistency
#' #       df1 <- df1 |>
#' #         dplyr::mutate(race = dplyr::case_when(
#' #           race == "White, non-Hispanic" ~ "White",
#' #           race == "Black, non-Hispanic" ~ "Black",
#' #           race == "Hispanic, any race" ~ "Hispanic",
#' #           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#' #         ))
#' #
#' #       # Extract data for White individuals as the comparison group
#' #       df_white <- df1 |> dplyr::filter(race == "White")
#' #
#' #       # Initialize sentences for each comparison group
#' #       black_sentence <- ""
#' #       hispanic_sentence <- ""
#' #       other_sentence <- ""
#' #
#' #       # --- Black vs White Comparison ---
#' #       df_black <- df1 |> dplyr::filter(race == "Black")
#' #       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#' #         los_diff_black <- round(df_black[[los_col]], 1) - round(df_white[[los_col]], 1)
#' #         abs_los_diff_black <- abs(round(los_diff_black, 1))
#' #
#' #         if (!is.na(los_diff_black)) {
#' #           black_sentence <- if (los_diff_black > 0) {
#' #             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_black, " more years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           } else {
#' #             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_black, " less years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # --- Hispanic vs White Comparison ---
#' #       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#' #       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#' #         los_diff_hispanic <- round(df_hispanic[[los_col]], 1) - round(df_white[[los_col]], 1)
#' #         abs_los_diff_hispanic <- abs(round(los_diff_hispanic, 1))
#' #
#' #         if (!is.na(los_diff_hispanic)) {
#' #           hispanic_sentence <- if (los_diff_hispanic > 0) {
#' #             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_hispanic, " more years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           } else {
#' #             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_hispanic, " less years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # --- Other Races vs White Comparison ---
#' #       df_other <- df1 |> dplyr::filter(race == "non-Hispanic people of other races")
#' #       if (nrow(df_other) > 0 && nrow(df_white) > 0) {
#' #         los_diff_other <- round(df_other[[los_col]], 1) - round(df_white[[los_col]], 1)
#' #         abs_los_diff_other <- abs(round(los_diff_other, 1))
#' #
#' #         if (!is.na(los_diff_other)) {
#' #           other_sentence <- if (los_diff_other > 0) {
#' #             paste0("non-Hispanic people of other races ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_other, " more years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           } else {
#' #             paste0("non-Hispanic people of other races ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", abs_los_diff_other, " less years ",
#' #                    if (type == "in prison") "in prison" else "past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # Combine sentences into a single statement
#' #       sentences <- c(black_sentence, hispanic_sentence, other_sentence)
#' #       sentences <- sentences[sentences != ""]
#' #       if (length(sentences) > 0) {
#' #         # return(paste0("In ", year, ", ", paste(sentences, collapse = ", and "), " compared to White people."))
#' #         return(paste0(paste(sentences, collapse = ", and "), " compared to White people."))
#' #       } else {
#' #         return("No significant differences in average years spent compared to White people.")
#' #       }
#' #
#' #     } else {
#' #       # Handle invalid comparison variable input
#' #       return("Invalid comparison variable.")
#' #     }
#' #   })
#' #
#' #   # Assign state names to the list of generated sentences
#' #   all_sentences <- setNames(all_sentences, states)
#' #
#' #   return(all_sentences)
#' # }
#' # fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#' #
#' #   # Extract unique states for iteration
#' #   states <- unique(df$state)
#' #
#' #   # Generate sentences for each state
#' #   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#' #
#' #     # Use helper function to filter data by state and year
#' #     filtered_data <- fnc_filter_data_by_state_year(df, state_var)
#' #     df1 <- filtered_data$data
#' #     year <- filtered_data$year
#' #
#' #     # Handle missing data for the state
#' #     if (nrow(df1) == 0) {
#' #       return(paste0("No data available for ", state_var))
#' #     }
#' #
#' #     if (compare_var == "sex") {
#' #       # Generate sentence using helper function for sex comparisons
#' #       return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
#' #
#' #     } else if (compare_var == "race") {
#' #       # Standardize race categories for consistency
#' #       df1 <- df1 |>
#' #         dplyr::mutate(race = dplyr::case_when(
#' #           race == "White, non-Hispanic" ~ "White",
#' #           race == "Black, non-Hispanic" ~ "Black",
#' #           race == "Hispanic, any race" ~ "Hispanic",
#' #           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#' #         ))
#' #
#' #       # Extract data for White individuals as the comparison group
#' #       df_white <- df1 |> dplyr::filter(race == "White")
#' #
#' #       # Initialize sentences for each comparison group
#' #       black_sentence <- ""
#' #       hispanic_sentence <- ""
#' #       other_sentence <- ""
#' #
#' #       # --- Black vs White Comparison ---
#' #       df_black <- df1 |> dplyr::filter(race == "Black")
#' #       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#' #         los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
#' #         abs_los_diff_black <- abs(los_diff_black)
#' #
#' #         # Convert to months if difference is less than 1 year
#' #         if (!is.na(los_diff_black)) {
#' #           black_sentence <- if (los_diff_black > 0) {
#' #             time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
#' #             time_unit <- if (abs_los_diff_black < 1) "months" else "years"
#' #             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", time_value, " more ", time_unit,
#' #                    if (type == "in prison") " in prison" else " past parole eligibility")
#' #           } else {
#' #             time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
#' #             time_unit <- if (abs_los_diff_black < 1) "months" else "years"
#' #             paste0("Black people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", time_value, " less ", time_unit,
#' #                    if (type == "in prison") " in prison" else " past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # --- Hispanic vs White Comparison ---
#' #       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#' #       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#' #         los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
#' #         abs_los_diff_hispanic <- abs(los_diff_hispanic)
#' #
#' #         # Convert to months if difference is less than 1 year
#' #         if (!is.na(los_diff_hispanic)) {
#' #           hispanic_sentence <- if (los_diff_hispanic > 0) {
#' #             time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
#' #             time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
#' #             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", time_value, " more ", time_unit,
#' #                    if (type == "in prison") " in prison" else " past parole eligibility")
#' #           } else {
#' #             time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
#' #             time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
#' #             paste0("Hispanic people ", if (type == "in prison") "released" else "still incarcerated",
#' #                    " spent on average ", time_value, " less ", time_unit,
#' #                    if (type == "in prison") " in prison" else " past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # Combine sentences into a single statement
#' #       sentences <- c(black_sentence, hispanic_sentence, other_sentence)
#' #       sentences <- sentences[sentences != ""]
#' #       if (length(sentences) > 0) {
#' #         return(paste0(paste(sentences, collapse = ", and "), " compared to White people."))
#' #       } else {
#' #         return("No significant differences in average years spent compared to White people.")
#' #       }
#' #
#' #     } else {
#' #       return("Invalid comparison variable.")
#' #     }
#' #   })
#' #
#' #   # Assign state names to the list of generated sentences
#' #   all_sentences <- setNames(all_sentences, states)
#' #
#' #   return(all_sentences)
#' # }
#' # fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#' #
#' #   # Extract unique states for iteration
#' #   states <- unique(df$state)
#' #
#' #   # Generate sentences for each state
#' #   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#' #
#' #     # Use helper function to filter data by state and year
#' #     filtered_data <- fnc_filter_data_by_state_year(df, state_var)
#' #     df1 <- filtered_data$data
#' #     year <- filtered_data$year
#' #
#' #     # Handle missing data for the state
#' #     if (nrow(df1) == 0) {
#' #       return(paste0("No data available for ", state_var))
#' #     }
#' #
#' #     if (compare_var == "sex") {
#' #       # Generate sentence using helper function for sex comparisons
#' #       return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
#' #
#' #     } else if (compare_var == "race") {
#' #       # Standardize race categories for consistency
#' #       df1 <- df1 |>
#' #         dplyr::mutate(race = dplyr::case_when(
#' #           race == "White, non-Hispanic" ~ "White",
#' #           race == "Black, non-Hispanic" ~ "Black",
#' #           race == "Hispanic, any race" ~ "Hispanic",
#' #           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#' #         ))
#' #
#' #       # Extract data for White individuals as the comparison group
#' #       df_white <- df1 |> dplyr::filter(race == "White")
#' #
#' #       # Initialize sentences and flags for each comparison group
#' #       black_sentence <- ""
#' #       hispanic_sentence <- ""
#' #       overall_summary <- ""
#' #       groups_more <- c()
#' #       groups_less <- c()
#' #
#' #       # --- Black vs White Comparison ---
#' #       df_black <- df1 |> dplyr::filter(race == "Black")
#' #       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#' #         los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
#' #         abs_los_diff_black <- abs(los_diff_black)
#' #
#' #         if (!is.na(los_diff_black)) {
#' #           time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
#' #           time_unit <- if (abs_los_diff_black < 1) "months" else "years"
#' #           black_sentence <- if (los_diff_black > 0) {
#' #             groups_more <- c(groups_more, "Black people")
#' #             paste0("Black people still incarcerated spent on average ", time_value, " more ", time_unit, " past parole eligibility")
#' #           } else {
#' #             groups_less <- c(groups_less, "Black people")
#' #             paste0("Black people still incarcerated spent on average ", time_value, " less ", time_unit, " past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # --- Hispanic vs White Comparison ---
#' #       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#' #       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#' #         los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
#' #         abs_los_diff_hispanic <- abs(los_diff_hispanic)
#' #
#' #         if (!is.na(los_diff_hispanic)) {
#' #           time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
#' #           time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
#' #           hispanic_sentence <- if (los_diff_hispanic > 0) {
#' #             groups_more <- c(groups_more, "Hispanic people")
#' #             paste0("Hispanic people still incarcerated spent on average ", time_value, " more ", time_unit, " past parole eligibility")
#' #           } else {
#' #             groups_less <- c(groups_less, "Hispanic people")
#' #             paste0("Hispanic people still incarcerated spent on average ", time_value, " less ", time_unit, " past parole eligibility")
#' #           }
#' #         }
#' #       }
#' #
#' #       # Construct overall summary
#' #       if (length(groups_more) > 0) {
#' #         overall_summary <- paste0(paste(groups_more, collapse = " and "), " spend more time behind bars than White people.")
#' #       }
#' #       if (length(groups_less) > 0) {
#' #         if (overall_summary != "") {
#' #           overall_summary <- paste0(overall_summary, " ")
#' #         }
#' #         overall_summary <- paste0(overall_summary, paste(groups_less, collapse = " and "), " spend less time behind bars than White people.")
#' #       }
#' #
#' #       # Combine sentences into a single statement
#' #       sentences <- c(black_sentence, hispanic_sentence)
#' #       sentences <- sentences[sentences != ""]
#' #       if (length(sentences) > 0) {
#' #         final_sentence <- paste0(overall_summary, " ", paste(sentences, collapse = ", and "), " compared to White people.")
#' #         # Ensure proper grammar (e.g., no `. and`)
#' #         final_sentence <- gsub("\\. and", " and", final_sentence)
#' #         return(final_sentence)
#' #       } else {
#' #         return("No significant differences in average years spent compared to White people.")
#' #       }
#' #
#' #     } else {
#' #       return("Invalid comparison variable.")
#' #     }
#' #   })
#' #
#' #   # Assign state names to the list of generated sentences
#' #   all_sentences <- setNames(all_sentences, states)
#' #
#' #   return(all_sentences)
#' # }
#' fnc_generate_disparity_sentences <- function(df, type, compare_var, los_col) {
#'
#'   # Extract unique states for iteration
#'   states <- unique(df$state)
#'
#'   # Generate sentences for each state
#'   all_sentences <- purrr::map(.x = states, .f = function(state_var) {
#'
#'     # Use helper function to filter data by state and year
#'     filtered_data <- fnc_filter_data_by_state_year(df, state_var)
#'     df1 <- filtered_data$data
#'     year <- filtered_data$year
#'
#'     # Handle missing data for the state
#'     if (nrow(df1) == 0) {
#'       return(paste0("No data available for ", state_var))
#'     }
#'
#'     if (compare_var == "sex") {
#'       # Generate sentence using helper function for sex comparisons
#'       return(fnc_generate_sentence_sex(df1, year, type, los_col, state_var))
#'
#'     } else if (compare_var == "race") {
#'       # Standardize race categories for consistency
#'       df1 <- df1 |>
#'         dplyr::mutate(race = dplyr::case_when(
#'           race == "White, non-Hispanic" ~ "White",
#'           race == "Black, non-Hispanic" ~ "Black",
#'           race == "Hispanic, any race" ~ "Hispanic",
#'           race == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races"
#'         ))
#'
#'       # Extract data for White individuals as the comparison group
#'       df_white <- df1 |> dplyr::filter(race == "White")
#'
#'       # Initialize sentences and flags for each comparison group
#'       black_sentence <- ""
#'       hispanic_sentence <- ""
#'       overall_summary <- ""
#'       groups_more <- c()
#'       groups_less <- c()
#'
#'       # Define phrasing based on `type`
#'       if (type == "in prison") {
#'         summary_phrase <- "spend more time behind bars than White people"
#'         less_phrase <- "spend less time behind bars than White people"
#'         detail_suffix <- "in prison"
#'       } else if (type == "past parole eligibility") {
#'         summary_phrase <- "spend more time behind bars after becoming eligible for parole than White people"
#'         less_phrase <- "spend less time behind bars after becoming eligible for parole than White people"
#'         detail_suffix <- "past parole eligibility"
#'       }
#'
#'       # --- Black vs White Comparison ---
#'       df_black <- df1 |> dplyr::filter(race == "Black")
#'       if (nrow(df_black) > 0 && nrow(df_white) > 0) {
#'         los_diff_black <- df_black[[los_col]] - df_white[[los_col]]
#'         abs_los_diff_black <- abs(los_diff_black)
#'
#'         if (!is.na(los_diff_black)) {
#'           time_value <- if (abs_los_diff_black < 1) round(abs_los_diff_black * 12) else round(abs_los_diff_black, 1)
#'           time_unit <- if (abs_los_diff_black < 1) "months" else "years"
#'           black_sentence <- if (los_diff_black > 0) {
#'             groups_more <- c(groups_more, "Black people")
#'             paste0("Black people spent on average ", time_value, " more ", time_unit, " ", detail_suffix)
#'           } else {
#'             groups_less <- c(groups_less, "Black people")
#'             paste0("Black people spent on average ", time_value, " less ", time_unit, " ", detail_suffix)
#'           }
#'         }
#'       }
#'
#'       # --- Hispanic vs White Comparison ---
#'       df_hispanic <- df1 |> dplyr::filter(race == "Hispanic")
#'       if (nrow(df_hispanic) > 0 && nrow(df_white) > 0) {
#'         los_diff_hispanic <- df_hispanic[[los_col]] - df_white[[los_col]]
#'         abs_los_diff_hispanic <- abs(los_diff_hispanic)
#'
#'         if (!is.na(los_diff_hispanic)) {
#'           time_value <- if (abs_los_diff_hispanic < 1) round(abs_los_diff_hispanic * 12) else round(abs_los_diff_hispanic, 1)
#'           time_unit <- if (abs_los_diff_hispanic < 1) "months" else "years"
#'           hispanic_sentence <- if (los_diff_hispanic > 0) {
#'             groups_more <- c(groups_more, "Hispanic people")
#'             paste0("Hispanic people spent on average ", time_value, " more ", time_unit, " ", detail_suffix)
#'           } else {
#'             groups_less <- c(groups_less, "Hispanic people")
#'             paste0("Hispanic people spent on average ", time_value, " less ", time_unit, " ", detail_suffix)
#'           }
#'         }
#'       }
#'
#'       # Construct overall summary
#'       if (length(groups_more) > 0) {
#'         overall_summary <- paste0(paste(groups_more, collapse = " and "), " ", summary_phrase, ".")
#'       }
#'       if (length(groups_less) > 0) {
#'         if (overall_summary != "") {
#'           overall_summary <- paste0(overall_summary, " ")
#'         }
#'         overall_summary <- paste0(overall_summary, paste(groups_less, collapse = " and "), " ", less_phrase, ".")
#'       }
#'
#'       # Combine sentences into a single statement
#'       sentences <- c(black_sentence, hispanic_sentence)
#'       sentences <- sentences[sentences != ""]
#'       if (length(sentences) > 0) {
#'         final_sentence <- paste0(overall_summary, " ", paste(sentences, collapse = ", and "), " compared to White people.")
#'         # Ensure proper grammar (e.g., no `. and`)
#'         final_sentence <- gsub("\\. and", " and", final_sentence)
#'         return(final_sentence)
#'       } else {
#'         return("No significant differences in average years spent compared to White people.")
#'       }
#'
#'     } else {
#'       return("Invalid comparison variable.")
#'     }
#'   })
#'
#'   # Assign state names to the list of generated sentences
#'   all_sentences <- setNames(all_sentences, states)
#'
#'   return(all_sentences)
#' }
#'
#'
#' #' Generate Disparity Sentence for Sex Comparison
#' #'
#' #' This function generates a sentence comparing the average years spent
#' #' (either in prison or past parole eligibility) between females and males
#' #' for a given state and year.
#' #'
#' #' @param df1 A filtered data frame containing `sex` and the specified column
#' #'   (`los_col`) for length of stay (LOS) comparisons.
#' #' @param year An integer representing the reporting year for the comparison.
#' #' @param type A string indicating the context of the comparison:
#' #'   `"in prison"` or `"past parole eligibility"`.
#' #' @param los_col A string specifying the column name in `df1` that contains
#' #'   the average length of stay data.
#' #' @param state_var A string representing the name of the state for the analysis.
#' #' @return A string summarizing the disparity in average years spent between
#' #'   females and males for the specified state and year.
#' #' @examples
#' #' sentence <- fnc_generate_sentence_sex(filtered_data, 2022, "in prison", "average_los", "Georgia")
#' #' print(sentence)
#' #' @export
#' # fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
#' #   # Filter the data for males
#' #   df_male <- df1 |> dplyr::filter(sex == "Male")
#' #
#' #   # Initialize an empty sentence variable
#' #   sentence <- ""
#' #
#' #   # Filter the data for females
#' #   df_female <- df1 |> dplyr::filter(sex == "Female")
#' #
#' #   # Check if both male and female data exist
#' #   if (nrow(df_female) > 0 && nrow(df_male) > 0) {
#' #     # Calculate the difference in length of stay (LOS) between females and males
#' #     los_diff_female <- round(df_female[[los_col]], 1) - round(df_male[[los_col]], 1)
#' #     abs_los_diff_female <- abs(los_diff_female)
#' #
#' #     # Ensure the LOS difference is not NA
#' #     if (!is.na(los_diff_female)) {
#' #       if (los_diff_female > 0) {
#' #         # Females spent more years on average
#' #         sentence <- paste0(
#' #           # "In ", year, ", females ",
#' #           "Females ",
#' #           if (type == "in prison") "released" else "who were still incarcerated",
#' #           " spent on average ", abs_los_diff_female,
#' #           if (abs_los_diff_female == 1) " more year" else " more years",
#' #           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#' #           " compared to males in ", state_var, "."
#' #         )
#' #       } else if (los_diff_female < 0) {
#' #         # Females spent fewer years on average
#' #         sentence <- paste0(
#' #           # "In ", year, ", females ",
#' #           "Females ",
#' #           if (type == "in prison") "released" else "who were still incarcerated",
#' #           " spent on average ", abs_los_diff_female,
#' #           if (abs_los_diff_female == 1) " less year" else " less years",
#' #           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#' #           " compared to males in ", state_var, "."
#' #         )
#' #       }
#' #     }
#' #   }
#' #
#' #   # Handle cases where no meaningful disparity exists or data is missing
#' #   if (sentence != "") {
#' #     return(sentence)  # Return the constructed sentence if disparity is found
#' #   } else {
#' #     return(paste0(
#' #       # "In ", year, ", females and males spent the same average number of years ",
#' #       "Females and males spent the same average number of years ",
#' #       if (type == "in prison") "in prison." else "past parole eligibility."
#' #     ))
#' #   }
#' # }
#' # fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
#' #   # Filter the data for males
#' #   df_male <- df1 |> dplyr::filter(sex == "Male")
#' #
#' #   # Initialize an empty sentence variable
#' #   sentence <- ""
#' #
#' #   # Filter the data for females
#' #   df_female <- df1 |> dplyr::filter(sex == "Female")
#' #
#' #   # Check if both male and female data exist
#' #   if (nrow(df_female) > 0 && nrow(df_male) > 0) {
#' #     # Calculate the difference in length of stay (LOS) between females and males
#' #     los_diff_female <- df_female[[los_col]] - df_male[[los_col]]
#' #     abs_los_diff_female <- abs(los_diff_female)
#' #
#' #     # Ensure the LOS difference is not NA
#' #     if (!is.na(los_diff_female)) {
#' #       if (los_diff_female > 0) {
#' #         # Females spent more time on average
#' #         time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
#' #         time_unit <- if (abs_los_diff_female < 1) "months" else "years"
#' #         sentence <- paste0(
#' #           "Females ",
#' #           if (type == "in prison") "released" else "who were still incarcerated",
#' #           " spent on average ", time_value, " more ", time_unit,
#' #           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#' #           " compared to males in ", state_var, "."
#' #         )
#' #       } else if (los_diff_female < 0) {
#' #         # Females spent less time on average
#' #         time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
#' #         time_unit <- if (abs_los_diff_female < 1) "months" else "years"
#' #         sentence <- paste0(
#' #           "Females ",
#' #           if (type == "in prison") "released" else "who were still incarcerated",
#' #           " spent on average ", time_value, " less ", time_unit,
#' #           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#' #           " compared to males in ", state_var, "."
#' #         )
#' #       }
#' #     }
#' #   }
#' #
#' #   # Handle cases where no meaningful disparity exists or data is missing
#' #   if (sentence != "") {
#' #     return(sentence)  # Return the constructed sentence if disparity is found
#' #   } else {
#' #     return(paste0(
#' #       "Females and males spent the same average number of years ",
#' #       if (type == "in prison") "in prison." else "past parole eligibility."
#' #     ))
#' #   }
#' # }
#' fnc_generate_sentence_sex <- function(df1, year, type, los_col, state_var) {
#'   # Filter the data for males
#'   df_male <- df1 |> dplyr::filter(sex == "Male")
#'
#'   # Initialize an empty sentence variable
#'   sentence <- ""
#'
#'   # Filter the data for females
#'   df_female <- df1 |> dplyr::filter(sex == "Female")
#'
#'   # Check if both male and female data exist
#'   if (nrow(df_female) > 0 && nrow(df_male) > 0) {
#'     # Calculate the difference in length of stay (LOS) between females and males
#'     los_diff_female <- df_female[[los_col]] - df_male[[los_col]]
#'     abs_los_diff_female <- abs(los_diff_female)
#'
#'     # Ensure the LOS difference is not NA
#'     if (!is.na(los_diff_female)) {
#'       if (los_diff_female > 0) {
#'         # Females spent more time on average
#'         time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
#'         time_unit <- if (abs_los_diff_female < 1) "months" else "years"
#'         sentence <- paste0(
#'           "Females ",
#'           if (type == "in prison") "released" else "who were still incarcerated",
#'           " spent on average ", time_value, " more ", time_unit,
#'           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#'           " compared to males."
#'         )
#'       } else if (los_diff_female < 0) {
#'         # Females spent less time on average
#'         time_value <- if (abs_los_diff_female < 1) round(abs_los_diff_female * 12) else round(abs_los_diff_female, 1)
#'         time_unit <- if (abs_los_diff_female < 1) "months" else "years"
#'         sentence <- paste0(
#'           "Females ",
#'           if (type == "in prison") "released" else "who were still incarcerated",
#'           " spent on average ", time_value, " less ", time_unit,
#'           " ", if (type == "in prison") "in prison" else "past parole eligibility",
#'           " compared to males."
#'         )
#'       }
#'     }
#'   }
#'
#'   # Handle cases where no meaningful disparity exists or data is missing
#'   if (sentence != "") {
#'     return(sentence)  # Return the constructed sentence if disparity is found
#'   } else {
#'     return(paste0(
#'       "Females and males spent the same average number of years ",
#'       if (type == "in prison") "in prison." else "past parole eligibility."
#'     ))
#'   }
#' }
#'
#' #' Generate Offense-Specific Disparity Sentences
#' #'
#' #' This function analyzes disparities in average time served or time spent past parole eligibility
#' #' for each offense type by grouping variable (e.g., race or sex) within states and generates
#' #' descriptive sentences highlighting the largest disparities.
#' #'
#' #' @param data A data frame containing information on offense types, grouping variables
#' #'   (e.g., race or sex), and time measures (e.g., average time served).
#' #' @param grouping_var A string specifying the grouping variable, either `"race"` or `"sex"`.
#' #' @param time_var A string specifying the measure variable, such as `"average_los"` (average time served).
#' #' @return A named list of descriptive disparity sentences, with each element corresponding to a state.
#' #' @export
#' # fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los") {
#' #
#' #   # Extract unique states to iterate over
#' #   states <- unique(data$state)
#' #
#' #   # Generate sentences for each state
#' #   all_sentences <- purrr::map(.x = states, .f = function(x) {
#' #
#' #     # Filter data for the specified state and exclude unspecified offense types
#' #     df1 <- data |>
#' #       dplyr::filter(state == x & fbi_index != "Other or Unspecified")
#' #
#' #     # Extract the year for this state's data
#' #     year <- unique(df1$rptyear)
#' #
#' #     # Handle missing data: If no data exists for the state, return a message
#' #     if (nrow(df1) == 0) {
#' #       return(paste0("No data available for ", x))
#' #     }
#' #
#' #     # Calculate disparities between groups for each offense type
#' #     df_disparity <- df1 |>
#' #       dplyr::group_by(fbi_index) |>
#' #       dplyr::reframe(
#' #         max_los = max(!!rlang::sym(time_var)),               # Maximum value of time_var
#' #         min_los = min(!!rlang::sym(time_var)),               # Minimum value of time_var
#' #         diff_los = max_los - min_los,                        # Difference between max and min
#' #         group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],  # Group with max value
#' #         group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]  # Group with min value
#' #       ) |>
#' #       dplyr::arrange(dplyr::desc(diff_los))                 # Sort by largest disparities
#' #
#' #     # Standardize group labels for race or sex
#' #     if (grouping_var == "race") {
#' #       df_disparity <- df_disparity |>
#' #         dplyr::mutate(
#' #           group_longest = dplyr::case_when(
#' #             group_longest == "Black, non-Hispanic" ~ "Black",
#' #             group_longest == "White, non-Hispanic" ~ "White",
#' #             group_longest == "Hispanic, any race" ~ "Hispanic",
#' #             group_longest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#' #             TRUE ~ group_longest
#' #           ),
#' #           group_shortest = dplyr::case_when(
#' #             group_shortest == "Black, non-Hispanic" ~ "Black",
#' #             group_shortest == "White, non-Hispanic" ~ "White",
#' #             group_shortest == "Hispanic, any race" ~ "Hispanic",
#' #             group_shortest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#' #             TRUE ~ group_shortest
#' #           )
#' #         )
#' #     }
#' #
#' #     # Focus on relevant comparisons: e.g., Black, Hispanic vs. White (for race) or Male vs. Female (for sex)
#' #     if (grouping_var == "race") {
#' #       df_disparity_filtered <- df_disparity |>
#' #         dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic", "non-Hispanic people of other races"))
#' #     } else {
#' #       df_disparity_filtered <- df_disparity |>
#' #         dplyr::filter(group_shortest == "Female" & group_longest == "Male") |>
#' #         dplyr::mutate(
#' #           group_longest = "males",
#' #           group_shortest = "females"
#' #         )
#' #     }
#' #
#' #     # Handle cases with no significant disparities
#' #     if (nrow(df_disparity_filtered) == 0) {
#' #       time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
#' #       return(paste0("The chart below shows the average ", time_description, " by offense type and ",
#' #                     ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."))
#' #     }
#' #
#' #     # Exclude "Other Violent Offenses" if it's the largest disparity (and there are other offenses)
#' #     if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
#' #       df_disparity_filtered <- df_disparity_filtered |> dplyr::slice(2)
#' #     }
#' #
#' #     # Extract details for the largest disparity
#' #     largest_disparity <- df_disparity_filtered |> dplyr::slice(1)
#' #     offense_type <- largest_disparity$fbi_index
#' #     group_longest <- largest_disparity$group_longest
#' #     disparity_diff <- round(largest_disparity$diff_los, 1)
#' #     group_shortest <- largest_disparity$group_shortest
#' #
#' #     # Construct the descriptive sentence
#' #     time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
#' #     sentence <- paste0(
#' #       "The chart below shows the average ", time_description, " by offense type and ",
#' #       ifelse(grouping_var == "race", "race and ethnicity", grouping_var), ". ",
#' #       "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
#' #       group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
#' #       " spent on average ", disparity_diff, " more years in prison compared to ",
#' #       group_shortest, if (grouping_var == "race") " people" else "", "."
#' #     )
#' #
#' #     return(sentence)
#' #   })
#' #
#' #   # Assign state names to the resulting list
#' #   all_sentences <- setNames(all_sentences, states)
#' #
#' #   return(all_sentences)
#' # }
#' fnc_generate_offense_disparity_sentence <- function(data, grouping_var = "race", time_var = "average_los") {
#'
#'   # Extract unique states to iterate over
#'   states <- unique(data$state)
#'
#'   # Generate sentences for each state
#'   all_sentences <- purrr::map(.x = states, .f = function(x) {
#'
#'     # Filter data for the specified state and exclude unspecified offense types
#'     df1 <- data |>
#'       dplyr::filter(state == x & fbi_index != "Other or Unspecified")
#'
#'     # Extract the year for this state's data
#'     year <- unique(df1$rptyear)
#'
#'     # Handle missing data: If no data exists for the state, return a message
#'     if (nrow(df1) == 0) {
#'       return(paste0("No data available for ", x))
#'     }
#'
#'     # Calculate disparities between groups for each offense type
#'     df_disparity <- df1 |>
#'       dplyr::group_by(fbi_index) |>
#'       dplyr::reframe(
#'         max_los = max(!!rlang::sym(time_var)),               # Maximum value of time_var
#'         min_los = min(!!rlang::sym(time_var)),               # Minimum value of time_var
#'         diff_los = max_los - min_los,                        # Difference between max and min
#'         group_longest = .data[[grouping_var]][which.max(!!rlang::sym(time_var))],  # Group with max value
#'         group_shortest = .data[[grouping_var]][which.min(!!rlang::sym(time_var))]  # Group with min value
#'       ) |>
#'       dplyr::arrange(dplyr::desc(diff_los))                 # Sort by largest disparities
#'
#'     # Standardize group labels for race or sex
#'     if (grouping_var == "race") {
#'       df_disparity <- df_disparity |>
#'         dplyr::mutate(
#'           group_longest = dplyr::case_when(
#'             group_longest == "Black, non-Hispanic" ~ "Black",
#'             group_longest == "White, non-Hispanic" ~ "White",
#'             group_longest == "Hispanic, any race" ~ "Hispanic",
#'             group_longest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#'             TRUE ~ group_longest
#'           ),
#'           group_shortest = dplyr::case_when(
#'             group_shortest == "Black, non-Hispanic" ~ "Black",
#'             group_shortest == "White, non-Hispanic" ~ "White",
#'             group_shortest == "Hispanic, any race" ~ "Hispanic",
#'             group_shortest == "Other race(s), non-Hispanic" ~ "non-Hispanic people of other races",
#'             TRUE ~ group_shortest
#'           )
#'         )
#'     }
#'
#'     # Focus on relevant comparisons: e.g., Black, Hispanic vs. White (for race) or Male vs. Female (for sex)
#'     if (grouping_var == "race") {
#'       df_disparity_filtered <- df_disparity |>
#'         dplyr::filter(group_shortest == "White" & group_longest %in% c("Black", "Hispanic", "non-Hispanic people of other races"))
#'     } else {
#'       df_disparity_filtered <- df_disparity |>
#'         dplyr::filter(group_shortest == "Female" & group_longest == "Male") |>
#'         dplyr::mutate(
#'           group_longest = "males",
#'           group_shortest = "females"
#'         )
#'     }
#'
#'     # Handle cases with no significant disparities
#'     if (nrow(df_disparity_filtered) == 0) {
#'       time_description <- ifelse(time_var == "time_served", "time served in prison", "time spent in prison past parole eligibility")
#'       return(paste0("The chart below shows the average ", time_description, " by offense type and ",
#'                     ifelse(grouping_var == "race", "race and ethnicity", grouping_var), "."))
#'     }
#'
#'     # Exclude "Other Violent Offenses" if it's the largest disparity (and there are other offenses)
#'     if (df_disparity_filtered$fbi_index[1] == "Other Violent Offenses" & nrow(df_disparity_filtered) > 1) {
#'       df_disparity_filtered <- df_disparity_filtered |> dplyr::slice(2)
#'     }
#'
#'     # Extract details for the largest disparity
#'     largest_disparity <- df_disparity_filtered |> dplyr::slice(1)
#'     offense_type <- largest_disparity$fbi_index
#'     group_longest <- largest_disparity$group_longest
#'     group_shortest <- largest_disparity$group_shortest
#'     disparity_diff <- largest_disparity$diff_los
#'
#'     # Adjust for months if disparity is less than 1 year
#'     time_value <- if (disparity_diff < 1) round(disparity_diff * 12) else round(disparity_diff, 1)
#'     time_unit <- if (disparity_diff < 1) "months" else "years"
#'
#'     # Construct the descriptive sentence
#'     time_description <- ifelse(time_var == "average_los", "time served in prison", "time spent in prison past parole eligibility")
#'     sentence <- paste0(
#'       "The chart below shows the average ", time_description, " by offense type and ",
#'       ifelse(grouping_var == "race", "race and ethnicity", grouping_var), ". ",
#'       "The largest disparity was observed among ", tolower(offense_type), " offenses, where ",
#'       group_longest, if (grouping_var == "race" && group_longest != "White") " people" else "",
#'       " spent on average ", time_value, " more ", time_unit, " in prison compared to ",
#'       group_shortest, if (grouping_var == "race") " people" else "", "."
#'     )
#'
#'     return(sentence)
#'   })
#'
#'   # Assign state names to the resulting list
#'   all_sentences <- setNames(all_sentences, states)
#'
#'   return(all_sentences)
#' }
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # RRI Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Calculate Relative Rate Index (RRI) for Groups
#' #'
#' #' This function calculates the Relative Rate Index (RRI) for a specified
#' #' category (e.g., race or sex) compared to a reference group.
#' #'
#' #' @param data A data frame containing the data, including `state`, `past_pe_rate`,
#' #'   and the category of interest (e.g., race or sex).
#' #' @param comparison_group A string specifying the reference group for comparison
#' #'   (e.g., "White people" or "females").
#' #' @param category A string indicating the column name for the category of interest
#' #'   (e.g., "race" or "sex").
#' #' @return A data frame containing the state, category, and calculated RRI.
#' #' @export
#' fnc_calculate_rri <- function(data, comparison_group, category) {
#'   # Calculate reference rate for the comparison group
#'   reference_rate_data <- data |>
#'     filter(!!sym(category) == comparison_group) |>
#'     select(state, rptyear, past_pe_rate) |>  # Include rptyear in the selection
#'     rename(reference_past_pe_rate = past_pe_rate)  # Rename rate for clarity
#'
#'   # Calculate RRI for all groups
#'   rri_data <- data |>
#'     inner_join(reference_rate_data, by = c("state", "rptyear")) |>  # Join by state and rptyear
#'     mutate(rri = round(past_pe_rate / reference_past_pe_rate, 1)) |>  # Calculate RRI
#'     select(state, rptyear, !!sym(category), rri)  # Keep rptyear in the output
#'
#'   return(rri_data)
#' }
#'
#' #' Generate RRI Sentences for Disparities
#' #'
#' #' This function generates HTML-formatted sentences describing disparities in
#' #' incarceration rates past parole eligibility for a given category (e.g., race or sex)
#' #' compared to a reference group.
#' #'
#' #' @param data A data frame containing the calculated RRI values for each group,
#' #'   including `state`, `category`, and `rri`.
#' #' @param category A string indicating the column name for the category of interest
#' #'   (e.g., "race" or "sex").
#' #' @param label A string specifying the label for the group of interest (e.g., "Black people").
#' #' @param color A string indicating the HTML color code for styling the group label in the sentence.
#' #' @return A named list of HTML-formatted sentences for each state.
#' #' @export
#' fnc_generate_rri_sentences <- function(data, category, label, color) {
#'   # Define comparison group and color based on category
#'   comparison_group <- if (category == "race") "White people" else "females"
#'   comparison_color <- if (category == "race") red else purple
#'
#'   # Iterate over each state to generate sentences
#'   sentences <- map(unique(data$state), function(state_name) {
#'     # Filter data for the specific state and category label
#'     df1 <- data |> filter(state == state_name, !!sym(category) == label)
#'
#'     # Handle missing data
#'     if (nrow(df1) == 0 || is.na(df1$rri)) return("")
#'
#'     # Extract RRI value
#'     rri <- df1$rri
#'
#'     # Ensure "label" is lowercase if it matches "Male"
#'     # label <- if (label == "Male") "males" else label
#'     # Adjust the label for specific cases
#'     label <- case_when(
#'       label == "Hispanic, any race" ~ "Hispanic people",
#'       label == "Black, non-Hispanic" ~ "Black, non-Hispanic people",
#'       TRUE ~ label
#'     )
#'
#'     # Generate sentence for RRI > 1 (higher disparity)
#'     if (rri > 1) {
#'       paste0(
#'         # "In ", df1$rptyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
#'         "<span style='color:", color, "; font-weight:bold;'>", label,
#'         "</span> were incarcerated in state prison past parole eligibility at a rate <span style='color:",
#'         color, "; font-weight:bold;'>", rri, " times higher</span> than <span style='color:",
#'         comparison_color, "; font-weight:bold;'>", comparison_group,
#'         "</span>, when accounting for prison population sizes in ", state_name, "."
#'       )
#'     } else {  # Generate sentence for RRI <= 1 (lower disparity)
#'       percent_less <- round((1 - rri) * 100, 0)
#'       paste0(
#'         # "In ", df1$rptyear, ", <span style='color:", color, "; font-weight:bold;'>", label,
#'         "<span style='color:", color, "; font-weight:bold;'>", label,
#'         "</span> were <span style='color:", color, "; font-weight:bold;'>", percent_less,
#'         " percent less likely</span> to be incarcerated in state prison past parole eligibility compared to <span style='color:",
#'         comparison_color, "; font-weight:bold;'>", comparison_group,
#'         "</span>, when accounting for population sizes in ", state_name, "."
#'       )
#'     }
#'   })
#'
#'   # Assign state names to the list of sentences
#'   sentences <- setNames(sentences, unique(data$state))
#'
#'   return(sentences)
#' }
#'
#'
#' # ---------------------------------------------------------------------------- #
#' # People Infographic Helper Functions
#' # ---------------------------------------------------------------------------- #
#'
#' #' Create and Save State-Specific Infographic
#' #'
#' #' This function generates and saves state-specific infographics based on the provided
#' #' Relative Rate Index (RRI) data. For each state, it creates an infographic using
#' #' the `fnc_create_infographic` function, saves the plot as a PNG file, and crops
#' #' the saved image for better presentation.
#' #'
#' #' @param data A data frame containing the RRI data with columns `state` and `rri`.
#' #' @param color A string representing the color to use for the infographic elements.
#' #' @param prefix A string to prefix the saved infographic filenames, typically indicating
#' #'        the type of data or infographic.
#' #'
#' #' @return This function does not return a value but saves PNG files to the specified
#' #'         folder (`png_folder`) for each state.
#' #' @examples
#' #' # Example usage:
#' #' fnc_create_and_save_infographic(data = rri_data, color = "blue", prefix = "rri_")
#' #'
#' fnc_create_and_save_infographic <- function(data, color, prefix) {
#'   # Get a unique list of states to iterate over
#'   states <- unique(data$state)
#'
#'   # Iterate over each state and create its infographic
#'   purrr::map(.x = states, .f = function(x) {
#'     # Filter the data for the specific state
#'     df_state <- data |> filter(state == x)
#'
#'     # Create the infographic using the state's RRI and specified color
#'     fnc_create_infographic(df_state$rri, color)
#'
#'     # Format the state name for filename consistency
#'     # Convert the state name to lowercase and replace spaces with underscores
#'     formatted_state <- stringr::str_to_lower(stringr::str_replace_all(x, " ", "_"))
#'
#'     # Construct the file path for saving the infographic
#'     file_path <- file.path(png_folder, paste0(prefix, formatted_state, ".png"))
#'
#'     # Save the infographic to the specified file path
#'     # Use `ggsave` with standard dimensions and resolution
#'     ggsave(file_path, plot = ggplot2::last_plot(), width = 8, height = 6, dpi = 300)
#'
#'     # Read the saved PNG image for cropping
#'     img <- magick::image_read(file_path)
#'
#'     # Crop the image to remove excess whitespace
#'     img_cropped <- magick::image_trim(img)
#'
#'     # Save the cropped image back to the same file path
#'     magick::image_write(img_cropped, file_path)
#'   })
#' }
#'
#' #' @title Blank Out Plot Theme
#' #' @description This function sets up a theme for blanking out plot elements like axes, scales, and legends.
#' #' @return A list of ggplot2 theme and scale elements for use in plots.
#' #' @export
#' fnc_blankitout <- function(){
#'   list(
#'     theme_void(),  # Removes background and gridlines for a clean appearance.
#'     scale_x_continuous(expand = expansion(mult = ex_w, add = 0)),  # Customizes x-axis scale expansion.
#'     scale_y_continuous(expand = expansion(mult = ex_h, add = 0)),  # Customizes y-axis scale expansion.
#'     theme(legend.position = "none", aspect.ratio = img_ar_hw)  # Removes legend and sets the aspect ratio for the plot.
#'   )
#' }
#'
#' #' Generate Icon Options with Partial and Full Fill States
#' #'
#' #' This function generates a set of icon plots based on different fill states
#' #' (empty, full, partial) using a specified image matrix. The icons can be filled
#' #' horizontally or vertically and are styled with customizable colors.
#' #'
#' #' @param partialval A numeric value between 0 and 1 indicating the proportion of
#' #'   the icon to be filled for the "partial" state.
#' #' @param empty A string specifying the color for the empty part of the icon (default: white).
#' #' @param fill A string specifying the color for the fully filled part of the icon (default: dark color).
#' #' @param partial A string specifying the color for the partially filled part of the icon (default: light color).
#' #' @param bg A string specifying the background color of the icon (default: white).
#' #' @param fillHoriz A logical value indicating whether the fill should be applied
#' #'   horizontally (TRUE) or vertically (FALSE). Defaults to FALSE (vertical fill).
#' #'
#' #' @return A list of ggplot objects representing the empty, full, and partially filled states of the icon.
#' fnc_icon_options <- function(partialval, empty = "#FFFFFF", fill = dark_color, partial = light_color, bg = "#FFFFFF", fillHoriz = FALSE) {
#'   # Ensure partialval is within valid range
#'   if (partialval < 0 | partialval >= 1) stop("partialval must be between 0 and 1")
#'
#'   # Define color sets for different states of the icon (empty, full, partial)
#'   cols_lst <- list(
#'     "empty" = c(bg, empty),
#'     "full" = c(bg, fill),
#'     "partial" = c(bg, partial, fill)
#'   )
#'
#'   # Define percentage fills for each icon state
#'   pcts_lst <- list(
#'     "empty" = 0,
#'     "full" = 100,
#'     "partial" = partialval * 100
#'   )
#'
#'   # Initialize the plot list to store generated plots for each state
#'   plot_lst <- list("empty" = NULL, "full" = NULL, "partial" = NULL)
#'
#'   # Determine the boundaries for filling either horizontally or vertically
#'   if (fillHoriz == FALSE) {
#'     pos1 <- which(apply(img[,,1], 2, function(y) any(y == 1)))  # Determine filled vertical range
#'     max <- max(pos1)
#'   } else {
#'     pos1 <- which(apply(img[,,1], 1, function(y) any(y == 1)))  # Determine filled horizontal range
#'     max <- max(pos1)
#'   }
#'   h <- dim(img)[1]  # Icon height
#'   w <- dim(img)[2]  # Icon width
#'   min <- min(pos1)
#'
#'   # Loop through each icon state and generate corresponding plot
#'   for (j in names(plot_lst)) {
#'     pcts <- pcts_lst[[j]]  # Get the fill percentage for the current state
#'     pospct <- round((max - min) * pcts / 100 + min)  # Calculate the fill position based on percentage
#'     finalimg <- img[h:1,,1]  # Flip the image vertically for correct orientation
#'     bkgr <- (finalimg == 1)  # Background mask
#'     colfill <- matrix(rep(FALSE, h*w), nrow = h)  # Initialize fill matrix
#'
#'     # Apply the fill either horizontally or vertically
#'     if (fillHoriz == FALSE) {
#'       colfill[1:h, max:pospct] <- TRUE
#'     } else {
#'       colfill[max:pospct, 1:w] <- TRUE
#'     }
#'
#'     # Assign partially filled cells in the image
#'     finalimg[bkgr & colfill] <- 0.5
#'     df <- reshape2::melt(finalimg)  # Convert matrix to long format for plotting
#'
#'     # Remove partial fill for the 'full' state
#'     if (j == "full") {
#'       df[df$value == 0.5, ] <- 0
#'     }
#'
#'     # Create the ggplot for each icon state
#'     plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value))) +
#'       geom_raster() +
#'       scale_fill_manual(values = cols_lst[[j]]) +  # Apply the corresponding color scheme
#'       fnc_blankitout()  # Apply the blank theme
#'
#'     plot_lst[[j]] <- plot  # Store the plot in the list
#'   }
#'
#'   return(plot_lst)  # Return the list of generated plots
#' }
#'
#' #' Create Icons for Representing RRI (Relative Rate Index)
#' #'
#' #' This function generates a grid of icons to visually represent the Relative Rate Index (RRI).
#' #' Icons can be fully filled, partially filled, or empty, with customizable colors and arrangements.
#' #'
#' #' @param rri_raw Numeric value of the RRI to represent.
#' #' @param rri_digits Integer specifying the number of decimal places to round the RRI (default: 1).
#' #' @param fillcolor Character specifying the color for fully filled icons (default: `dark_color`).
#' #' @param partialcolor Character specifying the color for partially filled icons (default: `light_color`).
#' #' @param emptyhumans Logical indicating whether to include empty icons in the grid (default: `TRUE`).
#' #' @param emptycolor Character specifying the color for empty icons (default: white).
#' #' @param infogs Integer specifying the total number of icons in the grid (default: `default_ncols`).
#' #' @param infogs_ncol Integer specifying the number of columns in the grid (default: `default_ncols`).
#' #' @param fillHoriz Logical indicating whether the fill should be applied horizontally (default: `FALSE`).
#' #'
#' #' @return A grid of icons as a ggplot object.
#' fnc_create_icons <- function(rri_raw, rri_digits = 1, fillcolor = dark_color, partialcolor = light_color,
#'                              emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
#'                              infogs_ncol = default_ncols, fillHoriz = FALSE) {
#'
#'   # Round the RRI value and compute full and partial icons
#'   RRI <- round(rri_raw, digits = rri_digits)
#'   numfull <- floor(RRI)  # Number of fully filled icons
#'   numremain <- RRI - numfull  # Portion of the partial icon
#'
#'   # Generate plot options for full, partial, and empty icons
#'   plot_opts <- fnc_icon_options(partialval = numremain, empty = emptycolor, fill = fillcolor, partial = partialcolor, fillHoriz = fillHoriz)
#'
#'   plot_list <- list()  # Initialize list for storing plots
#'
#'   # Create full and partial icons based on RRI value
#'   if (RRI > 1 & numremain != 0) {
#'     for (i in 1:numfull) {
#'       plot_list[[i]] <- plot_opts$full
#'     }
#'     plot_list[[numfull + 1]] <- plot_opts$partial
#'   } else if (RRI > 1 & numremain == 0) {
#'     for (i in 1:numfull) {
#'       plot_list[[i]] <- plot_opts$full
#'     }
#'   } else if (RRI == 1) {
#'     plot_list[[1]] <- plot_opts$full
#'   } else if (RRI < 1) {
#'     plot_list[[1]] <- plot_opts$partial
#'   }
#'
#'   # Add empty icons if needed
#'   if (emptyhumans == TRUE & length(plot_list) != infogs) {
#'     st_empty <- ifelse(numremain != 0, numfull + 2, numfull + 1)
#'     for (i in st_empty:infogs) {
#'       plot_list[[i]] <- plot_opts$empty
#'     }
#'   }
#'
#'   # Determine the number of rows for the icon grid
#'   rows <- ifelse(infogs > infogs_ncol, ceiling(rri_raw / infogs_ncol), 1)
#'
#'   # Return the grid of icon plots
#'   plot_grid(plotlist = plot_list, nrow = rows)
#' }
#'
#' #' Create an Infographic Representing the RRI (Relative Rate Index)
#' #'
#' #' This function generates an infographic that visually represents the Relative Rate Index (RRI)
#' #' using an icon grid and a bold text label displaying the RRI value.
#' #'
#' #' @param rri_raw Numeric value of the RRI to represent.
#' #' @param infographic_color Character specifying the color for the icons and text in the infographic.
#' #'
#' #' @return A ggplot object combining the RRI text label and the icon grid.
#' fnc_create_infographic <- function(rri_raw, infographic_color) {
#'
#'   # Round the RRI value and format as a text label
#'   rri_text <- paste0(round(rri_raw, digits = 1), "x")
#'
#'   # Generate the icons for the infographic
#'   ggtemp_justpeople <- fnc_create_icons(
#'     rri_raw = rri_raw,
#'     infogs = default_ncols,
#'     infogs_ncol = default_ncols,
#'     fillcolor = infographic_color,
#'     partialcolor = light_color,
#'     emptyhumans = TRUE,
#'     emptycolor = "white",
#'     fillHoriz = FALSE
#'   )
#'
#'   # Create the plot for displaying the RRI text label
#'   rri_label_plot <- ggplot() +
#'     annotate("text", x = 1, y = 1, label = rri_text, size = 12, hjust = 0.5,
#'              fontface = "bold",
#'              color = infographic_color,
#'              family = "Graphik") +
#'     theme_void()
#'
#'   # Combine the RRI label plot with the icon grid
#'   final_plot <- plot_grid(
#'     rri_label_plot, ggtemp_justpeople,
#'     nrow = 1, rel_widths = c(1, 6)  # Adjust widths to balance the label and icons
#'   )
#'
#'   print(final_plot)  # Display the final infographic plot
#' }
#'
#' #' Round Numbers to Significant Figures or Nearest Tens
#' #'
#' #' This function rounds numbers based on their magnitude.
#' #' - For numbers with 3 or more digits, it rounds to the nearest power of 10 below.
#' #' - For smaller numbers, it rounds to the nearest tens place.
#' #'
#' #' @param x A numeric vector to be rounded.
#' #' @return A numeric vector with rounded values. If the input contains `NA`, the corresponding output will also be `NA`.
#' #' @examples
#' #' fnc_round_to_power(c(12345, 678, 45, 9, NA))
#' #' # Returns: 12300, 680, 50, 10, NA
#' #'
#' fnc_round_to_power <- function(x) {
#'   sapply(x, function(val) {
#'     # Check if the value is NA, and return NA if true
#'     if (is.na(val)) {
#'       return(NA)
#'     }
#'
#'     # Determine the number of digits in the number
#'     digits <- nchar(floor(val))
#'
#'     # Define the rounding level: if digits >= 3, round to the nearest power of 10 down, else round to 10
#'     if (digits >= 3) {
#'       power <- 10^(digits - 2) # This determines the rounding level to the nearest power of 10 below
#'       round(val / power) * power  # Use round to round to the nearest significant value
#'     } else {
#'       round(val, -1)
#'     }
#'   })
#' }
#'
#'
#' #' Create an Icon Grid for the Homepage
#' #'
#' #' This function generates a grid of icons representing the Relative Rate Index (RRI),
#' #' with the first icon displayed in a distinct color (e.g., green), followed by a combination
#' #' of full, partial, and empty icons to represent the RRI value.
#' #'
#' #' @param rri_raw Numeric value of the RRI to represent.
#' #' @param rri_digits Integer specifying the number of decimal places to round the RRI.
#' #' @param fillcolor Character specifying the color for fully filled icons (default: "darkgray").
#' #' @param partialcolor Character specifying the color for partially filled icons (default: "white").
#' #' @param emptyhumans Logical indicating whether to include empty icons (default: TRUE).
#' #' @param emptycolor Character specifying the color for empty icons (default: "white").
#' #' @param infogs Integer specifying the total number of icons in the grid (default: `default_ncols`).
#' #' @param infogs_ncol Integer specifying the number of columns in the icon grid (default: `default_ncols`).
#' #' @param fillHoriz Logical indicating whether the icons should fill horizontally (default: FALSE).
#' #'
#' #' @return A ggplot object representing the grid of icons for the homepage.
#' #' @examples
#' #' # Generate a homepage icon grid for an RRI of 3.5
#' #' fnc_create_icons_homepage(rri_raw = 3.5, fillcolor = "blue")
#' fnc_create_icons_homepage <- function(rri_raw, rri_digits = 1, fillcolor = "darkgray", partialcolor = "white",
#'                                       emptyhumans = TRUE, emptycolor = "white", infogs = default_ncols,
#'                                       infogs_ncol = default_ncols, fillHoriz = FALSE) {
#'
#'   # Round the RRI value to the specified number of digits
#'   RRI <- round(rri_raw, digits = rri_digits)
#'   numfull <- floor(RRI)  # Number of fully filled icons
#'   numremain <- RRI - numfull  # Portion of the partially filled icon
#'
#'   # Generate plot options for full, partial, and empty icons
#'   plot_opts <- fnc_icon_options(
#'     partialval = numremain,  # Partial fill value for partially filled icons
#'     empty = emptycolor,      # Color for empty icons
#'     fill = fillcolor,        # Color for fully filled icons
#'     partial = partialcolor,  # Color for partially filled icons
#'     fillHoriz = fillHoriz    # Direction of the fill (horizontal or vertical)
#'   )
#'
#'   # Initialize a list to store the plots
#'   plot_list <- list()
#'
#'   # Set the first icon to a distinct color (e.g., green)
#'   first_icon_color <- color4  # Customize this color as needed
#'   first_icon_opts <- fnc_icon_options(
#'     partialval = 0,          # No partial fill for the first icon
#'     empty = emptycolor,      # Color for empty background
#'     fill = first_icon_color, # Color for the first icon
#'     partial = first_icon_color, # Use the same color for partial fill
#'     fillHoriz = fillHoriz    # Direction of the fill
#'   )
#'   plot_list[[1]] <- first_icon_opts$full  # Add the first icon to the list
#'
#'   # Create fully filled icons in gray for the remaining RRI value
#'   for (i in 2:numfull) {
#'     plot_list[[i]] <- plot_opts$full
#'   }
#'
#'   # Add a partially filled icon if the RRI has a fractional part
#'   if (numremain > 0) {
#'     plot_list[[numfull + 1]] <- plot_opts$partial
#'   }
#'
#'   # Add empty icons to complete the grid if needed
#'   if (emptyhumans && length(plot_list) < infogs) {
#'     for (i in (numfull + 2):infogs) {
#'       plot_list[[i]] <- plot_opts$empty
#'     }
#'   }
#'
#'   # Determine the number of rows for the icon grid
#'   rows <- ifelse(infogs > infogs_ncol, ceiling(length(plot_list) / infogs_ncol), 1)
#'
#'   # Return the grid of icons as a ggplot object
#'   plot_grid(plotlist = plot_list, nrow = rows)
#' }
#'
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#' #####################################################################################################################################
#'
