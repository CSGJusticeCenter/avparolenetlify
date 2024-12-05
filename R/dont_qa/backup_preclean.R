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
      race = factor(race, levels = c("Unknown",
                                     "Other race(s), non-Hispanic",
                                     "White, non-Hispanic",
                                     "Hispanic, any race",
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
                                      levels = c("18-24 years",
                                                 "25-34 years",
                                                 "35-44 years",
                                                 "45-54 years",
                                                 "55+ years",
                                                 "Unknown")))
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
  # Read the CSV file and skip the specified number of rows for headers/footers
  read.csv(file.path(sp_data_path, file_path))[-(1:skip_rows), ] |>
    clean_names() |>  # Clean column names to ensure consistent formatting
    select(
      state = x,                # Select the state column (renamed as "state")
      male = !!sym(male_col),   # Select the male population column
      female = !!sym(female_col) # Select the female population column
    ) |>
    # Clean the state names and handle special cases for Alaska and Utah
    mutate(
      state = str_replace_all(state, "/.*", ""),  # Remove content after "/"
      state = str_replace_all(state, c("Alaskab" = "Alaska", "Utahc" = "Utah"))
    ) |>
    # Filter out rows with invalid or non-state values
    filter(
      !state %in% c("", "State", "Federal", "District of Columbia",
                    "U.S. Total", "U.S. total", "U.S. tota")
    ) |>
    # Clean and convert male and female population columns to numeric
    mutate(
      male = as.numeric(str_replace_all(male, "[^\\d]", "")),  # Remove non-digit characters
      female = as.numeric(str_replace_all(female, "[^\\d]", "")) # Remove non-digit characters
    ) |>
    # Reshape the data from wide to long format for easier analysis
    pivot_longer(
      cols = c(male, female),  # Specify columns to pivot
      names_to = "sex",        # Create a new column "sex" for male/female
      values_to = "n"          # Create a new column "n" for population counts
    ) |>
    # Group by state to calculate proportions and labels
    group_by(state) |>
    mutate(
      prop = (n / sum(n)) * 100,                     # Calculate percentage for each sex
      prop_label = paste0(round(prop, 0), "%"),      # Create a label for percentage
      n_label = formattable::comma(n, 0),            # Format the population count with commas
      sex = case_when(
        sex == "male" ~ "Male",                      # Standardize "male" to "Male"
        sex == "female" ~ "Female",                  # Standardize "female" to "Female"
        TRUE ~ sex                                   # Leave other values unchanged
      ),
      rptyear = year                                 # Add reporting year
    ) |>
    ungroup() # Remove grouping for final output
}

#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

#######################################
# Project: AV Parole
# File: national_trends.R
# Authors: Mari Roberts
# Date last updated: November 4, 2024 (MAR)
# Description:
#    Parole eligibility map, tables, and other visualizations for national trends page
#######################################

#------------------------------------------------------------------------------#
# Parole Eligibility Table
#------------------------------------------------------------------------------#

# Filter NCRP projections for the specified projection year and calculate rounded values
# - Exclude states listed in `states_to_exclude`
# - Calculate projected population past parole eligibility year (PEY) rounded to nearest power
# - Round percentage past PEY to the nearest whole number
# - Select only relevant columns for output
parole_eligibility_table_projection_year <- ncrp_projections |>
  filter(year == projection_year) |>
  # filter(!state %in% states_abolished_parole$state) |>
  filter(!state %in% states_to_exclude$state) |>
  mutate(proj_pop_past_pey_rounded = fnc_round_to_power(proj_pop_past_pey),
         proj_pcnt_ppey_rounded = round(proj_pcnt_ppey, 0)) |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded)

# OPTION 1)
# Calculate the total projected population past parole eligibility (PE) across all states
proj_past_pe <- ncrp_projections |>
  filter(year == projection_year) |>
  summarise(past_pe_pop = sum(proj_pop_past_pey, na.rm = TRUE))

# Round the total projected population past PE to the nearest power
proj_past_pe_count_rounded <- proj_past_pe |>
  mutate(past_pe_pop_rounded = fnc_round_to_power(past_pe_pop)) |>
  pull(past_pe_pop_rounded)

# Extract the unrounded total projected population past PE for further calculations
proj_past_pe <- proj_past_pe |>
  pull(past_pe_pop)

# Calculate the total projected prison population for the specified projection year
proj_prison_pop <- ncrp_population_projections |>
  filter(year == projection_year) |>
  summarise(total_prison_pop = sum(total_prison_population, na.rm = TRUE)) |>
  pull(total_prison_pop)

# Calculate the ratio of total prison population to population past PE (1 in X individuals)
proj_past_pe_1_in_x <- round(proj_prison_pop/proj_past_pe, 0)

#-------------------------------------------------------------------------------
# PEOPLE INFOGRAPHICS
#-------------------------------------------------------------------------------

# General setup
wd <- getwd()
whichimage <- "person-2745706-bw"

# Set up colors
light_color  <- "white"
empty_color   <- "#FFFFFF"
default_ncols <- ceiling(proj_past_pe_1_in_x)

# Image setup
if (whichimage == "person-2745706-bw") {
  px_h <- 521
  px_w <- 323
  ex_h <- 0.005
  ex_w <- 0.02
  img_ar_hw <- (px_h * (1 + ex_h)) / (px_w * (1 + ex_w))
  img_ar_wh <- (px_w * (1 + ex_w)) / (px_h * (1 + ex_h))
  rawimg <- readPNG(file.path(wd, glue("img/{whichimage}.png")))
  img <- ifelse(rawimg == 0, 1, 0)
}

# Create 1 in X infographic
fnc_create_icons_homepage(proj_past_pe_1_in_x, emptyhumans = TRUE)

# Save the infographic with the formatted state name
file_path <- file.path("img/pe_1_in_x.png")
ggsave(file_path, plot = last_plot(), width = 8, height = 6, dpi = 300)

# Load, crop, and save the image
img <- image_read(file_path)
img_cropped <- image_trim(img)
image_write(img_cropped, file_path)

#------------------------------------------------------------------------------#
# Parole Board Members by State
#------------------------------------------------------------------------------#

# Get parole status information by state
# Get number of parole board members
states_parole <- state_notes |>
  select(state, abolished_parole, members)


#------------------------------------------------------------------------------#
# Parole Eligibility Table
#------------------------------------------------------------------------------#

# Only include states that abolished parole + Lousiana (high PE population)
parole_eligibility_table <- parole_eligibility_table_projection_year |>
  left_join(states_parole, by = "state") |>
  filter(abolished_parole == "N" | state == "Louisiana") |>
  select(state, proj_pcnt_ppey_rounded, proj_pop_past_pey_rounded, members)

# Rename variables for downloadable table
parole_eligibility_table_download <- parole_eligibility_table |>
  select(State = state,
         `2023 Projection: In Prison Past Parole Eligibility (N)` = proj_pop_past_pey_rounded,
         `2023 Projection: In Prison Past Parole Eligibility (%)` = proj_pcnt_ppey_rounded,
         `Parole Board Members` = members)


#------------------------------------------------------------------------------#
# Parole Eligibility Map
#------------------------------------------------------------------------------#

# Create a vector of all state names
all_states <- state.name

# Define the gradient colors for categories
gradient_colors <- c(gradient1, gradient2, gradient3, gradient4, blue)

# Prepare tooltips and map data
# Prepare data for national maps
map_data <- parole_eligibility_table_projection_year |>

  # add missing states
  complete(state = all_states) |>

  # add info about whether state abolished parole release
  left_join(states_parole, by = "state") |>

  # Format data and create tooltip
  mutate(
    state_abb = state.abb[match(state, state.name)],

    all_na = ifelse(is.na(proj_pop_past_pey_rounded)
                    , TRUE, FALSE),

    # Create tooltips
    tooltip = case_when(

      state == "Louisiana" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Percentage of People: ", paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ", formattable::comma(proj_pop_past_pey_rounded, 0),
               "<br>Louisiana is listed among the states with parole systems, despite<br>
               its recent abolition of parole, due to a substantial population<br>
               that remains eligible for parole release under the previous system.<br>"),

      all_na == TRUE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Parole eligibility data is not available.<br>"),

      all_na == TRUE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "Y" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               state, " abolished discretionary parole.<br>"),

      all_na == FALSE & abolished_parole == "N" ~
        paste0("<span style='font-size: 1.5em;'><b>", state, "</b></span><br>",
               "Percentage of People: ",
               paste0(round(proj_pcnt_ppey_rounded, 0), "%<br>"),
               "Number of People: ",
               formattable::comma(proj_pop_past_pey_rounded, 0))
    ),

    tooltip = str_replace_all(tooltip, "NA%", "No Data"),
    tooltip = str_replace_all(tooltip, "NA", "No Data")
  ) |>

  # create data labels
  mutate(change_label = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         # change_label = str_replace_all(change_label, "NA%", "-"),
         change_label = str_replace_all(change_label, "NA%", " "),

         currentperclabel = paste0(round(proj_pcnt_ppey_rounded, 0), "%"),
         currentperclabel = str_replace_all(currentperclabel, "NA%", "No Data"))


# Calculate the breaks for the percent of people eligible for parole
num_breaks <- length(gradient_colors) - 1
breaks <- quantile(map_data$proj_pcnt_ppey_rounded, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE)
breaks[1] <- 0  # Set the first break to 0
breaks <- unique(c(breaks[1], round(breaks[-1], 0)))  # Round and remove duplicates
breaks <- cummax(breaks)  # Ensure breaks are strictly increasing

# Process map_data to include gradient color and data category
map_data_breaks <- map_data |>
  mutate(
    gradient_color = findInterval(proj_pcnt_ppey_rounded, vec = breaks, rightmost.closed = TRUE, all.inside = TRUE),
    gradient_color = ifelse(is.na(proj_pcnt_ppey_rounded), NA, gradient_colors[gradient_color]),
    proj_pcnt_ppey_rounded = round(proj_pcnt_ppey_rounded, 0),
    data_category_num = as.numeric(factor(gradient_color, levels = gradient_colors))
  ) |>
  group_by(gradient_color) |>
  mutate(
    data_category = case_when(
      # state == "Louisiana" ~ "No Discretionary Parole",
      gradient_color == gradient_colors[1] ~ paste0(breaks[1], "% - ", breaks[2], "%"),
      gradient_color == gradient_colors[2] ~ paste0(breaks[2] + 1, "% - ", breaks[3], "%"),
      gradient_color == gradient_colors[3] ~ paste0(breaks[3] + 1, "% - ", breaks[4], "%"),
      gradient_color == gradient_colors[4] ~ paste0(breaks[4] + 1, "% - ", breaks[5], "%"),
      gradient_color == gradient_colors[5] ~ paste0(breaks[5] + 1, "% - ", max(map_data$proj_pcnt_ppey_rounded, na.rm = TRUE), "%")
    ),
    data_category = case_when(
      is.na(data_category) & abolished_parole == "N" ~ "Missing Data",
      is.na(data_category) & abolished_parole == "Y" ~ "No Discretionary Parole",
      # state == "Louisiana" ~ "No Discretionary Parole",
      TRUE ~ data_category
    ),
    gradient_color = case_when(
      is.na(gradient_color) & data_category == "Missing Data" ~ darkgray,
      is.na(gradient_color) & data_category == "No Discretionary Parole" ~ "white",
      # state == "Louisiana" ~ "white",
      TRUE ~ gradient_color
    ),
    data_category_num = case_when(
      is.na(data_category_num) & data_category == "Missing Data" ~ 6,
      is.na(data_category_num) & data_category == "No Discretionary Parole" ~ 5,
      # state == "Louisiana" ~ 5,
      TRUE ~ data_category_num
    )
  )

# create hex map
map_percent <- highchart(height = 625) |>

  hc_chart(marginTop = 50,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      align = "center",
                      formatter = JS("function() {
                          return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
                      }"),
                      style = list(fontSize = "16px",
                                   fontWeight = "regular",
                                   align = "center",
                                   fontFamily = "Graphik",
                                   textOutline = 0)),

    borderColor = darkgray,
    borderWidth = 0.5,
    nullColor = lightgray) |>

  hc_colorAxis(dataClassColor = "category",
               dataClasses = list(
                 list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
                      marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
                 list(from = 6, to = 6, color = darkgray, name = "Missing Data")
               )
  ) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_add_theme(base_hc_theme) |>

  hc_plotOptions(series = list(
    animation = FALSE,
    cursor = "pointer",
    borderWidth = 3,
    accessibility = list(
      enabled = TRUE,
      keyboardNavigation = list(enabled = TRUE),
      pointDescriptionFormatter = JS("function(point) {
        return 'State: ' + point.state_abb + ', Percentage: ' + point.currentperclabel;
      }")
    )
  ),
  accessibility = list(
    enabled = TRUE,
    keyboardNavigation = list(enabled = TRUE),
    linkedDescription =
      paste0("This hexagonal map visualizes the projected proportion of people in prison past their parole eligibility across different U.S. states in 2023. ",
             "States are represented as hexagons, with color gradients indicating different percentage ranges of prison populations past parole eligibility. ",
             "The map also includes a category for states that have abolished discretionary parole and those with missing data."),
    landmarkVerbosity = "one"
  ),
  area = list(accessibility = list(description =
                                     paste0("This chart visually compares parole eligibility status across U.S. states, using colors to denote different percentage ranges.")))
  ) |>

  hc_tooltip(
    borderWidth = 1,
    borderRadius = 0,
    backgroundColor = '#FFFFFF', # Fully opaque white background
    outside = TRUE, # Ensure tooltip is rendered outside
    useHTML = TRUE,
    formatter = JS("function() {
          return '<div style=\"background-color: #FFFFFF; opacity: 1; border: none; padding: 5px;\">' +
          '<div style=\"text-align:left;\">' +
          '<span style=\"font-weight:normal; font-size: 1em;\">' + this.point.tooltip + '</span>' +
          '</div></div>';
    }")
  ) |>

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(enabled = FALSE, filename = "proj_past_parole_eligibility_2023") |>

  hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
             y = 0) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 0,
            y = -30,
            itemMarginTop = 2,
            itemMarginBottom = 2)

# Add JavaScript to apply a gray border to the "No Discretionary Parole" legend item
map_percent <- onRender(map_percent, "
  function(el, x) {
    // Add CSS to target the circle symbol of the second legend item
    var style = document.createElement('style');
    style.innerHTML = `
      .highcharts-legend-item:nth-child(5) .highcharts-point {
        stroke: gray;
        stroke-width: 1px;
      }
    `;
    document.head.appendChild(style);
  }
")

# View map
map_percent

# KEEP THIS CODE FOR NOW
# DOWNLOAD MAP OPTION
map_percent_download <- highchart(height = 625,
                                  width = 1000) |>

  hc_chart(marginTop = 50,
           marginBottom = 50,
           marginRight = 50) |>

  hc_add_series_map(
    map = hex_gj,
    df = map_data_breaks,
    joinBy = "state_abb",
    value = "data_category_num",
    dataLabels = list(enabled = TRUE,
                      useHTML = TRUE,
                      align = "center",
                      formatter = JS("function() {
                          return '<div style=\"text-align:center; font-weight:regular;\">' + this.point.state_abb + '<br>' + this.point.change_label + '</div>';
                      }"),
                      style = list(fontSize = "16px",
                                   fontWeight = "regular",
                                   align = "center",
                                   fontFamily = "Graphik",
                                   textOutline = 0)),

    borderColor = darkgray,
    borderWidth = 0.5,
    nullColor = lightgray) |>

  hc_colorAxis(dataClassColor = "category",
               dataClasses = list(
                 list(from = 1, to = 1, color = gradient1, name = paste0(breaks[1], "% - ", breaks[2], "%")),
                 list(from = 2, to = 2, color = gradient2, name = paste0(breaks[2] + 1, "% - ", breaks[3], "%")),
                 list(from = 3, to = 3, color = gradient3, name = paste0(breaks[3] + 1, "% - ", breaks[4], "%")),
                 list(from = 4, to = 4, color = gradient4, name = paste0(breaks[4] + 1, "% - ", breaks[5], "%")),
                 list(from = 5, to = 5, color = "white", name = "No Discretionary Parole",
                      marker = list(lineColor = 'gray', lineWidth = 2, radius = 10)), # Define radius for visibility
                 list(from = 6, to = 6, color = darkgray, name = "Missing Data")
               )
  ) |>

  hc_legend(align = "right",
            verticalAlign = "bottom",
            layout = "vertical",
            symbolHeight = 15,
            symbolWidth = 15,
            x = 0,
            y = -40,
            itemMarginTop = 2,
            itemMarginBottom = 2) |>

  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>

  hc_add_theme(base_hc_theme) |>

  hc_title(text = "Percentage of People in Prison Past Parole Eligibility<br>2023 Projections",
           align = "center",
           style = list(fontSize = "1.75em", fontWeight = "bold")) |>

  hc_exporting(
    enabled = FALSE) |>

  hc_caption(text = "National Corrections Reporting Program, 2019 and CSG Justice Center Estimates",
             y = 0)

# Add JavaScript to apply a gray border to the "Abolished Discretionary Parole" legend item
map_proj_past_parole_eligibility_2023 <- onRender(map_percent_download, "
  function(el, x) {
    // Add CSS to target the circle symbol of the second legend item
    var style = document.createElement('style');
    style.innerHTML = `
      .highcharts-legend-item:nth-child(5) .highcharts-point {
        stroke: gray;
        stroke-width: 1px;
      }
    `;
    document.head.appendChild(style);
  }
")

# Save map_proj_past_parole_eligibility_2023 as a temporary HTML file
saveWidget(map_proj_past_parole_eligibility_2023, file = "temp.html", selfcontained = TRUE)

# Use webshot to take a screenshot and save it as a PNG
webshot2::webshot(
  url = "temp.html",
  file = file.path("img/map_proj_past_parole_eligibility_2023.png"),
  delay = 1,
  vwidth = 1200,
  vheight = 500,
  cliprect = c(0, 0, 1000, 625)
)

#------------------------------------------------------------------------------#
# Save Data
#------------------------------------------------------------------------------#

# Define the data objects and their corresponding file names
data_files <- list(
  map_percent                       = "map_percent.rds",
  proj_past_pe_count_rounded        = "proj_past_pe_count_rounded.rds",
  proj_past_pe_1_in_x               = "proj_past_pe_1_in_x.rds",
  parole_eligibility_table          = "parole_eligibility_table.rds",
  parole_eligibility_table_download = "parole_eligibility_table_download.rds"
)

# Loop through the list and save each data object to its corresponding file
invisible(lapply(names(data_files), function(obj) {
  save(list = obj, file = file.path(app_folder, data_files[[obj]]))
}))

# Define file name
file_name <- "parole_eligibility_by_state_2023_estimates.csv"

# Construct the full path
file_path <- file.path(app_folder, file_name)

# Example of writing to this path
write.csv(parole_eligibility_table_download, file_path, row.names = FALSE)



#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

