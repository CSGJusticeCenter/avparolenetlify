
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# IMPORT FUNCTIONS
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

fnc_format_citation <- function(citation) {
  # Italicize the report title: Add * around report title
  formatted_citation <- str_replace_all(
    citation,
    "Prison-Release Discretion and Prison Population Size: State Report: [^\\(]+",
    function(x) paste0("*", x, "*")
  )

  # Convert URLs to markdown hyperlinks and ensure the period is outside the link
  formatted_citation <- str_replace_all(
    formatted_citation,
    "(https?://[^\\s]+)\\.",  # Match the URL pattern followed by a period
    function(x) {
      url <- str_remove(x, "\\.$")  # Remove the period from the URL
      paste0("[", url, "](", url, ").")  # Place the period outside the link
    }
  )

  return(formatted_citation)
}


fnc_read_and_add_year <- function(file_path) {
  # Read the data from Stata file
  data <- read_dta(file_path)

  # Extract year from file name using regular expression
  year <- sub(".*_(\\d{4})_.*", "\\1", file_path)

  # Add extracted year as rptyear column
  data <- data %>% mutate(rptyear = as.numeric(year))

  # Remove labels from state_encoded, if it exists
  if("state_encoded" %in% colnames(data)) {
    data$state_encoded <- as.numeric(data$state_encoded)
  }

  return(data)
}

fnc_create_fbi_index <- function(df) {
  # Define custom order (in reverse)
  custom_order <- c("Drug", "Public Order", "Property",
                    "Aggravated or Simple Assault", "Robbery", "Rape or Sexual Assault",
                    "Negligent Manslaughter", "Murder or Nonnegligent Manslaughter", "Other Violent Offenses",
                    "Unknown")
  df |>
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
}


fnc_create_admtype <- function(df) {
  df |>
    mutate(admtype = case_when(
      admtype == "Other admission (including unsentenced, transfer, AWOL/escapee return)" ~ "Other",
      is.na(admtype) ~ "Unknown",
      TRUE ~ admtype
    ))
}

fnc_clean_bjs_data <- function(df) {
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

  return(df)
}
