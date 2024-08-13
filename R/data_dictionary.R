
raw_aoc_subdirectory <- "data/raw/AOC"

# Load raw data
raw_probation_drc_program_referral <- read_excel(file.path(config$sp_data_path,
                                                           raw_aoc_subdirectory, "Probation_DRC Program Referral Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_financial_assessment <- read_excel(file.path(config$sp_data_path,
                                                                raw_aoc_subdirectory, "Probation_OCMS Financial Assessment Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_offender_supervision <- read_excel(file.path(config$sp_data_path,
                                                                raw_aoc_subdirectory, "Probation_OCMS Offender and Supervision Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_risk_assessment <- read_excel(file.path(config$sp_data_path,
                                                           raw_aoc_subdirectory, "Probation_OCMS Risk Assessment Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_sentencing <- read_excel(file.path(config$sp_data_path,
                                                      raw_aoc_subdirectory, "Probation_OCMS Sentencing Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_substance_testing <- read_excel(file.path(config$sp_data_path,
                                                             raw_aoc_subdirectory, "Probation_OCMS Substance Testing Data 2013-2023 do not send.xlsx"), skip = 3)
raw_probation_ocms_supervision_violations <- read_excel(file.path(config$sp_data_path,
                                                                  raw_aoc_subdirectory, "Probation_OCMS Supervision Violations 2013-2023.xlsx"), skip = 3)

raw_wv_magistrate_arrest <- read_excel(file.path(config$sp_data_path,
                                                 raw_aoc_subdirectory, "WV Magistrate Court Services - Arrest Data.xlsx"), skip = 3)
raw_wv_magistrate_bailpiece_served <- read_excel(file.path(config$sp_data_path,
                                                           raw_aoc_subdirectory, "WV Magistrate Court Services - Bailpiece Served Data.xlsx"), skip = 3)
raw_wv_magistrate_bond <- read_excel(file.path(config$sp_data_path,
                                               raw_aoc_subdirectory, "WV Magistrate Court Services - Bond Data.xlsx"), skip = 3)
raw_wv_magistrate_fee_assessment <- read_excel(file.path(config$sp_data_path,
                                                         raw_aoc_subdirectory, "WV Magistrate Court Services - Fee  Assessment Data.xlsx"), skip = 3)
raw_wv_magistrate_hearing <- read_excel(file.path(config$sp_data_path,
                                                  raw_aoc_subdirectory, "WV Magistrate Court Services - Hearing Data.xlsx"), skip = 3)
raw_wv_magistrate_pretrial_risk_assessment <- read_excel(file.path(config$sp_data_path,
                                                                   raw_aoc_subdirectory, "WV Magistrate Court Services - Pretrial Risk Assessment Data.xlsx"), skip = 3)
raw_wv_magistrate_sentencing <- read_excel(file.path(config$sp_data_path,
                                                     raw_aoc_subdirectory, "WV Magistrate Court Services - Sentencing Data.xlsx"), skip = 3)
raw_wv_magistrate_transfer_appealed <- read_excel(file.path(config$sp_data_path,
                                                            raw_aoc_subdirectory, "WV Magistrate Court Services - Transfer Appealed Remanded Circuit Docket Data.xlsx"), skip = 3)

raw_wv_circuit_data_request <- read_excel(file.path(config$sp_data_path,
                                                    raw_aoc_subdirectory, "WVCircuit_Data_RequestCSG.xlsx"))
raw_wv_magistrate_case_defendant_disposition <- read_excel(file.path(config$sp_data_path,
                                                                     raw_aoc_subdirectory, "WV Magistrate Court Services - Case, Defendant, Disposition Data.xlsx"), skip = 3)





fnc_create_codebook <- function(df, var_descriptions = NULL) {

  # Initialize an empty dataframe with desired columns
  codebook <- data.frame(
    variable_name = character(),
    variable_description = character(),
    variable_type = character(),
    number_of_unique_values = integer(),
    missing_values = integer(),
    percentage_missing = double(),
    statistics = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each column name in the dataframe
  for (colname in names(df)) {
    # Determine the variable description; if provided, use it, otherwise, set it as 'Description needed.'
    var_desc <- ifelse(!is.null(var_descriptions) && !is.null(var_descriptions[[colname]]),
                       var_descriptions[[colname]],
                       "Description needed.")

    # Get the variable's data using the column name
    var <- df[[colname]]
    # Count the total number of values
    total_values <- length(var)
    # Count the number of NA values
    num_occurrences <- sum(!is.na(var))
    # Calculate the number of missing values
    num_missing <- total_values - num_occurrences
    # Calculate the percentage of missing values
    percent_missing <- (num_missing / total_values)
    # Determine the number of unique values
    num_unique <- length(unique(var))
    # Initialize an empty string to store statistics (determined below)
    stats <- ""

    # Determine the type of variable and create statistics or examples
    if (is.factor(var)) {
      # Create a table of factor levels and their counts, sorted in decreasing order
      level_counts <- sort(table(var), decreasing = TRUE)
      # Calculate the percentage for each factor level count
      level_percentages <- (level_counts / sum(level_counts))
      # Initialize an empty vector to store statistics for each level
      stats_vector <- c()

      # Loop through each level in the factor
      for (level in names(level_counts)) {
        # Join the level, its count, and its percentage into the stats_vector
        stats_vector <- c(stats_vector,
                          paste0(level, " = ", formatC(level_counts[[level]], format="f", #big.mark=",",
                                                       digits=0),
                                 " (", formatC(round(level_percentages[[level]]*100, 1), format="f", #big.mark=",",
                                               digits=1), "%)"))
      }

      # If there are missing values, add their count and percentage to the stats_vector
      if (num_missing > 0) {
        stats_vector <- c(stats_vector,
                          paste0("Missing = ", formatC(num_missing, format="f", #big.mark=",",
                                                       digits=0),
                                 " (", formatC(round(percent_missing*100, 1), format="f", #big.mark=",",
                                               digits=1), "%)"))

      }

      # Join all elements of the stats_vector into a single string with HTML breaks
      stats <- paste(stats_vector, collapse = "<br>")

    } else if (is.numeric(var)) {
      # Create a string of basic descriptive statistics for numeric variables
      stats <- paste("Min: ", formatC(round(min(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "Avg: ", formatC(round(mean(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "Median: ", formatC(round(median(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                         digits=1), "<br>",
                     "Max: ", formatC(round(max(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "SD: ", formatC(round(sd(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                     digits=1))

    } else if (inherits(var, "Date")) {
      # For Date variables, create a string with the minimum and maximum dates
      stats <- paste("Min Date: ", min(var, na.rm = TRUE), "<br>",
                     "Max Date: ", max(var, na.rm = TRUE))

    } else if (is.character(var)) {
      # For character variables, get non-missing values
      non_missing_var <- var[!is.na(var)]
      # Determine whether to show sample values based on the number of available values
      if (length(non_missing_var) > 0) {
        # If at least two non-missing values exist, sample two of them; otherwise, take what's available
        if (length(non_missing_var) >= 2) {
          sample_values <- sample(non_missing_var, size = 2)
        } else {
          sample_values <- non_missing_var
        }
        # Create a string with example values from the variable
        stats <- paste("Example values: ", paste(shQuote(sample_values), collapse = ", "))
      } else {
        stats <- "No character values to display."
      }
    } else {
      # If the variable type is not factor, numeric, date, or character, indicate it as 'Other type'
      stats <- "Other type"
    }

    # Add the gathered information as a new row in the codebook dataframe
    codebook <- rbind(
      codebook,
      data.frame(
        variable_name = colname,
        variable_description = var_desc,
        statistics = stats,
        variable_type = class(var)[1],
        number_of_unique_values = num_unique,
        missing_values = num_missing,
        percentage_missing = percent_missing,
        stringsAsFactors = FALSE
      )
    )
  }

  # Replace underscores with spaces in column names and convert them to title case
  new_colnames <- gsub("_", " ", colnames(codebook))
  new_colnames <- tools::toTitleCase(new_colnames)
  colnames(codebook) <- new_colnames

  # Return the completed codebook
  return(codebook)
}

fnc_convert_to_factors <- function(data, vars) {
  data <- data |> mutate(across(all_of(vars), as.factor))
  return(data)
}








fnc_create_codebook <- function(df, var_descriptions = NULL, hide_statistics = NULL) {

  # Initialize an empty dataframe with desired columns
  codebook <- data.frame(
    variable_name = character(),
    variable_description = character(),
    variable_type = character(),
    number_of_unique_values = integer(),
    missing_values = integer(),
    percentage_missing = double(),
    statistics = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each column name in the dataframe
  for (colname in names(df)) {
    # Determine the variable description; if provided, use it, otherwise, set it as 'Description needed.'
    var_desc <- ifelse(!is.null(var_descriptions) && !is.null(var_descriptions[[colname]]),
                       var_descriptions[[colname]],
                       "Description needed.")

    # Get the variable's data using the column name
    var <- df[[colname]]
    # Count the total number of values
    total_values <- length(var)
    # Count the number of NA values
    num_occurrences <- sum(!is.na(var))
    # Calculate the number of missing values
    num_missing <- total_values - num_occurrences
    # Calculate the percentage of missing values
    percent_missing <- round((num_missing / total_values) * 100, 1)
    # Determine the number of unique values
    num_unique <- length(unique(var))
    # Initialize an empty string to store statistics (determined below)
    stats <- ""

    # Determine the type of variable and create statistics or examples
    if (is.factor(var)) {
      # Create a table of factor levels and their counts
      level_counts <- table(var)
      # Calculate the percentage for each factor level count
      level_percentages <- (level_counts / sum(level_counts))

      # Sort levels based on the variable name
      if (grepl("county", tolower(colname))) {
        # Sort alphabetically if "county" is in the name
        level_counts <- level_counts[order(as.character(names(level_counts)))]
      } else if (grepl("year", tolower(colname))) {
        # Sort numerically if "year" is in the name
        level_counts <- level_counts[order(as.numeric(names(level_counts)))]
      } else {
        # Default to sorting by proportion
        level_counts <- sort(level_counts, decreasing = TRUE)
      }

      # Initialize an empty vector to store statistics for each level
      stats_vector <- c()

      # Loop through each level in the factor
      for (level in names(level_counts)) {
        # Join the level, its count, and its percentage into the stats_vector
        stats_vector <- c(stats_vector,
                          paste0(level, " = ", formatC(level_counts[[level]], format="f", #big.mark=",",
                                                       digits=0),
                                 " (", paste0(round(level_percentages[[level]]*100, 1), "%)")))
      }

      # If there are missing values, add their count and percentage to the stats_vector
      if (num_missing > 0) {
        stats_vector <- c(stats_vector,
                          paste0("Missing = ", formatC(num_missing, format="f", #big.mark=",",
                                                       digits=0),
                                 " (", formatC(round(percent_missing, 1), format="f", #big.mark=",",
                                               digits=1), "%)"))
      }

      # Join all elements of the stats_vector into a single string with HTML breaks
      stats <- paste(stats_vector, collapse = "<br>")

    } else if (is.numeric(var)) {
      # Create a string of basic descriptive statistics for numeric variables
      stats <- paste("Min: ", formatC(round(min(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "Avg: ", formatC(round(mean(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "Median: ", formatC(round(median(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                         digits=1), "<br>",
                     "Max: ", formatC(round(max(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                      digits=1), "<br>",
                     "SD: ", formatC(round(sd(var, na.rm = TRUE), 1), format="f", #big.mark=",",
                                     digits=1))

    } else if (inherits(var, "Date")) {
      # For Date variables, create a string with the minimum and maximum dates
      stats <- paste("Min Date: ", min(var, na.rm = TRUE), "<br>",
                     "Max Date: ", max(var, na.rm = TRUE))

    } else if (is.character(var)) {
      # For character variables, get non-missing values
      non_missing_var <- var[!is.na(var)]
      # Determine whether to show sample values based on the number of available values
      if (length(non_missing_var) > 0) {
        # If at least two non-missing values exist, sample two of them; otherwise, take what's available
        if (length(non_missing_var) >= 2) {
          sample_values <- sample(non_missing_var, size = 2)
        } else {
          sample_values <- non_missing_var
        }
        # Create a string with example values from the variable
        stats <- paste("Example values: ", paste(shQuote(sample_values), collapse = ", "))
      } else {
        stats <- "No character values to display."
      }
    } else {
      # If the variable type is not factor, numeric, date, or character, indicate it as 'Other type'
      stats <- "Other type"
    }

    # Add the gathered information as a new row in the codebook dataframe
    codebook <- rbind(
      codebook,
      data.frame(
        variable_name = colname,
        variable_description = var_desc,
        statistics = if (is.null(hide_statistics) || !(colname %in% hide_statistics)) stats else "Hidden",
        variable_type = class(var)[1],
        number_of_unique_values = comma(num_unique),
        missing_values = comma(num_missing),
        percentage_missing = paste0(percent_missing, "%"),
        stringsAsFactors = FALSE
      )
    )
  }

  # Replace underscores with spaces in column names and convert them to title case
  new_colnames <- gsub("_", " ", colnames(codebook))
  new_colnames <- tools::toTitleCase(new_colnames)
  colnames(codebook) <- new_colnames

  # Return the completed codebook
  return(codebook)
}

## Probation_DRC Program Referral Data 2013-2023.xlsx
vars_to_convert <- c("IntakeYear", "CountyOfJurisdiction", "ServiceType", "Provider", "SPAgency", "ClosureType")
raw_probation_drc_program_referral <- fnc_convert_to_factors(raw_probation_drc_program_referral, vars_to_convert)
raw_probation_drc_program_referral <- raw_probation_drc_program_referral |> mutate(Provider = as.character(Provider))
dictionary_raw_probation_drc_program_referral <- fnc_create_codebook(raw_probation_drc_program_referral,
                                                                     hide_statistics = c("ProbationFileID", "OffenderID"))


## Probation_OCMS Financial Assessment Data 2013-2023.xlsx
vars_to_convert <- c("IntakeYear", "CountyOfJurisdiction", "AccountType", "TransactionType")
raw_probation_ocms_financial_assessment <- fnc_convert_to_factors(raw_probation_ocms_financial_assessment, vars_to_convert)
dictionary_raw_probation_ocms_financial_assessment <- fnc_create_codebook(raw_probation_ocms_financial_assessment,
                                                                          hide_statistics = c("ProbationFileID", "OffenderID"))


## Probation_OCMS Offender and Supervision Data 2013-2023.xlsx
vars_to_convert <- c("IntakeYear", "CountyOfJurisdiction", "CountyOfSupervision", "Race", "MultiRaceComponents",
                     "Sex", "ProbationFileStatus", "CaseType", "ProbationFileType", "SupervisionLevel", "Term",
                     "IncarcerationTerm", "SupervisionType", "DischargeType", "ChargeSeverity", "Agency")
raw_probation_ocms_offender_supervision <- fnc_convert_to_factors(raw_probation_ocms_offender_supervision, vars_to_convert)
raw_probation_ocms_offender_supervision <- raw_probation_ocms_offender_supervision |>
  mutate(Term = as.character(Term),
         IncarcerationTerm = as.character(IncarcerationTerm))
dictionary_raw_probation_ocms_offender_supervision <- fnc_create_codebook(raw_probation_ocms_offender_supervision,
                                                                          hide_statistics = c("NameComposite", "ProbationFileID", "OffenderID"))


## Probation_OCMS Risk Assessment Data 2013-2023.xlsx
vars_to_convert <- c("IntakeYear","CountyOfJurisdiction", "AssessmentType", "Instrument", "Risk/Needs Assessment Levels")
raw_probation_ocms_risk_assessment <- fnc_convert_to_factors(raw_probation_ocms_risk_assessment, vars_to_convert)
dictionary_raw_probation_ocms_risk_assessment <- fnc_create_codebook(raw_probation_ocms_risk_assessment,
                                                                     hide_statistics = c("ProbationFileID", "OffenderID"))


## Probation_OCMS Sentencing Data 2013-2023.xlsx
vars_to_convert <- c("IntakeYear","CountyOfJurisdiction", "CaseType", "SentenceTerm", "SentenceType")
raw_probation_ocms_sentencing <- fnc_convert_to_factors(raw_probation_ocms_sentencing, vars_to_convert)
raw_probation_ocms_sentencing <- raw_probation_ocms_sentencing |>
  mutate(SentenceTerm = as.character(SentenceTerm))
dictionary_raw_probation_ocms_sentencing <- fnc_create_codebook(raw_probation_ocms_sentencing,
                                                                hide_statistics = c("ProbationFileID", "OffenderID",
                                                                                    "ChargeID", "CourtCaseID"))


## Probation_OCMS Substance Testing Data 2013-2023 do not send.xlsx
vars_to_convert <- c("IntakeYear","CountyOfJurisdiction", "SampleType", "TestResults", "Substance", "ProbationAgency", "SubstanceTestingAgency")
raw_probation_ocms_substance_testing <- fnc_convert_to_factors(raw_probation_ocms_substance_testing, vars_to_convert)
raw_probation_ocms_substance_testing <- raw_probation_ocms_substance_testing |>
  mutate(Substance = as.character(Substance))
dictionary_raw_probation_ocms_substance_testing <- fnc_create_codebook(raw_probation_ocms_substance_testing,
                                                                       hide_statistics = c("ProbationFileID", "OffenderID"))


## Probation_OCMS Supervision Violations 2013-2023.xlsx
vars_to_convert <- c("IntakeYear","CountyOfJurisdiction", "Location", "ReasonIncarcerated", "CaseRelated", "ConditionOfProbation",
                     "Sanctions")
raw_probation_ocms_supervision_violations <- fnc_convert_to_factors(raw_probation_ocms_supervision_violations, vars_to_convert)
dictionary_raw_probation_ocms_supervision_violations <- fnc_create_codebook(raw_probation_ocms_supervision_violations,
                                                                            hide_statistics = c("ProbationFileID", "OffenderID"))


## WV Magistrate Court Services - Arrest Data.xlsx
vars_to_convert <- c("FilingYear", "CaseCounty", "ArrestCounty")
raw_wv_magistrate_arrest <- fnc_convert_to_factors(raw_wv_magistrate_arrest, vars_to_convert)
dictionary_raw_wv_magistrate_arrest <- fnc_create_codebook(raw_wv_magistrate_arrest,
                                                           hide_statistics = c("DefendantID", "OffenderID"))


## WV Magistrate Court Services - Bailpiece Served Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "BailPieceDesc")
raw_wv_magistrate_bailpiece_served <- fnc_convert_to_factors(raw_wv_magistrate_bailpiece_served, vars_to_convert)
dictionary_raw_wv_magistrate_bailpiece_served <- fnc_create_codebook(raw_wv_magistrate_bailpiece_served,
                                                                     hide_statistics = c("BailPieceCaseID"))


## WV Magistrate Court Services - Bond Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "BondDescription", "BondStatus")
raw_wv_magistrate_bond <- fnc_convert_to_factors(raw_wv_magistrate_bond, vars_to_convert)
dictionary_raw_wv_magistrate_bond <- fnc_create_codebook(raw_wv_magistrate_bond,
                                                         hide_statistics = c("DefendantID", "CaseID", "BondID", "BondSetID"))


## WV Magistrate Court Services - Fee  Assessment Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "FeeType")
raw_wv_magistrate_fee_assessment <- fnc_convert_to_factors(raw_wv_magistrate_fee_assessment, vars_to_convert)
dictionary_raw_wv_magistrate_fee_assessment <- fnc_create_codebook(raw_wv_magistrate_fee_assessment,
                                                                   hide_statistics = c("DefendantID", "CaseID"))


## WV Magistrate Court Services - Hearing Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "HearingDesc")
raw_wv_magistrate_hearing <- fnc_convert_to_factors(raw_wv_magistrate_hearing, vars_to_convert)
dictionary_raw_wv_magistrate_hearing <- fnc_create_codebook(raw_wv_magistrate_hearing,
                                                            hide_statistics = c("DefendantID", "CaseID"))


## WV Magistrate Court Services - Pretrial Risk Assessment Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "PreTrialRADesc")
raw_wv_magistrate_pretrial_risk_assessment <- fnc_convert_to_factors(raw_wv_magistrate_pretrial_risk_assessment, vars_to_convert)
dictionary_raw_wv_magistrate_pretrial_risk_assessment <- fnc_create_codebook(raw_wv_magistrate_pretrial_risk_assessment,
                                                                             hide_statistics = c("DefendantID", "CaseID"))


## WV Magistrate Court Services - Sentencing Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "SentenceDescription", "SentenceLengthUnit")
raw_wv_magistrate_sentencing <- fnc_convert_to_factors(raw_wv_magistrate_sentencing, vars_to_convert)
raw_wv_magistrate_sentencing <- raw_wv_magistrate_sentencing |>
  mutate(SentenceLengthUnit = as.character(SentenceLengthUnit))
dictionary_raw_wv_magistrate_sentencing <- fnc_create_codebook(raw_wv_magistrate_sentencing,
                                                               hide_statistics = c("DefendantID", "CaseID"))


## WV Magistrate Court Services - Transfer Appealed Remanded Circuit Docket Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "TransferredToCircuitCrt", "AppealedToCircuitCrt", "RemandedBackFromCircuitCrt")
raw_wv_magistrate_transfer_appealed <- fnc_convert_to_factors(raw_wv_magistrate_transfer_appealed, vars_to_convert)
raw_wv_magistrate_transfer_appealed <- raw_wv_magistrate_transfer_appealed |>
  mutate(TransferredToCircuitCrt = as.character(TransferredToCircuitCrt),
         RemandedBackFromCircuitCrt = as.character(RemandedBackFromCircuitCrt),
         AppealedToCircuitCrt = as.character(AppealedToCircuitCrt))
dictionary_raw_wv_magistrate_transfer_appealed <- fnc_create_codebook(raw_wv_magistrate_transfer_appealed,
                                                                      hide_statistics = c("DefendantID", "CaseID"))


## WVCircuit_Data_RequestCSG.xlsx
vars_to_convert <- c("Sex", "Race", "County", "Court", "Description")
raw_wv_circuit_data_request <- fnc_convert_to_factors(raw_wv_circuit_data_request, vars_to_convert)
dictionary_raw_wv_circuit_data_request <- fnc_create_codebook(raw_wv_circuit_data_request,
                                                              hide_statistics = c("Name", "CaseNumber", "Date of Birth"))


## WV Magistrate Court Services - Case, Defendant, Disposition Data.xlsx
vars_to_convert <- c("FilingYear", "County", "Court", "CaseType", "Gender", "Race",
                     "OriginalChrgLevel", "FinalChrgLevel", "ChargeType", "DispositionDescription")
raw_wv_magistrate_case_defendant_disposition <- fnc_convert_to_factors(raw_wv_magistrate_case_defendant_disposition, vars_to_convert)
dictionary_raw_wv_magistrate_case_defendant_disposition <- fnc_create_codebook(raw_wv_magistrate_case_defendant_disposition,
                                                                               hide_statistics = c("DefendantID", "CaseID",
                                                                                                   "Defendant", "DOB"))






#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS) {
  save(dictionary_raw_probation_drc_program_referral, file = file.path(folder, "dictionary_raw_probation_drc_program_referral.rds"))
  save(dictionary_raw_probation_ocms_financial_assessment, file = file.path(folder, "dictionary_raw_probation_ocms_financial_assessment.rds"))
  save(dictionary_raw_probation_ocms_offender_supervision, file = file.path(folder, "dictionary_raw_probation_ocms_offender_supervision.rds"))
  save(dictionary_raw_probation_ocms_risk_assessment, file = file.path(folder, "dictionary_raw_probation_ocms_risk_assessment.rds"))
  save(dictionary_raw_probation_ocms_sentencing, file = file.path(folder, "dictionary_raw_probation_ocms_sentencing.rds"))
  save(dictionary_raw_probation_ocms_substance_testing, file = file.path(folder, "dictionary_raw_probation_ocms_substance_testing.rds"))
  save(dictionary_raw_probation_ocms_supervision_violations, file = file.path(folder, "dictionary_raw_probation_ocms_supervision_violations.rds"))
  save(dictionary_raw_wv_magistrate_arrest, file = file.path(folder, "dictionary_raw_wv_magistrate_arrest.rds"))
  save(dictionary_raw_wv_magistrate_bailpiece_served, file = file.path(folder, "dictionary_raw_wv_magistrate_bailpiece_served.rds"))
  save(dictionary_raw_wv_magistrate_bond, file = file.path(folder, "dictionary_raw_wv_magistrate_bond.rds"))
  save(dictionary_raw_wv_magistrate_fee_assessment, file = file.path(folder, "dictionary_raw_wv_magistrate_fee_assessment.rds"))
  save(dictionary_raw_wv_magistrate_hearing, file = file.path(folder, "dictionary_raw_wv_magistrate_hearing.rds"))
  save(dictionary_raw_wv_magistrate_pretrial_risk_assessment, file = file.path(folder, "dictionary_raw_wv_magistrate_pretrial_risk_assessment.rds"))
  save(dictionary_raw_wv_magistrate_sentencing, file = file.path(folder, "dictionary_raw_wv_magistrate_sentencing.rds"))
  save(dictionary_raw_wv_magistrate_transfer_appealed, file = file.path(folder, "dictionary_raw_wv_magistrate_transfer_appealed.rds"))
  save(dictionary_raw_wv_circuit_data_request, file = file.path(folder, "dictionary_raw_wv_circuit_data_request.rds"))
  save(dictionary_raw_wv_magistrate_case_defendant_disposition, file = file.path(folder, "dictionary_raw_wv_magistrate_case_defendant_disposition.rds"))
}



















