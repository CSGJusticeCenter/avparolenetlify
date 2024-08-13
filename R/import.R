# Define the raw_aoc_subdirectory to be added to config$sp_data_path
raw_aoc_subdirectory <- "data/raw/AOC"

raw_probation_drc_program_referral <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_DRC Program Referral Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_financial_assessment <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Financial Assessment Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_offender_supervision <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Offender and Supervision Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_risk_assessment <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Risk Assessment Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_sentencing <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Sentencing Data 2013-2023.xlsx"), skip = 3)
raw_probation_ocms_substance_testing <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Substance Testing Data 2013-2023 do not send.xlsx"), skip = 3)
raw_probation_ocms_supervision_violations <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "Probation_OCMS Supervision Violations 2013-2023.xlsx"), skip = 3)

raw_wv_magistrate_arrest <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Arrest Data.xlsx"), skip = 3)
raw_wv_magistrate_bailpiece_served <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Bailpiece Served Data.xlsx"), skip = 3)
raw_wv_magistrate_bond <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Bond Data.xlsx"), skip = 3)
raw_wv_magistrate_fee_assessment <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Fee  Assessment Data.xlsx"), skip = 3)
raw_wv_magistrate_hearing <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Hearing Data.xlsx"), skip = 3)
raw_wv_magistrate_pretrial_risk_assessment <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Pretrial Risk Assessment Data.xlsx"), skip = 3)
raw_wv_magistrate_sentencing <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Sentencing Data.xlsx"), skip = 3)
raw_wv_magistrate_transfer_appealed <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Transfer Appealed Remanded Circuit Docket Data.xlsx"), skip = 3)

raw_wv_circuit_data_request <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WVCircuit_Data_RequestCSG.xlsx"), skip = 3)
raw_wv_magistrate_case_defendant_disposition <- read_excel(file.path(config$sp_data_path, raw_aoc_subdirectory, "WV Magistrate Court Services - Case, Defendant, Disposition Data.xlsx"), skip = 3)


# raw_wv_magistrate_case_defendant_disposition$Defendant ### NAME
# raw_wv_magistrate_case_defendant_disposition$DefendantID
# raw_wv_magistrate_case_defendant_disposition$DOB
#
# raw_probation_ocms_offender_supervision$NameComposite  ### NAME
# raw_probation_ocms_offender_supervision$DOB
# raw_probation_ocms_offender_supervision$OffenderID



