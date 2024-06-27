#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Import NCRP data (admissions, population, year end population)
#    Import BJS Prisoners data
#    Import Annual Parole Survey data
#######################################

#------ Helper Functions ------#

#' Load NCRP Data
#'
#' This function loads NCRP data for a given file ID.
#'
#' @param file_id A string value representing the file ID of the data to be loaded.
#' @return The loaded data object.
#' @export
fnc_load_ncrp_data <- function(file_id) {
  file_path <- paste0(config$sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/", file_id, "/38492-", file_id, "-Data.rda")
  if (file.exists(file_path)) {
    load(file_path)
    return(get(ls()[ls() != "file_path"]))  # Return the loaded data object
  } else {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
}


#' Load BJS Prisoners Data by Gender
#'
#' This function loads BJS prisoners data by gender for given years.
#'
#' @param year A string representing the year of the data to load.
#' @param subfolder A string representing the subfolder containing the data.
#' @param file_name A string representing the file name of the data.
#' @return A data frame with the loaded data, or NULL if the file does not exist.
#' @export
fnc_load_bjs_prison_data <- function(year, subfolder, file_name) {
  file_path <- file.path(config$sp_data_path, "data/raw", subfolder, file_name)
  if (file.exists(file_path)) {
    return(read.csv(file_path))
  } else {
    warning(paste("File for year", year, "not found:", file_path))
    return(NULL)
  }
}


#' Load Annual Parole Survey Data
#'
#' This function loads Annual Parole Survey data for a given year and ICPSR code.
#'
#' @param year A string representing the year of the data to load.
#' @param icpsr_code A string representing the ICPSR code for the data.
#' @return The loaded data as a data frame.
#' @export
fnc_load_aps_data <- function(year, icpsr_code) {
  file_path <- file.path(config$sp_data_path, "data/raw/ICPSR_", icpsr_code, "-V1/ICPSR_", icpsr_code, "/DS0001/", paste0(icpsr_code, "-0001-Data.rda"))
  if (file.exists(file_path)) {
    loaded_object_name <- load(file_path)
    get(loaded_object_name)
  } else {
    stop(paste("File not found:", file_path))
  }
}





#######################################

# Import Data

#######################################

#------ Import NCRP Data ------#

ncrp_files <- list(
  term_records = "da38492.0001",
  admissions = "da38492.0002",
  releases = "da38492.0003",
  yearendpop = "da38492.0004"
)

lapply(seq_along(ncrp_files), fnc_load_ncrp_data)


#------ Import Robina Institute Data ------#

robinainfo <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"),
                        sheet = "classifications")
robinadefinitions <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"),
                               sheet = "definitions")
robinaparoleeligibility <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"),
                                     sheet = "eligibility")


#------ Import BJS Race, Ethnicity, Gender Data ------#

bjs_prison_pop_by_race_state_2020 <- read.csv(paste0(sp_data_path,
                                                     "/data/raw/p20st/p20stat02.csv"), skip = 10)

file_info <- list(
  "2010" = list(subfolder = "p10", file_name = "p10at01.csv"),
  "2012" = list(subfolder = "p12tar9112", file_name = "p12tar9112at06.csv"),
  "2013" = list(subfolder = "p13", file_name = "p13t02.csv"),
  "2014" = list(subfolder = "p14/CSV tables", file_name = "p14t02.csv"),
  "2015" = list(subfolder = "p15", file_name = "p15t02.csv"),
  "2016" = list(subfolder = "p16", file_name = "p16t02.csv"),
  "2017" = list(subfolder = "p17", file_name = "p17t02.csv"),
  "2018" = list(subfolder = "p18", file_name = "p18t02.csv"),
  "2019" = list(subfolder = "p19", file_name = "p19t02.csv"),
  "2020" = list(subfolder = "p20st", file_name = "p20stt02.csv"),
  "2021" = list(subfolder = "p21st", file_name = "p21stt02.csv")
)

bjs_prison_pop_by_gender_state <- lapply(names(file_info), function(year) {
  info <- file_info[[year]]
  fnc_load_bjs_prison_data(year, info$subfolder, info$file_name)
})

bjs_prison_pop_by_gender_state <- Filter(Negate(is.null),
                                         bjs_prison_pop_by_gender_state)


#------ Import Annual Parole Survey ------#

aps_data_info <- data.frame(
  year = 2000:2018,
  icpsr_code = c("31325", "31326", "31327", "31328", "31329", "31330", "31331",
                 "31332", "34380", "34381", "34382", "34718", "35257", "35629",
                 "36320", "36619", "37441", "37471", "38058")
)

aps_parole_list <- lapply(1:nrow(aps_data_info), function(i) {
  fnc_load_aps_data(aps_data_info$year[i], aps_data_info$icpsr_code[i])
})

names(aps_parole_list) <- paste0("aps_parole_", aps_data_info$year)


#------ Import Shapefiles ------#

hex <- read_sf(paste0(sp_data_path, "/data/raw/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC")



