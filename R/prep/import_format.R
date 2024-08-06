#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: June 27, 2024 (MAR)
# Description:
#    Import NCRP data (admissions, population, year end population)
#    Import BJS Prisoners data
#    Import Annual Parole Survey data
#    Prepares files for analysis
#######################################

#------ Background Info About States ------#

parole_info_by_state <- read.xlsx(paste0(config$sp_data_path,
                                         "/background/app/Parole Info by State.xlsx"),
                                  sheet = "Overall") |>
  clean_names()


#------ Shapefile for Map ------#

hex_gj <- read_sf(paste0(config$sp_data_path, "/data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(3857) |>
  sf_geojson() |>
  fromJSON(simplifyVector = FALSE)



#------ Robina Institute ------#

# robinainfo <- read.xlsx(paste0(config$sp_data_path, "/data/raw/Robina Institute/robinainfo.xlsx"),
#                         sheet = "classifications")
# robinadefinitions <- read.xlsx(paste0(config$sp_data_path, "/data/raw/Robina Institute/robinainfo.xlsx"),
#                                sheet = "definitions")

carl_state_notes <- read.xlsx(paste0(config$sp_data_path, "/data/raw/Carl State Notes/carl_state_notes.xlsx")) |>
  clean_names()



#------ NCRP Data ------#

ncrp_files <- list(
  term_records = "1",
  admissions = "2",
  releases = "3",
  yearendpop = "4"
)

ncrp_data <- lapply(ncrp_files, fnc_load_ncrp_data)
names(ncrp_data) <- names(ncrp_files)


#------ Prepare NCRP Term Records ------#

ncrp_term_records <- ncrp_data$term_records |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(sex:reltype, ~ str_sub(., 5, -1))) |>
  mutate(across(everything(), trimws))



#------ Prepare NCRP Admissions ------#

ncrp_admissions <- ncrp_data$admissions |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(c(sex, education, admtype, offgeneral, offdetail, race,
                  sentlgth, ageadmit), ~ str_sub(., 5, -1))) |>
  mutate(offdetail = trimws(offdetail)) |>
  fnc_create_admtype()



#------ Prepare NCRP Releases ------#

ncrp_releases <- ncrp_data$releases |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(c(offgeneral, offdetail, admtype, race, sex, ageadmit,
                  agerlse, sentlgth, reltype, timesrvd_rel, education), ~ str_sub(., 5, -1))) |>
  mutate(offdetail = trimws(offdetail)) |>
  fnc_create_parelig_status() |>
  fnc_create_fbi_index() |>
  mutate(
    time_between_admisson_release = relyr - admityr,
    time_between_ped_release = relyr - parelig_year,
    time_between_ped_release_category = case_when(
      time_between_ped_release < 0    ~ "Released before Parole Eligibility Year",
      time_between_ped_release == 0   ~ "Released on Parole Eligibility Year",
      time_between_ped_release <= 5 &
        time_between_ped_release > 0  ~ "Released 1 to 5 Years After Parole Eligibility Year",
      time_between_ped_release > 5    ~ "Released more than 5 Years After Parole Eligibility Year",
      is.na(time_between_ped_release) ~ "Missing Parole Eligibility Year") |>
      factor(levels = c("Released before Parole Eligibility Year",
                        "Released on Parole Eligibility Year",
                        "Released 1 to 5 Years After Parole Eligibility Year",
                        "Released more than 5 Years After Parole Eligibility Year",
                        "Missing Parole Eligibility Year"))) |>
  fnc_create_admtype() |>
  mutate(across(c(race, agerlse, sentlgth), ~ ifelse(is.na(.), "Unknown", .))) |>
  mutate(
    race = factor(race, levels = c("Unknown",
                                   "Other race(s), non-Hispanic",
                                   "White, non-Hispanic",
                                   "Hispanic, any race",
                                   "Black, non-Hispanic")),
    agerlse = factor(agerlse, levels = c("55+ years",
                                         "45-54 years",
                                         "35-44 years",
                                         "25-34 years",
                                         "18-24 years")),
    sentlgth = factor(sentlgth, levels = c("< 1 year",
                                           "1-1.9 years",
                                           "2-4.9 years",
                                           "5-9.9 years",
                                           "10-24.9 years",
                                           ">=25 years",
                                           "Life, LWOP, Life plus additional years, Death",
                                           "Unknown")))


#------ Prepare NCRP Year End Pop ------#

ncrp_yearendpop <- ncrp_data$yearendpop |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(c(offgeneral, offdetail, race, education, admtype, sex, sentlgth, ageadmit, ageyrend, timesrvd_yrend), ~ str_sub(., 5, -1))) |>
  mutate(
    offdetail = trimws(offdetail),
    offgeneral = case_when(
      is.na(offgeneral) ~ "Other or Unknown",
      offgeneral == "Other/unspecified" ~ "Other or Unknown",
      TRUE ~ offgeneral
    )
  ) |>
  fnc_create_fbi_index() |>
  fnc_create_parelig_status() |>
  fnc_create_admtype() |>
  mutate(
    race = ifelse(is.na(race), "Unknown", race),
    ageyrend = ifelse(is.na(ageyrend), "Unknown", ageyrend),
    sentlgth = ifelse(is.na(sentlgth), "Unknown", sentlgth)
  ) |>
  mutate(
    race = factor(race,
                  levels = c("Unknown",
                             "Other race(s), non-Hispanic",
                             "White, non-Hispanic",
                             "Hispanic, any race",
                             "Black, non-Hispanic")),
    ageyrend = factor(ageyrend,
                      levels = c("55+ years",
                                 "45-54 years",
                                 "35-44 years",
                                 "25-34 years",
                                 "18-24 years")),
    sentlgth = factor(sentlgth,
                      levels = c(
                        "< 1 year",
                        "1-1.9 years",
                        "2-4.9 years",
                        "5-9.9 years",
                        "10-24.9 years",
                        ">=25 years",
                        "Life, LWOP, Life plus additional years, Death",
                        "Unknown")))



#------ Import and Prepare BJS Race, Ethnicity, Gender Data ------#

bjs_prison_pop_by_race_state_2020 <- read.csv(paste0(config$sp_data_path,
                                                     "/data/raw/BJS Prison Pop/p20st/p20stat02.csv"), skip = 10)

# Define the list of filenames and corresponding column indices
file_info <- list(
  "2010" = list(file = "p10/p10at01.csv", col = "x_3"),
  "2011" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_1"),
  "2012" = list(file = "p12tar9112/p12tar9112at06.csv", col = "x_5"),
  "2013" = list(file = "p13/p13t02.csv", col = "x_5"),
  "2014" = list(file = "p14/CSV tables/p14t02.csv", col = "x_5"),
  "2015" = list(file = "p15/p15t02.csv", col = "x_6"),
  "2016" = list(file = "p16/p16t02.csv", col = "x_5"),
  "2017" = list(file = "p17/p17t02.csv", col = "x_5"),
  "2018" = list(file = "p18/p18t02.csv", col = "x_5"),
  "2019" = list(file = "p19/p19t02.csv", col = "x_5"),
  "2020" = list(file = "p20st/p20stt02.csv", col = "x_4"),
  "2021" = list(file = "p21st/p21stt02.csv", col = "x_4")
)

# Initialize an empty list to store the cleaned data
cleaned_data_list <- list()

# Loop through the file information to read, process, and store the data
for (year in names(file_info)) {
  file_path <- paste0(config$sp_data_path, "/data/raw/BJS Prison Pop/", file_info[[year]]$file)
  col_name <- file_info[[year]]$col

  # Read and process the data
  df <- read.csv(file_path) |>
    clean_names() |>
    select(state = x, bjs_prison_population = !!sym(col_name)) |>
    fnc_clean_bjs_data() |>
    mutate(rptyear = as.numeric(year))

  # Append the cleaned data to the list
  cleaned_data_list[[year]] <- df
}

# Combine all years' data into a single dataframe
bjs_prison_pop_by_rptyear <- do.call(rbind, cleaned_data_list)




#------ Import Annual Parole Survey ------#

# get state abbreviations and state names
state_names_abb <- data.frame(abbreviation = state.abb,
                              name = state.name,
                              stringsAsFactors = FALSE) %>%
  rename(state = name, stateid = abbreviation)

# aps_files <- list(
#   list(file = "ICPSR_38058-V1/ICPSR_38058/DS0001/38058-0001-Data.rda", year = 2018),
#   list(file = "ICPSR_37471-V1/ICPSR_37471/DS0001/37471-0001-Data.rda", year = 2017),
#   list(file = "ICPSR_37441-V1/ICPSR_37441/DS0001/37441-0001-Data.rda", year = 2016),
#   list(file = "ICPSR_36619-V1/ICPSR_36619/DS0001/36619-0001-Data.rda", year = 2015),
#   list(file = "ICPSR_36320-V1/ICPSR_36320/DS0001/36320-0001-Data.rda", year = 2014),
#   list(file = "ICPSR_35629-V1/ICPSR_35629/DS0001/35629-0001-Data.rda", year = 2013),
#   list(file = "ICPSR_35257-V1/ICPSR_35257/DS0001/35257-0001-Data.rda", year = 2012),
#   list(file = "ICPSR_34718-V1/ICPSR_34718/DS0001/34718-0001-Data.rda", year = 2011),
#   list(file = "ICPSR_34382-V1/ICPSR_34382/DS0001/34382-0001-Data.rda", year = 2010),
#   list(file = "ICPSR_34381-V1/ICPSR_34381/DS0001/34381-0001-Data.rda", year = 2009),
#   list(file = "ICPSR_34380-V1/ICPSR_34380/DS0001/34380-0001-Data.rda", year = 2008),
#   list(file = "ICPSR_31332-V1/ICPSR_31332/DS0001/31332-0001-Data.rda", year = 2007),
#   list(file = "ICPSR_31331-V1/ICPSR_31331/DS0001/31331-0001-Data.rda", year = 2006),
#   list(file = "ICPSR_31330-V1/ICPSR_31330/DS0001/31330-0001-Data.rda", year = 2005),
#   list(file = "ICPSR_31329-V1/ICPSR_31329/DS0001/31329-0001-Data.rda", year = 2004),
#   list(file = "ICPSR_31328-V1/ICPSR_31328/DS0001/31328-0001-Data.rda", year = 2003),
#   list(file = "ICPSR_31327-V1/ICPSR_31327/DS0001/31327-0001-Data.rda", year = 2002),
#   list(file = "ICPSR_31326-V1/ICPSR_31326/DS0001/31326-0001-Data.rda", year = 2001),
#   list(file = "ICPSR_31325-V1/ICPSR_31325/DS0001/31325-0001-Data.rda", year = 2000)
# )
#
# aps_data_list <- lapply(aps_files, function(f) {
#   load(paste0(config$sp_data_path, "/data/raw/Annual Parole Survey/", f$file))
#   get(ls()[1])
# })
# names(aps_data_list) <- paste0("aps_", sapply(aps_files, function(f) f$year))
#
# aps_years <- sapply(aps_files, function(f) f$year)
# aps_pre_2008 <- aps_years < 2008
#
# aps_parole_by_rptyear <- mapply(
#   fnc_prepare_aps_data,
#   data = aps_data_list,
#   year = aps_years,
#   pre_2008 = aps_pre_2008,
#   SIMPLIFY = FALSE) |> bind_rows() |>
#   filter(!state %in% c("District of Columbia", "Federal") & !is.na(state))
#


#------ Prepare BJS: Prisoners in 2020 ------#

total_bjs_pop_2020 <- bjs_prison_pop_by_race_state_2020 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(x, total) |>
  rename(state = x) |>
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

# Warning OK - characters like '~' turned to NA
bjs_prison_pop_by_race_2020 <- bjs_prison_pop_by_race_state_2020 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(-jurisdiction) |>
  rename(state = x) |>
  mutate(across(everything(), ~str_replace_all(., ",", ""))) |>
  mutate(across(-state, as.numeric)) |>
  pivot_longer(cols = total:did_not_report,
               names_to = "race",
               values_to = "n") |>
  mutate(race = case_when(
    race == "total" ~ "Total Population",
    race == "white_a" ~ "White, non-Hispanic",
    race == "black_a" ~ "Black, non-Hispanic",
    race == "hispanic" ~ "Hispanic, any race",
    race %in% c("american_indian_alaska_native_a",
                "asian_a",
                "native_hawaiian_other_pacific_islander_a",
                "two_or_more_races_a",
                "other_a") ~ "Other race(s), non-Hispanic",
    race == "unknown" ~ "Unknown",
    race == "did_not_report" ~ "Unknown",
    TRUE ~ race
  )) |>
  filter(race != "Unknown" & race != "Total Population") |>
  group_by(state, race) |>
  summarise(n = sum(n, na.rm = TRUE)) |>
  left_join(total_bjs_pop_2020, by = "state") |>
  ungroup() |>
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") |>
  select(-total)



#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(ncrp_yearendpop,                    file = file.path(folder, "ncrp_yearendpop.rds"))
  save(ncrp_admissions,                    file = file.path(folder, "ncrp_admissions.rds"))
  save(ncrp_term_records,                  file = file.path(folder, "ncrp_term_records.rds"))
  save(ncrp_releases,                      file = file.path(folder, "ncrp_releases.rds"))
  #save(aps_parole_by_rptyear,              file = file.path(folder, "aps_parole_by_rptyear.rds"))
  save(bjs_prison_pop_by_race_2020,        file = file.path(folder, "bjs_prison_pop_by_race_2020.rds"))
  save(bjs_prison_pop_by_rptyear,          file = file.path(folder, "bjs_prison_pop_by_rptyear.rds"))

  save(hex_gj,                             file = file.path(folder, "hex_gj.rds"))
  save(robinadefinitions,                  file = file.path(folder, "robinadefinitions.rds"))
  save(robinainfo,                         file = file.path(folder, "robinainfo.rds"))
  save(carl_state_notes,                   file = file.path(folder, "carl_state_notes.rds"))
  save(parole_info_by_state,               file = file.path(folder, "parole_info_by_state.rds"))

}

