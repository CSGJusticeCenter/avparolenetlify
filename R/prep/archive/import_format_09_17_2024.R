#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: September 11, 2024 (MAR)
# Description:
#    Import NCRP data (admissions, population, year end population)
#    Import BJS Prisoners data
#    Import Annual Parole Survey data
#    Prepares files for analysis
#######################################

#------ Background Info About States ------------------------------------------#

parole_info_by_state <- read.xlsx(paste0(config$sp_data_path,
                                         "/background/app/Parole Info by State.xlsx"),
                                  sheet = "Overall") |>
  clean_names()


#------ Shapefile for Map -----------------------------------------------------#

hex_gj <- read_sf(paste0(config$sp_data_path, "/data/raw/Shapefiles/us_states_hexgrid.geojson")) |>
  select(state_abb = iso3166_2) |>
  filter(state_abb != "DC") |>
  st_transform(3857) |>
  sf_geojson() |>
  fromJSON(simplifyVector = FALSE)



#------ Robina Institute/Carl Notes -------------------------------------------#

carl_state_notes <- read.xlsx(paste0(config$sp_data_path, "/data/raw/Carl State Notes/carl_state_notes.xlsx")) |>
  clean_names()



#------ NCRP Data -------------------------------------------------------------#

ncrp_files <- list(
  term_records = "1",
  admissions = "2",
  releases = "3",
  yearendpop = "4"
)

ncrp_data <- lapply(ncrp_files, fnc_load_ncrp_data)
names(ncrp_data) <- names(ncrp_files)


#------ Prepare NCRP Term Records ---------------------------------------------#

# Takes a couple minutes to run
ncrp_term_records <- ncrp_data$term_records |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(sex:reltype, ~ str_sub(., 5, -1))) |>
  mutate(across(everything(), trimws))



#------ Prepare NCRP Releases -------------------------------------------------#

ncrp_releases <- ncrp_data$releases |>
  clean_names() |>
  mutate(across(c(state), ~ str_sub(., 6, -1))) |>
  mutate(across(c(offgeneral, offdetail, admtype, race, sex, ageadmit,
                  agerlse, sentlgth, reltype, timesrvd_rel, education), ~ str_sub(., 5, -1))) |>
  mutate(offdetail = trimws(offdetail)) |>
  mutate(across(c(race, agerlse, sex, sentlgth), ~ ifelse(is.na(.), "Unknown", .))) |>
  fnc_create_parelig_status() |>
  fnc_create_fbi_index() |>
  fnc_create_admtype() |>
  mutate(
    time_between_admisson_release = relyr - admityr,
    time_between_ped_release = relyr - parelig_year,
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
  mutate(across(c(offgeneral, offdetail, race, education, admtype, sex,
                  sentlgth, ageadmit, ageyrend, timesrvd_yrend), ~ str_sub(., 5, -1))) |>
  mutate(offdetail = trimws(offdetail)) |>
  fnc_create_fbi_index() |>
  fnc_create_parelig_status() |>
  fnc_create_admtype() |>
  mutate(across(c(race, ageyrend, sex, sentlgth), ~ ifelse(is.na(.), "Unknown", .))) |>
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





#------ Import and Prepare BJS Race, Ethnicity, Sex Data -------------------#

bjs_prison_pop_by_race_state_2020 <- read.csv(paste0(config$sp_data_path,
                                                     "/data/raw/BJS Prison Pop/p20st/p20stat02.csv"), skip = 10)
bjs_prison_pop_by_race_state_2022 <- read.csv(paste0(config$sp_data_path,
                                                     "/data/raw/BJS Prison Pop/p22st/p22stat01.csv"), skip = 10)

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
  "2021" = list(file = "p21st/p21stt02.csv", col = "x_4"),
  "2022" = list(file = "p22st/p22stt02.csv", col = "x_1")
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

# Total pop in 2020
total_bjs_pop_2020 <- bjs_prison_pop_by_race_state_2020 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(x, total) |>
  rename(state = x) |>
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

# Pop by Race and Ethnicity
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

# Total pop in 2022
total_bjs_pop_2022 <- bjs_prison_pop_by_race_state_2022 |>
  clean_names() |>
  filter(jurisdiction == "") |>
  select(x, total) |>
  rename(state = x) |>
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

# Pop by Race and Ethnicity
# Warning OK - characters like '~' turned to NA
bjs_prison_pop_by_race_2022 <- bjs_prison_pop_by_race_state_2022 |>
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
  left_join(total_bjs_pop_2022, by = "state") |>
  ungroup() |>
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") |>
  select(-total)|>
  mutate(state = str_replace(state, "/.*", ""))


bjs_prison_pop_by_sex_2022_raw <- read_csv("C:/Users/mroberts/The Council of State Governments/JC Research - Documents/RES_Parole/data/raw/BJS Prison Pop/p22st/p22stt02.csv")

bjs_prison_pop_by_sex_2022 <- bjs_prison_pop_by_sex_2022_raw  |>
  clean_names() |>
  select(state = x2, male = x8, female = x9) |>
  mutate(state = str_replace(state, "/.*", "")) %>%
  mutate(state = str_replace(state, "Alaskab", "Alaska")) %>%
  mutate(state = str_replace(state, "Utahc", "Utah")) %>%
  filter(state != "" &
           state != "State" &
           state != "Federal" &
           state != "District of Columbia" &
           state != "U.S. Total" &
           state != "U.S. total" &
           state != "U.S. tota") |>
  mutate(male = str_replace_all(male, "[^\\d]", "")) |>
  mutate(male = as.numeric(male)) |>
  mutate(female = str_replace_all(female, "[^\\d]", "")) |>
  mutate(female = as.numeric(female)) |>
  pivot_longer(cols = c(male, female), names_to = "sex", values_to = "population") |>
  group_by(state) |>
  mutate(
    n = population,
    prop = population/sum(population),
    yearendpop_ped = sum(population),
    prop_label = paste0(round(prop*100, 0), "%"),
    n_label = formattable::comma(population, 0)
  ) |>
  ungroup() |>
  mutate(tooltip = paste0("<b>", state, " - ",
                          sex, "</b><br>",
                          prop_label, "<br>")) |>
  mutate(sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female",
                         TRUE ~ sex))


#------ Save Data ------#

theseFOLDERS <- c("sharepoint" = paste0(config$sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(ncrp_yearendpop,                    file = file.path(folder, "ncrp_yearendpop.rds"))
  save(ncrp_term_records,                  file = file.path(folder, "ncrp_term_records.rds"))
  save(ncrp_releases,                      file = file.path(folder, "ncrp_releases.rds"))
  save(bjs_prison_pop_by_race_2020,        file = file.path(folder, "bjs_prison_pop_by_race_2020.rds"))
  save(bjs_prison_pop_by_race_2022,        file = file.path(folder, "bjs_prison_pop_by_race_2022.rds"))
  save(bjs_prison_pop_by_sex_2022,         file = file.path(folder, "bjs_prison_pop_by_sex_2022.rds"))
  save(bjs_prison_pop_by_rptyear,          file = file.path(folder, "bjs_prison_pop_by_rptyear.rds"))

  save(hex_gj,                             file = file.path(folder, "hex_gj.rds"))
  save(carl_state_notes,                   file = file.path(folder, "carl_state_notes.rds"))
  save(parole_info_by_state,               file = file.path(folder, "parole_info_by_state.rds"))

}

