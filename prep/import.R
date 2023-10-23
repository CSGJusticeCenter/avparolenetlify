#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: October 5, 2023 (MAR)
# Description:
#    Import NCRP data (admissions, population, year end population)
#######################################

# load packages and functions
source("prep/library.R")
source("prep/functions.R")

# load prison sentencing system info from Robina
robinainfo              <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"), sheet = "classifications")
robinadefinitions       <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"), sheet = "definitions")
robinaparoleeligibility <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"), sheet = "eligibility")

# load NCRP data
# https://www.icpsr.umich.edu/web/NACJD/studies/38492
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0001/38492-0001-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0002/38492-0002-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0003/38492-0003-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0004/38492-0004-Data.rda"))

# load Prisoners in 2020 - Statistical Tables
# Using this info for RRI's bc there is more race/ethnicity data
# https://bjs.ojp.gov/library/publications/prisoners-2020-statistical-tables
bjs_prison_pop_by_race_state_2020 <- read.csv(paste0(sp_data_path, "/data/raw/p20st/p20stat02.csv"), skip = 10)

# load Prisoners data from 2010-2021
# CHECK TO MAKE SURE THESE ARE THE RIGHT FILES
bjs_prison_pop_by_gender_state_2010.csv <- read.csv(paste0(sp_data_path, "/data/raw/p10/p10at01.csv"))
# bjs_prison_pop_by_gender_state_2011.csv <- read.csv(paste0(sp_data_path, "/data/raw/p11/p11at01.csv"))
bjs_prison_pop_by_gender_state_2012.csv <- read.csv(paste0(sp_data_path, "/data/raw/p12tar9112/p12tar9112at06.csv"))
bjs_prison_pop_by_gender_state_2013.csv <- read.csv(paste0(sp_data_path, "/data/raw/p13/p13t02.csv"))
bjs_prison_pop_by_gender_state_2014.csv <- read.csv(paste0(sp_data_path, "/data/raw/p14/CSV tables/p14t02.csv"))
bjs_prison_pop_by_gender_state_2015.csv <- read.csv(paste0(sp_data_path, "/data/raw/p15/p15t02.csv"))
bjs_prison_pop_by_gender_state_2016.csv <- read.csv(paste0(sp_data_path, "/data/raw/p16/p16t02.csv"))
bjs_prison_pop_by_gender_state_2017.csv <- read.csv(paste0(sp_data_path, "/data/raw/p17/p17t02.csv"))
bjs_prison_pop_by_gender_state_2018.csv <- read.csv(paste0(sp_data_path, "/data/raw/p18/p18t02.csv"))
bjs_prison_pop_by_gender_state_2019.csv <- read.csv(paste0(sp_data_path, "/data/raw/p19/p19t02.csv"))
bjs_prison_pop_by_gender_state_2020.csv <- read.csv(paste0(sp_data_path, "/data/raw/p20st/p20stt02.csv"))
bjs_prison_pop_by_gender_state_2021.csv <- read.csv(paste0(sp_data_path, "/data/raw/p21st/p21stt02.csv"))


# load Annual Parole Survey Series
# https://www.icpsr.umich.edu/web/NACJD/studies/38058
# from 2000 to 2018 (most recent)
aps_parole_2018 <- load(paste0(sp_data_path, "/data/raw/ICPSR_38058-V1/ICPSR_38058/DS0001/38058-0001-Data.rda"))
aps_parole_2017 <- load(paste0(sp_data_path, "/data/raw/ICPSR_37471-V1/ICPSR_37471/DS0001/37471-0001-Data.rda"))
aps_parole_2016 <- load(paste0(sp_data_path, "/data/raw/ICPSR_37441-V1/ICPSR_37441/DS0001/37441-0001-Data.rda"))
aps_parole_2015 <- load(paste0(sp_data_path, "/data/raw/ICPSR_36619-V1/ICPSR_36619/DS0001/36619-0001-Data.rda"))
aps_parole_2014 <- load(paste0(sp_data_path, "/data/raw/ICPSR_36320-V1/ICPSR_36320/DS0001/36320-0001-Data.rda"))
aps_parole_2013 <- load(paste0(sp_data_path, "/data/raw/ICPSR_35629-V1/ICPSR_35629/DS0001/35629-0001-Data.rda"))
aps_parole_2012 <- load(paste0(sp_data_path, "/data/raw/ICPSR_35257-V1/ICPSR_35257/DS0001/35257-0001-Data.rda"))
aps_parole_2011 <- load(paste0(sp_data_path, "/data/raw/ICPSR_34718-V1/ICPSR_34718/DS0001/34718-0001-Data.rda"))
aps_parole_2010 <- load(paste0(sp_data_path, "/data/raw/ICPSR_34382-V1/ICPSR_34382/DS0001/34382-0001-Data.rda"))
aps_parole_2009 <- load(paste0(sp_data_path, "/data/raw/ICPSR_34381-V1/ICPSR_34381/DS0001/34381-0001-Data.rda"))
aps_parole_2008 <- load(paste0(sp_data_path, "/data/raw/ICPSR_34380-V1/ICPSR_34380/DS0001/34380-0001-Data.rda"))
aps_parole_2007 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31332-V1/ICPSR_31332/DS0001/31332-0001-Data.rda"))
aps_parole_2006 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31331-V1/ICPSR_31331/DS0001/31331-0001-Data.rda"))
aps_parole_2005 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31330-V1/ICPSR_31330/DS0001/31330-0001-Data.rda"))
aps_parole_2004 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31329-V1/ICPSR_31329/DS0001/31329-0001-Data.rda"))
aps_parole_2003 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31328-V1/ICPSR_31328/DS0001/31328-0001-Data.rda"))
aps_parole_2002 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31327-V1/ICPSR_31327/DS0001/31327-0001-Data.rda"))
aps_parole_2001 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31326-V1/ICPSR_31326/DS0001/31326-0001-Data.rda"))
aps_parole_2000 <- load(paste0(sp_data_path, "/data/raw/ICPSR_31325-V1/ICPSR_31325/DS0001/31325-0001-Data.rda"))

# load sp file
hex <- read_sf(paste0(sp_data_path, "/data/raw/us_states_hexgrid.geojson")) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC")

# load info on states that abolished parole
parole_info_by_state <-
  read.xlsx(paste0(sp_data_path, "/background/app/Parole Info by State.xlsx"),
            sheet = "Overall")





#############
# Prepare NCRP Term Records
#############

ncrp_term_records <- da38492.0001 %>% clean_names() %>%
  mutate(
    state        = str_sub(state, 6, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    race         = str_sub(race, 5, -1),
    sex          = str_sub(sex, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1),
    education    = str_sub(education, 5, -1),
    timesrvd     = str_sub(timesrvd, 5, -1),
    reltype      = str_sub(reltype, 5, -1),
    ageadmit     = str_sub(ageadmit, 5, -1),
    agerelease   = str_sub(agerelease, 5, -1)) %>%

  mutate(across(everything(), ~ trimws(.)))



#############
# Prepare NCRP Admissions
#############

ncrp_admissions <- da38492.0002 %>% clean_names() %>%

  mutate(
    sex          = str_sub(sex, 5, -1),
    state        = str_sub(state, 6, -1),
    education    = str_sub(education, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    race         = str_sub(race, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1),
    ageadmit     = str_sub(ageadmit, 5, -1)) %>%

  mutate(offdetail = trimws(offdetail))



#############
# Prepare NCRP Releases
#############

ncrp_releases   <- da38492.0003 %>% clean_names() %>%
  mutate(
    state        = str_sub(state, 6, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    race         = str_sub(race, 5, -1),
    sex          = str_sub(sex, 5, -1),
    ageadmit     = str_sub(ageadmit, 5, -1),
    agerlse      = str_sub(agerlse, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1),
    reltype      = str_sub(reltype, 5, -1),
    timesrvd_rel = str_sub(timesrvd_rel, 5, -1),
    education    = str_sub(education, 5, -1)) %>%

  mutate(offdetail = trimws(offdetail)) %>%

  # create parole eligibility status with custom function
  fnc_create_parelig_status() %>%

  # create new offense descriptions
  fnc_create_fbi_index() %>%

  # calculate timing of release by parole eligibility date (year)
  mutate(time_between_admisson_release = relyr - admityr,
         time_between_ped_release = relyr - parelig_year,
         time_between_ped_release_category = case_when(
           time_between_ped_release < 0     ~ "Released Before Parole Eligibility Year",
            time_between_ped_release == 0   ~ "Released at Parole Eligibility Year",
           time_between_ped_release <= 5 &
             time_between_ped_release > 0   ~ "Released 1-5 Years After Parole Eligibility Year",
           time_between_ped_release > 5     ~ "Released 5 Years After Parole Eligibility Year",
           is.na(time_between_ped_release)  ~ "Missing Parole Eligibility Year"
         )) %>%
  mutate(time_between_ped_release_category =
           factor(time_between_ped_release_category,
                       levels = c("Released Before Parole Eligibility Year",
                                  "Released at Parole Eligibility Year",
                                  "Released 1-5 Years After Parole Eligibility Year",
                                  "Released 5 Years After Parole Eligibility Year",
                                  "Missing Parole Eligibility Year"))) %>%

  # Calculate time served vs original sentence length
  mutate(sentlgth_avg <- case_when(
    sentlgth == "< 1 year"      ~ 0.5,
    sentlgth == "1-1.9 years"   ~ 1.5,
    sentlgth == "2-4.9 years"   ~ 3.5,
    sentlgth == "5-9.9 years"   ~ 7.5,
    sentlgth == "10-24.9 years" ~ 17.5
    # >=25 years
  )) %>%

  # include unknown race in analysis
  # include unknown admission type in analysis???
  # create age categories
  fnc_create_admtype() %>%
  mutate(race     = ifelse(is.na(race), "Unknown", race),
         agerlse  = ifelse(is.na(agerlse), "Unknown", agerlse),
         sentlgth = ifelse(is.na(sentlgth), "Unknown", sentlgth)) %>%

  mutate(race = factor(race,
                       levels = c("Unknown",
                                  "Other race(s), non-Hispanic",
                                  "White, non-Hispanic",
                                  "Hispanic, any race",
                                  "Black, non-Hispanic")),
         agerlse  = factor(agerlse,
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






#############
# Prepare NCRP Year End Population
#############

ncrp_yearendpop <- da38492.0004 %>% clean_names() %>%
  mutate(
    state          = str_sub(state, 6, -1),
    offgeneral     = str_sub(offgeneral, 5, -1),
    offdetail      = str_sub(offdetail, 5, -1),
    race           = str_sub(race, 5, -1),
    education    = str_sub(education, 5, -1),
    admtype        = str_sub(admtype, 5, -1),
    sex            = str_sub(sex, 5, -1),
    sentlgth       = str_sub(sentlgth, 5, -1),
    ageadmit       = str_sub(ageadmit, 5, -1),
    ageyrend       = str_sub(ageyrend, 5, -1),
    timesrvd_yrend = str_sub(timesrvd_yrend, 5, -1)) %>%

  mutate(offdetail = trimws(offdetail),
         offgeneral = case_when(
           is.na(offgeneral) ~ "Other or Unknown",
           offgeneral == "Other/unspecified" ~ "Other or Unknown",
           TRUE ~ offgeneral)) %>%

  # create new offense descriptions
  fnc_create_fbi_index() %>%

  # create parole eligibility status
  fnc_create_parelig_status() %>%

  # include unknown race in analysis
  # include unknown admission type in analysis???
  # create age categories
  fnc_create_admtype() %>%
  mutate(race = ifelse(is.na(race), "Unknown", race),
         ageyrend = ifelse(is.na(ageyrend), "Unknown", ageyrend),
         sentlgth = ifelse(is.na(sentlgth), "Unknown", sentlgth)) %>%

  mutate(race = factor(race,
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







##########
# Prepare BJS: Prisoners in 2020
# This file is broken down by race
##########

# clean up file to create dataframe of state prison pop by race
# NAs generated are for cells that had / or ~ and are now NA
total_pop <- bjs_prison_pop_by_race_state_2020 %>%
  clean_names() %>%
  filter(jurisdiction == "") %>%
  select(x, total) %>%
  rename(state = x) %>%
  mutate(total = str_replace_all(total, ",", ""),
         total = as.numeric(total))

bjs_prison_pop_by_race <- bjs_prison_pop_by_race_state_2020 %>%
  clean_names() %>%
  filter(jurisdiction == "") %>%
  select(-c(jurisdiction)) %>%
  rename(state = x) %>%
  mutate_all(~str_replace_all(., ",", "")) %>%
  mutate(across(-state, as.numeric)) %>%
  pivot_longer(cols = total:did_not_report,
               names_to = "race",
               values_to = "n") %>%
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
  )) %>%
  filter(race != "Unknown" & race != "Total Population") %>%
  group_by(state, race) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  left_join(total_pop, by = "state") %>%
  ungroup() %>%
  mutate(prop = n / total,
         prop_label = paste0(round(prop*100, 0), "%"),
         n_label = formattable::comma(n, 0),
         population_type = "In Prison") %>%
  select(-total)

# # select variable for prison population only
# bjs_prison_pop_by_state <- bjs_prison_pop_by_race_state %>%
#   select(state, bjs_total_prison_population = total) %>%









##########
# Prepare BJS: Prisoners from 2010-2021 ############CHECK THIS
##########

# 2010 data
bjs_prison_pop_by_state_2010 <- bjs_prison_pop_by_gender_state_2010.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_3) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2010)

# 2011 data
bjs_prison_pop_by_state_2011 <- bjs_prison_pop_by_gender_state_2012.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_1) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2011)

# 2012 data
bjs_prison_pop_by_state_2012 <- bjs_prison_pop_by_gender_state_2012.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2012)

# 2013 data
bjs_prison_pop_by_state_2013 <- bjs_prison_pop_by_gender_state_2013.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2013)

# 2014 data
bjs_prison_pop_by_state_2014 <- bjs_prison_pop_by_gender_state_2014.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2014)

# 2015 data
bjs_prison_pop_by_state_2015 <- bjs_prison_pop_by_gender_state_2015.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_6) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2015)

# 2016 data
bjs_prison_pop_by_state_2016 <- bjs_prison_pop_by_gender_state_2016.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2016)

# 2017 data
bjs_prison_pop_by_state_2017 <- bjs_prison_pop_by_gender_state_2017.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2017)

# 2018 data
bjs_prison_pop_by_state_2018 <- bjs_prison_pop_by_gender_state_2018.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2018)

# 2019 data
bjs_prison_pop_by_state_2019 <- bjs_prison_pop_by_gender_state_2019.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_5) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2019)

# 2020 data
bjs_prison_pop_by_state_2020 <- bjs_prison_pop_by_gender_state_2020.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_4) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2020)

# 2021 data
bjs_prison_pop_by_state_2021 <- bjs_prison_pop_by_gender_state_2021.csv %>%
  clean_names() %>%
  select(state = x, bjs_prison_population = x_4) %>%
  fnc_clean_bjs_data() %>%
  mutate(rptyear = 2021)

# combine data
bjs_prison_pop_by_state <- rbind(
  bjs_prison_pop_by_state_2010,
  bjs_prison_pop_by_state_2011,
  bjs_prison_pop_by_state_2012,
  bjs_prison_pop_by_state_2013,
  bjs_prison_pop_by_state_2014,
  bjs_prison_pop_by_state_2015,
  bjs_prison_pop_by_state_2016,
  bjs_prison_pop_by_state_2017,
  bjs_prison_pop_by_state_2018,
  bjs_prison_pop_by_state_2019,
  bjs_prison_pop_by_state_2020,
  bjs_prison_pop_by_state_2021
)

##########
# Prepare hex data for map
##########

# Reformat hex data for map
hex_gj <- hex %>%
  st_transform(3857) %>%
  sf_geojson() %>%
  fromJSON(simplifyVector = FALSE)








##########
# Prepare parole info by state for map
##########

parole_info_by_state <- parole_info_by_state  %>%
  clean_names()








##########
# Prepare annual parole survey
##########

# Get state abb
state_names_abb <- data.frame(abbreviation = state.abb,
                              name = state.name,
                              stringsAsFactors = FALSE) %>%
  rename(state = name, stateid = abbreviation)

# List of data frames and years
aps_data_list <- list(da38058.0001,
                      da37471.0001,
                      da37441.0001,
                      da36619.0001,
                      da36320.0001,
                      da35629.0001,
                      da35257.0001,
                      da34718.0001,
                      da34382.0001,
                      da34381.0001,
                      da34380.0001,
                      da31332.0001,
                      da31331.0001,
                      da31330.0001,
                      da31329.0001,
                      da31328.0001,
                      da31327.0001,
                      da31326.0001,
                      da31325.0001)
aps_years <- 2018:2000
aps_pre_2008 <- rep(FALSE, 7) %>% c(rep(TRUE, 12))

# Process and combine APS data
aps_parole_combined <- lapply(seq_along(aps_data_list), function(i) {
  fnc_prepare_aps_data(aps_data_list[[i]], aps_years[i], aps_pre_2008[i])
})
aps_parole_2000_2018 <- do.call(rbind, aps_parole_combined)

# Remove DC
aps_parole_2000_2018 <- aps_parole_2000_2018 %>%
  filter(!state %in% c("District of Columbia", "Federal") & !is.na(state))







##########
# Save data
##########

# Save files to app folder
theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis/app"))

for (folder in theseFOLDERS){

  save(ncrp_yearendpop,                    file = file.path(folder, "ncrp_yearendpop.rds"))
  save(ncrp_admissions,                    file = file.path(folder, "ncrp_admissions.rds"))
  save(ncrp_term_records,                  file = file.path(folder, "ncrp_term_records.rds"))
  save(ncrp_releases,                      file = file.path(folder, "ncrp_releases.rds"))
  save(aps_parole_2000_2018,               file = file.path(folder, "aps_parole_2000_2018.rds"))
  save(bjs_prison_pop_by_race,             file = file.path(folder, "bjs_prison_pop_by_race.rds"))
  save(bjs_prison_pop_by_state,            file = file.path(folder, "bjs_prison_pop_by_state.rds"))

  save(hex_gj,                             file = file.path(folder, "hex_gj.rds"))
  save(robinadefinitions,                  file = file.path(folder, "robinadefinitions.rds"))
  save(robinainfo,                         file = file.path(folder, "robinainfo.rds"))
  save(robinaparoleeligibility,            file = file.path(folder, "robinaparoleeligibility.rds"))
  save(parole_info_by_state,               file = file.path(folder, "parole_info_by_state.rds"))

}
