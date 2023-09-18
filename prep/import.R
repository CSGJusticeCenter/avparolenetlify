#######################################
# Project: AV Parole
# File: import.R
# Authors: Mari Roberts
# Date last updated: March 13, 2023 (MAR)
# Description:
#    Import NCRP data (admissions, population, year end population)
#######################################

# load prison sentencing system info from Robina
robinainfo <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"), sheet = "classifications")
robinadefinitions <- read.xlsx(paste0(sp_data_path, "/data/raw/robinainfo.xlsx"), sheet = "definitions")
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
prison_pop_by_race_state_2020 <- read.csv(paste0(sp_data_path, "/data/raw/p20st/p20stat02.csv"), skip = 10)

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
  read.xlsx(paste0(sp_data_path, "/background/Parole Info by State.xlsx"),
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
    sentlgth     = str_sub(sentlgth, 5, -1)) %>%

  mutate(across(everything(), ~ trimws(.)))



#############
# {repare NCRP Admissions
#############

ncrp_admissions <- da38492.0002 %>% clean_names() %>%

  mutate(
    state        = str_sub(state, 6, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    race         = str_sub(race, 5, -1),
    sex          = str_sub(sex, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1)) %>%

  mutate(offdetail = trimws(offdetail)) %>%

  # create parole eligibility status with custom function
  fnc_create_parelig_status()



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
    timesrvd_rel = str_sub(timesrvd_rel, 5, -1)) %>%

  mutate(offdetail = trimws(offdetail)) %>%

  # custom funciton that create sentence length and timeserved order since they are categorical
  # calculate proportion of sentence length served
  # determine timing of release
  fnc_sentlgth_timesrvd_rel()





#############
# Prepare NCRP Year End Population
#############

ncrp_yearendpop <- da38492.0004 %>% clean_names() %>%
  mutate(
    state          = str_sub(state, 6, -1),
    offgeneral     = str_sub(offgeneral, 5, -1),
    offdetail      = str_sub(offdetail, 5, -1),
    race           = str_sub(race, 5, -1),
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
           TRUE ~ offgeneral
         )) %>%

  # create new offense descriptions
  fnc_create_fbi_index() %>%

  # create parole eligibility status with custom function
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
         # ageyrend = factor(ageyrend,
         #                   levels = c("55+ years",
         #                              "45-54 years",
         #                              "35-44 years",
         #                              "25-34 years",
         #                              "18-24 years")),
         ageyrend = factor(ageyrend,
                           levels = c("18-24 years",
                                      "25-34 years",
                                      "35-44 years",
                                      "45-54 years",
                                      "55+ years"
                                      )),
         sentlgth = factor(sentlgth,
                           levels = c(
                             "< 1 year",
                             "1-1.9 years",
                             "2-4.9 years",
                             "5-9.9 years",
                             "10-24.9 years",
                             ">=25 years",
                             "Life, LWOP, Life plus additional years, Death",
                             "Unknown"))
         )








##########
# Prepare Prisoners in 2020 data for analysis
##########

# clean up file to create dataframe of state prison pop by race
prison_pop_by_race_state_2020 <- prison_pop_by_race_state_2020 %>%
  clean_names() %>%
  filter(jurisdiction == "") %>%
  select(-c(jurisdiction)) %>%
  rename(state = x) %>%
  mutate_all(~str_replace_all(.,",",""))








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
# Save data
##########

# theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))
#
# for (folder in theseFOLDERS){
#
#   save(ncrp_yearendpop,   file=file.path(folder, "ncrp_yearendpop.rds"))
#   save(ncrp_admissions,   file=file.path(folder, "ncrp_admissions.rds"))
#   save(ncrp_term_records, file=file.path(folder, "ncrp_term_records.rds"))
#   save(ncrp_releases,     file=file.path(folder, "ncrp_releases.rds"))
#
#   save(hex_gj, file=file.path(folder, "hex_gj.rds"))
#   save(robinadefinitions, file=file.path(folder, "robinadefinitions.rds"))
#   save(robinainfo, file=file.path(folder, "robinainfo.rds"))
#   save(robinaparoleeligibility, file=file.path(folder, "robinaparoleeligibility.rds"))
#   save(parole_info_by_state, file=file.path(folder, "parole_info_by_state.rds"))
#
# }

