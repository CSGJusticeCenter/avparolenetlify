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

# load NCRP data
# https://www.icpsr.umich.edu/web/NACJD/studies/38492
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0001/38492-0001-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0002/38492-0002-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0003/38492-0003-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0004/38492-0004-Data.rda"))

# load Prisoners in 2020 - Statistical Tables
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

# Load sp file
hex <- read_sf(paste0(sp_data_path, "/data/raw/us_states_hexgrid.geojson")) %>%
  select(state_abb = iso3166_2) %>%
  filter(state_abb != "DC")





##########
# Prepare NCRP data for analysis
##########

# rename df names and clean variable names
ncrp_term_records <- da38492.0001 %>% clean_names() %>%
  mutate(
    state        = str_sub(state, 6, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    race         = str_sub(race, 5, -1),
    sex          = str_sub(sex, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1))

ncrp_admissions <- da38492.0002 %>% clean_names() %>%
  # create parole eligibility status with custom function
  fnc_create_parelig_status() %>%
  mutate(
    state        = str_sub(state, 6, -1),
    offgeneral   = str_sub(offgeneral, 5, -1),
    offdetail    = str_sub(offdetail, 5, -1),
    admtype      = str_sub(admtype, 5, -1),
    race         = str_sub(race, 5, -1),
    sex          = str_sub(sex, 5, -1),
    sentlgth     = str_sub(sentlgth, 5, -1))

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
    timesrvd_rel = str_sub(timesrvd_rel, 5, -1))

ncrp_yearendpop <- da38492.0004 %>% clean_names() %>%
  mutate(
    state = str_sub(state, 6, -1),
    offgeneral = str_sub(offgeneral, 5, -1),
    admtype = str_sub(admtype, 5, -1),
    race = str_sub(race, 5, -1)) %>%
  # create parole eligibility status with custom function
  fnc_create_parelig_status()





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
# Save data
##########

theseFOLDERS <- c("sharepoint" = paste0(sp_data_path, "/data/analysis"))

for (folder in theseFOLDERS){

  save(hex_gj, file=file.path(folder, "hex_gj.rds"))
  save(robinadefinitions, file=file.path(folder, "robinadefinitions.rds"))
  save(robinainfo, file=file.path(folder, "robinainfo.rds"))

}

