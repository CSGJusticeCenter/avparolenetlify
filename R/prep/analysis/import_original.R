# load NCRP data admissions, releases, term records, and year end population
# term records = admissions from 1950 to 2020
# admissions = from 1991 to 2020
# releases = from 1991 to 2020
# year end population = from 1999 to 2020
# source = https://www.icpsr.umich.edu/web/NACJD/studies/38492
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0001/38492-0001-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0002/38492-0002-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0003/38492-0003-Data.rda"))
load(paste0(sp_data_path, "/data/raw/ICPSR_38492-V1/ICPSR_38492/DS0004/38492-0004-Data.rda"))

# Remove unwanted characters from strings
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

  # determine parole eligibility status
  fnc_create_parelig_status() %>%

  # create new offense descriptions
  fnc_create_fbi_index() %>%

  # calculate timing of release by parole eligibility date (year)
  # released before, on, or after parole eligibility year
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

  # change NAs to "Unknown"
  fnc_create_admtype() %>%
  mutate(race     = ifelse(is.na(race), "Unknown", race),
         agerlse  = ifelse(is.na(agerlse), "Unknown", agerlse),
         sentlgth = ifelse(is.na(sentlgth), "Unknown", sentlgth)) %>%

  # factor variables
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

# Remove unwanted characters from strings
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

  # combine other and unknown
  mutate(offdetail = trimws(offdetail),
         offgeneral = case_when(
           is.na(offgeneral) ~ "Other or Unknown",
           offgeneral == "Other/unspecified" ~ "Other or Unknown",
           TRUE ~ offgeneral)) %>%

  # create new offense descriptions
  fnc_create_fbi_index() %>%

  # determine parole eligibility status
  fnc_create_parelig_status() %>%

  # change NAs to "Unknown"
  fnc_create_admtype() %>%
  mutate(race = ifelse(is.na(race), "Unknown", race),
         ageyrend = ifelse(is.na(ageyrend), "Unknown", ageyrend),
         sentlgth = ifelse(is.na(sentlgth), "Unknown", sentlgth)) %>%

  # factor variables
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


