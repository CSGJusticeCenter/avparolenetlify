
##########
# Annual Parole Survey Series in 2018 data for analysis
##########

state_names_abb <-
  data.frame(abbreviation = state.abb,
             name = state.name,
             stringsAsFactors = FALSE)

state_names_abb <- state_names_abb %>%
  rename(state = name,
         stateid = abbreviation)

aps_parole_2018 <- da38058.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2018) %>%
  fnc_aps_prepare()

aps_parole_2017 <- da37471.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2017) %>%
  fnc_aps_prepare()

aps_parole_2016 <- da37441.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2016) %>%
  fnc_aps_prepare()

aps_parole_2015 <- da36619.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2015) %>%
  fnc_aps_prepare()

aps_parole_2014 <- da36320.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2014) %>%
  fnc_aps_prepare()

aps_parole_2013 <- da35629.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 6, -1),
         rptyear = 2013) %>%
  fnc_aps_prepare()

aps_parole_2012 <- da35257.0001 %>%
  clean_names() %>%
  mutate(state = str_sub(stateid, 5, -1),
         rptyear = 2012) %>%
  mutate(state = str_trim(state)) %>%
  fnc_aps_prepare()

aps_parole_2011 <- da34718.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2011) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2010 <- da34382.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2010) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2009 <- da34381.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2009) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2008 <- da34380.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2008) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2007 <- da31332.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2007) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2006 <- da31331.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2006) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2005 <- da31330.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2005) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2004 <- da31329.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2004) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2003 <- da31328.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2003) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2002 <- da31327.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2002) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2001 <- da31326.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2001) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2000 <- da31325.0001 %>%
  clean_names() %>%
  mutate(rptyear = 2000) %>%
  select(-stateid) %>%
  rename(stateid = state) %>%
  mutate(stateid = str_trim(stateid)) %>%
  left_join(state_names_abb, by = "stateid") %>%
  fnc_aps_prepare_pre2008()

aps_parole_2000_2018 <- rbind(aps_parole_2018,
                              aps_parole_2017,
                              aps_parole_2016,
                              aps_parole_2015,
                              aps_parole_2014,
                              aps_parole_2013,
                              aps_parole_2012,
                              aps_parole_2011,
                              aps_parole_2010,
                              aps_parole_2009,
                              aps_parole_2008,
                              aps_parole_2007,
                              aps_parole_2006,
                              aps_parole_2005,
                              aps_parole_2004,
                              aps_parole_2003,
                              aps_parole_2002,
                              aps_parole_2001,
                              aps_parole_2000)

aps_parole_2000_2018 <- aps_parole_2000_2018 %>%
  filter(state != "District of Columbia" &
           state != "Federal" &
           !is.na(state))
