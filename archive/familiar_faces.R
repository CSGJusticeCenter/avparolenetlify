
########################################

# Familiar faces of the prisons

########################################

# count number of entrances per person
# flag as a familiar face if in the 90th percentile (top 10%)
# first admtyear is 1950
ncrp_familiar_faces <- ncrp_term_records %>%
  # filter(admityr >= 1980) %>%
  group_by(abt_inmate_id, state) %>%
  summarise(num_entrances = n())
ncrp_familiar_faces <- ncrp_familiar_faces %>%
  mutate(high_utilizer_10_pct  = quantile(ncrp_familiar_faces$num_entrances, probs = 0.90) < num_entrances)

# merge info on familiar faces with ncrp_term_records
ncrp_term_records_clean <- ncrp_term_records %>%
  left_join(ncrp_familiar_faces, by = c("abt_inmate_id", "state"))


