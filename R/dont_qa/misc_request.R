# Calculate the average length of stay by state and by offense type
# 10 year change
ncrp_los_by_offense_type_10yr_change <- ncrp_releases %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, fbi_index, rptyear) %>%
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE)) %>%
  pivot_longer(cols = Average, names_to = "type", values_to = "value") %>%
  group_by(state) %>%
  mutate(max_rptyear = max(rptyear),
         min_rptyear = max_rptyear - 10) %>%
  filter(rptyear == min_rptyear | rptyear == max_rptyear) %>%
  group_by(state, fbi_index) %>%
  mutate(change_10_years = (last(value) - first(value)) / first(value) * 100)

write.csv(ncrp_los_by_offense_type_10yr_change, "ncrp_los_by_ncc_offense_type_10yr_change_05_31_24.csv")

# Calculate the average length of stay by state and by offense type
# All report years
ncrp_los_by_offense_type_by_year <- ncrp_releases %>%
  filter(admtype == "New court commitment") %>%
  group_by(state, fbi_index, rptyear) %>%
  summarise(
    Average = mean(time_between_admisson_release, na.rm = TRUE)) %>%
  pivot_longer(cols = Average, names_to = "type", values_to = "value")

write.csv(ncrp_los_by_offense_type_by_year, "ncrp_los_by_ncc_offense_type_by_year_05_31_24.csv")

# Get list of states with data by rpt year
# Create a pivot table
ncrp_available_data <- ncrp_releases %>%
  group_by(state, rptyear) %>%
  summarise(
    has_new_court_commitment = any(admtype == "New court commitment"),
    has_fbi_index = any(!is.na(fbi_index)),
    .groups = 'drop'
  ) %>%
  mutate(
    submission_status = case_when(
      has_new_court_commitment & has_fbi_index ~ "Submitted",
      !has_new_court_commitment | !has_fbi_index ~ "Incomplete Submission"
    )
  ) %>%
  select(-has_new_court_commitment, -has_fbi_index) %>%
  complete(state, rptyear, fill = list(submission_status = "NA")) %>%
  pivot_wider(names_from = rptyear, values_from = submission_status)

write.csv(ncrp_available_data, "ncrp_avilable_data_05_31_2024.csv")
