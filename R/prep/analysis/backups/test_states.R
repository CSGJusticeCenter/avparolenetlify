


iowa <- ncrp_yearendpop |> filter(state == "Iowa" & admtype == "New court commitment") |>
  group_by(parelig_status) |> summarise(total = n())

iowa <- ncrp_yearendpop |> filter(state == "Iowa") |>
  filter(rptyear == 2020) |> group_by(admtype, fbi_index, parelig_status) |> summarise(total = n())




oklahoma <- ncrp_releases |> filter(rptyear == 2020 & state == "Oklahoma") |>
  select(state, rptyear,
         admityr,
         relyr,
         race, time_between_admisson_release, fbi_index, offdetail)


oklahoma_summary <- oklahoma |>
  group_by(race) |>
  summarize(
    mean_stay = mean(time_between_admisson_release, na.rm = TRUE),
    median_stay = median(time_between_admisson_release, na.rm = TRUE),
    sd_stay = sd(time_between_admisson_release, na.rm = TRUE),
    min_stay = min(time_between_admisson_release, na.rm = TRUE),
    max_stay = max(time_between_admisson_release, na.rm = TRUE),
    count = n()
  )


# Assuming 'fbi_index' is the column representing the type of offense.
oklahoma_offense_summary <- ncrp_releases |>
  filter(rptyear == 2020 & state == "Oklahoma") |>
  select(state, rptyear, race, fbi_index, time_between_admisson_release) |>
  group_by(race, fbi_index) |>
  summarize(
    mean_stay = mean(time_between_admisson_release, na.rm = TRUE),
    median_stay = median(time_between_admisson_release, na.rm = TRUE),
    sd_stay = sd(time_between_admisson_release, na.rm = TRUE),
    count = n()
  ) |>
  arrange(race, fbi_index)

# Print the summary by offense type
print(oklahoma_offense_summary)



# Assuming 'parelig_status' is the column representing parole eligibility.
oklahoma_parole_summary <- ncrp_releases |>
  filter(rptyear == 2020 & state == "Oklahoma") |>
  select(state, rptyear, race, parelig_status, time_between_admisson_release) |>
  group_by(race, parelig_status) |>
  summarize(
    mean_stay = mean(time_between_admisson_release, na.rm = TRUE),
    median_stay = median(time_between_admisson_release, na.rm = TRUE),
    sd_stay = sd(time_between_admisson_release, na.rm = TRUE),
    count = n()
  ) |>
  arrange(race, parelig_status)

# Print the parole summary
print(oklahoma_parole_summary)


iowa <- ncrp_yearendpop |> filter(state == "Iowa") |> filter(rptyear == 2020) |> group_by(parelig_status) |> summarise(total = n())
